###################
# This script serve to create from .nc raster files (monthly suitability of feeding habitat for fin whale) 
# dowloaded from "https://ec.europa.eu/knowledge4policy/node/1246_de"
# square polygon Shapefiles with areas of greater probability of finding FW in the Med
# Output of this script is the polygon Shapefile of areas relevant for FW each month


setwd("C:\\DOM")



##########################################################
# 1. Load Libraries and custom functions
##########################################################

library(ncdf4)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

#-----------------------------------------------------------------------
# spatial_bboxes        Creates bounding box for hotspot polygons
#-----------------------------------------------------------------------
spatial_bboxes <- function(polygons) {
  individual_bb <- function(polygon, projection) {
    polygon <- sp::SpatialPolygons(list(polygon), proj4string = projection)
    spatial_bbox <- as(raster::extent(polygon), "SpatialPolygons")
    spatial_bbox <- spatial_bbox@polygons[[1]]
    spatial_bbox@ID <- polygon@polygons[[1]]@ID
    return(spatial_bbox)
  }
  polys <- lapply(polygons@polygons, individual_bb, polygons@proj4string)
  spatial_polys <- sp::SpatialPolygons(polys, proj4string = polygons@proj4string)
  spatial_polys_df <- sp::SpatialPolygonsDataFrame(spatial_polys, polygons@data)
  return(spatial_polys_df)
}
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# get_hotspot        Calculate hotspot from raster stack using quantile criteria
#-----------------------------------------------------------------------
get_hotspot <- function(s, prob = 0.95, rm.zero=TRUE){
  # s: raster stack
  # probs: quantile prob
  
  library(raster)
  
  # create a list object to store all values
  vals_list <- list()
  
  # extract values from each raster separatelly
  for (i in 1:nlayers(s)){
    
    # extract values from raster
    vals <- subset(s, i)[]
    
    # remove NAs
    vals <- vals[!is.na(vals)]
    
    # remove zeros
    if(rm.zero == TRUE) vals <- vals[vals != 0]
    
    # add into list
    vals_list[[i]] <- vals
  }
  
  # combine all values
  all_vals <- unlist(vals_list)
  
  # calculate hotspot using a 95% quantile criteria
  hotspot <- quantile(all_vals, probs = prob, na.rm = TRUE)
  return(hotspot)
}
#-----------------------------------------------------------------------


##########################################################
# 2. Set thresholds
##########################################################

## Parameters that have a major effect
hotspot_prob <- 0.99  # quantile probability to define the hotspots (higher number result in less polygons) 
min_pol_size <- 1000  # minimum size to define a spatial coherent area, in km2  (higher number result in less polygons) 


##########################################################
# 3. Import data per species
##########################################################

#--------------------------
# Common data
#--------------------------

# upload the layer that serve as clipping boundaries of the study area
westmed <- readOGR(dsn="C:\\DOM\\input",layer="WestMed_area")
#westmed <- readOGR(dsn="data/ext/eea",layer="western_mediterranean")


#------------------------------------------------------
# Fin whale (only run when processing this species)
#------------------------------------------------------

## Set parameters
in_folder <- "C:/DOM/input/fw.nc/"
out_folder <- "C:/DOM/output/"
infile_prefix <- "EMIS_Fin_whale_"
outfile_prefix <- "fw"
year <- c(2003:2014)
month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")  # for fin whale

## Import raster files (fin whale)
ncfiles <- list.files(in_folder, pattern=".nc", full.names = TRUE)  # check that the folder only contains the files that we need
s <- stack(ncfiles)  # create stack
s <- crop(s, westmed)  # crop model for the WMed



#------------------------------------------------------
# Blue fin tuna (only run when processing this species)
#------------------------------------------------------

## Blue fin
in_folder <- "C:/DOM/input/bft.nc/"
out_folder <- "C:/DOM/output/"
infile_prefix <- "EMIS_BFT500_SH_"
outfile_prefix <- "bf_"
year <- c(2003:2014)
month <- c("05", "06", "07") # blue fin

## Import raster files (bluefin)
## In this case, we only select months from may to july to calculate the hotspot threshold
ncfiles_05 <- list.files(in_folder, pattern="EMIS_BFT500_SH_05", full.names = TRUE)  # check that the folder only contains the files that we need
ncfiles_06 <- list.files(in_folder, pattern="EMIS_BFT500_SH_06", full.names = TRUE)  # check that the folder only contains the files that we need
ncfiles_07 <- list.files(in_folder, pattern="EMIS_BFT500_SH_07", full.names = TRUE)  # check that the folder only contains the files that we need
ncfiles <- c(ncfiles_05, ncfiles_06, ncfiles_07)
s <- stack(ncfiles)  # create stack
s <- crop(s, westmed)  # crop model for the WMed



##########################################################
# 4. Generate dynamic marine zones
##########################################################

## Calculate hotspot value
hotspot <- get_hotspot(s, prob = hotspot_prob, rm.zero=FALSE)


## Loop for each nc file
for (y in 1:length(year)){
  for (m in 1:length(month)){
    
    print(paste("Processing year", year[y], "month", month[m]))
    
    ## Import dynamic zones previously created
    rast <- paste0(in_folder, infile_prefix, month[m], "_", year[y],".nc")
    if (!file.exists(rast)) next
    finwhale <- raster(rast)
    
    # crop model for the WMed
    finwhale <- crop(finwhale, westmed)
    
    # # smooth using a moving window of 5 x 5
    # finwhale <- focal(finwhale, w=matrix(1, 5, 5), mean, na.rm=TRUE)
    # 
    # # set cells with prob = 0 to NA
    # finwhale[finwhale==0] <- NA
    # 
    # ## calculate hotspot (95th percentile)
    # min_threshold = 40
    # q95 <- quantile(finwhale, probs = 0.95)
    # if (is.na(q95)) next
    # if (q95 < min_threshold) {
    #   hotspot <- 100 # or next?
    # } else {
    #   hotspot <- q95
    # }
    
    ###reclassify (scale from 1 to 100 as from original data)
    v.rcl <- c(0, hotspot, 0, hotspot, 100, 1)
    m.rcl <- matrix(v.rcl, ncol=3, byrow=TRUE)
    finwhale.rec <- reclassify(finwhale, m.rcl)
    finwhale.rec[finwhale.rec==0]<-NA
    # plot(finwhale.rec, axes=T, col='red'); plot(westmed, add=T)
    
    if (is.null(finwhale.rec)) next
    
    ## convert raster into polygon
    poly.finwhale <- rasterToPolygons(finwhale.rec, na.rm=TRUE, digits=12, dissolve=TRUE)
    if (is.null(poly.finwhale)) next
    
    finwhale.laea <- spTransform(poly.finwhale,CRS("+init=EPSG:3035"))
    ## make the clip for WestMed, this way works fine
    westmed.laea <- spTransform(westmed,CRS("+init=EPSG:3035"))
    clip <- gIntersection(finwhale.laea, westmed.laea)
    if (is.null(clip)) next
    
    # split multipolygon in separate polygons
    split.poly <- disaggregate(clip)
    
    # 3) calculate area for each polygon
    finwhale.laea <- spTransform(split.poly,CRS("+init=EPSG:3035"))
    finwhale.laea$area <- area(finwhale.laea)/1e6  # return m2, convert to km2
    
    # 4) select polygon with area higher than our specified threshold
    ## threshold 1000 km2
    finwhale_large <- subset(finwhale.laea, area > min_pol_size)
    if (length(finwhale_large) == 0) next
    
    ##############
    
    finwhale_bboxes <- spatial_bboxes(finwhale_large)
    
    ##############
    
    # dissolve boxes to combine overlapped areas
    finwhale.uni <- gUnaryUnion(finwhale_bboxes)
    
    # dissagregate again to calculate areas
    finwhale_bboxes <- disaggregate(finwhale.uni)
    #finwhale_bboxes <- st_cast(finwhale.uni,"POLYGON")
    
    #if (!file.exists(finwhale.laea)) next
    finwhale_bboxes$area <- area(finwhale_bboxes)
    
    # dissolve the polygons in one multypoligon feature
    #finwhale.uni <- gUnaryUnion(finwhale_bboxes)
    
   
    # clip$area <- area(clip)
    clip_prj <- spTransform(finwhale_bboxes, CRS("+init=EPSG:4326"))
    name <- paste0(out_folder, outfile_prefix, "_", year[y], "_", month[m], ".shp")
    writePolyShape(clip_prj, name)
  }
}
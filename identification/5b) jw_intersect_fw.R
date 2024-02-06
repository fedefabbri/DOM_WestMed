###################
# This script serve to analyze the intersection between fw areas and jurisdictional waters
# Output of this script is the statistic on percentage of jw type intersected every month-year by the fw areas
# (just jw type, independently from nationality)

setwd("C:\\DOM")

library(shapefiles)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(rgeos) #### intersect() function, careful because it has conflicts with dplyr

crswgs4326=CRS("+init=EPSG:4326")

# Import the jurisdictional waters shapefiles


#### load shapefile IW:
# (this layer was modified in ArcGIS from the original: clip on the western mediterranean area) 

IW <- readShapePoly("input\\boundaries\\IW_clip.shp",proj4string=crswgs4326,verbose=TRUE)
### Create an Object_ID column as the one from arcGIS (not starting with "0" but with "1")..... is it really necessary?
n <- nrow(IW@data)
IW@data$FID <- c(0:(n-1))  ## necessary?
iw.laea <- spTransform(IW,CRS("+init=EPSG:3035"))
iw.laea <- gBuffer(iw.laea, byid=TRUE, width=0)
## dissolve the IW 
iw.diss <- gUnaryUnion(iw.laea)
iw.diss$Pol_type <- "IW"


# (this layer was modified in ArcGIS from the original: clip on the western mediterranean area)
TS <- readShapePoly("input\\boundaries\\TS_clip.shp",proj4string=crswgs4326,verbose=TRUE)
### I want an Object_ID column as the one from arcGIS (not starting with "0" but with "1")..... is it really necessary?
n <- nrow(TS@data)
TS@data$FID <- c(0:(n-1))  ## necessary?
ts.laea <- spTransform(TS,CRS("+init=EPSG:3035"))
#ts.laea <- gBuffer(ts.laea, byid=TRUE, width=0)
## dissolve the TS 
ts.diss <- gUnaryUnion(ts.laea)
ts.diss$Pol_type <- "TS"

# (this layer was modified in ArcGIS from the original: clip on the western mediterranean area + erase of TS&IW areas; otherwise it would overlap IW & TS)
EEZ <- readShapePoly("input\\boundaries\\eez_erase_ts_iw.shp",proj4string=crswgs4326,verbose=TRUE)
n <- nrow(EEZ@data)
EEZ@data$FID <- c(0:(n-1))
eez.laea <- spTransform(EEZ,CRS("+init=EPSG:3035"))
#eez.laea <- gBuffer(eez.laea, byid=TRUE, width=0)
## dissolve EEZ
eez.diss <- gUnaryUnion(eez.laea)
eez.diss$Pol_type <- "EEZ"

# Define years and month to analyse data
year <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)
month <- c("01","02","03","04","05", "06", "07","08","09","10","11","12")

## note: it doesn't work for july 2006 it has to be done manually

# Create empty dataframe to save the results
polyDF <- NULL


for (y in 1:length(year)){
  for (m in 1:length(month)){
    
    ## Import dynamic zones previously created
    shpfile <- paste0("C:\\DOM\\output\\fw_", year[y], "_",month[m] ,".shp")
    if (!file.exists(shpfile)) next
    dynamic_zones <- readShapePoly(shpfile, proj4string=crswgs4326,verbose=TRUE) 
    zones.laea <- spTransform(dynamic_zones,CRS("+init=EPSG:3035"))
    
    ## intersect dynamic polys with jurisdictional water layer
    # pi.iw <- intersect(iw.diss, zones.laea)
    # pi.iw$area <- raster::area(pi.iw)
    # zones.laea$iw_int <- pi.iw@data$area/zones.laea@data$area
    # zones.laea$iw_area <- pi.iw@data$area
    
    pi.ts <- raster::intersect(ts.diss, zones.laea)
    if (is.null(pi.ts)){
      zones.laea$ts_area <- 0
    } else {
      zones.laea$ts_area <- raster::area(pi.ts)
    }
    
    zones.laea$ts_int <- zones.laea$ts_area/zones.laea@data$area
    
    
#####
    
    pi.eez <- raster::intersect(eez.diss, zones.laea)
    if (is.null(pi.eez)){
      zones.laea$EEZ_area <- 0
    } else {
      zones.laea$EEZ_area <- raster::area(pi.eez)
    }
    
    zones.laea$EEZ_int <- zones.laea$EEZ_area/zones.laea@data$area
    
    
    
    ## obtain iw intersection area and percentage from subtraction
    zones.laea$iw_area <- zones.laea@data$area - zones.laea$EEZ_area - zones.laea$ts_area
    zones.laea$iw_area <- ifelse(zones.laea$iw_area >= 0, zones.laea$iw_area, 0)
    zones.laea$iw_int <- zones.laea@data$iw_area/zones.laea@data$area
    
    df <- data.frame(year = year[y], month = month[m]) #, iw_area = zones.laea$iw_area,  iw_int = zones.laea$iw_int, ts_area)
    zones.laea@data$year <- df$year
    zones.laea@data$month <- df$month
    polyDF <- rbind(polyDF, zones.laea@data)
  }
}

polyDF <- polyDF[, c(9,10,2,8,7,4,3,5,6)]

## export (this is by month)
write.csv(polyDF, file = "output/jw_intersect_fw.csv")

##################### relative areas of jw by year          
cntrAREA <- polyDF %>%
  group_by(year) %>%
  summarise(area = sum(area), iw_area = sum(iw_area), ts_area = sum(ts_area), EEZ_area = sum(EEZ_area)) %>%
  mutate(freq_iw = iw_area / area, freq_ts = ts_area / area, freq_eez = EEZ_area / area)

###########                 

write.csv(cntrAREA, file = "output/jw_intersect_YEAR_fw.csv")

### this would be to create an average value per year

DF <- polyDF %>% 
  group_by(year) %>% 
  summarise(year_eez = mean(EEZ_area), year_ts = mean(ts_area), year_iw = mean(iw_area))


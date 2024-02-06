########################################################################
####### area and % of area intersected of bft bounding boxes by each country marine jurisdiction ###
####### Output of this script are the statistics on countries relevant every month-year to bft areas ###

setwd("C:\\DOM")

library(shapefiles)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(rgeos) #### intersect() function, careful because it has conflicts with dplyr

# Import the jurisdictional waters limits
crswgs4326=CRS("+init=EPSG:4326")
JW <- readShapePoly("input\\boundaries\\eez_clip.shp",proj4string=crswgs4326,verbose=TRUE)
jw.laea <- spTransform(JW,CRS("+init=EPSG:3035"))
jw.laea <- gBuffer(jw.laea, byid=TRUE, width=0)


# Define years and month to analyse data
year <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)
month <- c("05", "06", "07")

# Create empty dataframe to save the results
polyDF <- NULL


for (y in 1:length(year)){
  for (m in 1:length(month)){
    
    ## Import dynamic zones previously created
    shpfile <- paste0("output\\bf__", year[y], "_", month[m],".shp")
    if (!file.exists(shpfile)) next
    dynamic_zones <- readShapePoly(shpfile, proj4string=crswgs4326,verbose=TRUE) 
    zones.laea <- spTransform(dynamic_zones,CRS("+init=EPSG:3035"))
    
    
    ## intersect dynamic polys with jurisdictional water layer
    
    
    pi <- raster::intersect(jw.laea, zones.laea)
    
    # calculate areas
    pi$area <- raster::area(pi)
    
    
    # calculate percentage
    pi$percent <- pi$area/sum(pi$area)
    
    
    
    df <- data.frame(year = year[y], month = month[m], GeoName = pi$GeoName, Country = pi$Sovereign1, area = pi$area, intersection = pi$percent)
    polyDF <- rbind(polyDF, df)
  }
}

# calculate n° of months each cntr is intersected
country_intersect <- polyDF %>% 
  group_by(GeoName) %>%
  summarize(n())

# calculate number of countries occurrences by month each year
data <- polyDF %>%
  group_by(year, month)  %>%
  tally()

# calculate number of countries occurrences by month
data_m <- polyDF %>%
  group_by(month)  %>%
  tally()

# calculate number of countries occurrences by year
data_year <- polyDF %>%
  group_by(year)  %>%
  distinct(GeoName) %>%
  tally()


## export
write.csv(polyDF, file = "output/bft_countries_intersection.csv")
write.csv(data, file = "output/bft_countries_occurence.csv")
write.csv(data_m, file = "output/bft_countries_occurence_month.csv")
write.csv(data_year, file = "output/bft_countries_occurence_year.csv")
write.csv(country_intersect, file = "output/bft_N_selections_by_cntr.csv")

### to get monthly mean for year

sum <- polyDF %>% 
  group_by(year, GeoName) %>%
  summarize(year_intersect = mean(area)) %>%
  mutate(freq = year_intersect / sum(year_intersect))


##leave only 3 decimals

sum[,'freq']=round(sum[,'freq'],4)

# %100

sum$percentage <- sum$freq*100

### to get total area intersected for each country by year ### and percentage

cntrAREA <- polyDF %>%
  group_by(year, GeoName) %>%
  summarise(area = sum(area)) %>%
  mutate(freq = area / sum(area))

##leave only 2 decimals

cntrAREA[,'freq']=round(cntrAREA[,'freq'],4)

# %100

cntrAREA$percentage <- cntrAREA$freq*100


###export by year

write.csv(sum, file = "output/bft_countries_intersection_year.csv")

write.csv(cntrAREA, file = "output/bft_countries_intersection_yearAREA.csv")

## calculate area of jw for each cntr

jw.laea$area <- raster::area(jw.laea)

View(jw.laea@data) ## it is already in the original shapefile

write.csv(jw.laea@data, file = "output/bft_countries_JW_SA-surface.csv")


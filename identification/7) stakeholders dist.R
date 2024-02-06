###################
# This script serve to find out which Stakeholders (all stakeholders including administrations) are relevant to the fw areas based on a distance range
# Output of this script are the statistics on stakeholder organizations relevant every month-year to fw areas
# 
# 
setwd("C:\\DOM")


# Load libraries
library(shapefiles)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(rgeos) #### intersect() function, careful because it has conflicts with dplyr


# set parameters
crswgs4326=CRS("+init=EPSG:4326")

# import stakeholder organizations
#punctual_infos <- readShapePoints("output\\no_admins.shp",proj4string=crswgs4326,verbose=TRUE)
# with this database I have all the orgs (admins and no admins)
punctual_infos <- readShapePoints("output\\all_orgs.shp",proj4string=crswgs4326,verbose=TRUE)
# select only traffic organizations to analyze interaction with fin whale
traffic_orgs <- subset(punctual_infos, Sector %in% c('Marine traffic'))


# set CRS
infos.laea <- spTransform(traffic_orgs,CRS("+init=EPSG:3035"))

## to have lat lon of the orgs spatial points data frame
coords <- as.data.frame(infos.laea)
c <- coords %>%
  select(ID, coords.x1, coords.x2)


# Define years and month to analyse data
year <- c(1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
month <- c("01","02","03","04","05", "06", "07","08","09","10","11","12")

# Create empty dataframe to save the results
distDF <- NULL

for (y in 1:length(year)){
  for (m in 1:length(month)){
    
    print(paste("Processing year", year[y], "month", month[m]))
    
    ## Import monthly bft zones previously created
    shpfile <- paste0("output/fw_", year[y], "_", month[m],".shp")
    if (!file.exists(shpfile)) next
    dynamic_zones <- readShapePoly(shpfile, proj4string=crswgs4326,verbose=TRUE) 
    zones.laea <- spTransform(dynamic_zones,CRS("+init=EPSG:3035"))
    
    ## aggregate spatial polygons into a multypoligon
    zones.uni <- gUnaryUnion(zones.laea)
    
    # Calculate distance to zones
    Distance <- gDistance(zones.uni, infos.laea, byid = TRUE)
    Distance <- as.data.frame(Distance)
    colnames(Distance)<-"dist"
    Distance$ym <- paste0(year[y],month[m])
    Distance$month <- month[m]
    Distance$year<- year[y]
    Distance$ID<-infos.laea$ID
    Distance$org_name<-infos.laea$Org_nam
    Distance$org_type<-infos.laea$Org_typ
    Distance$org_sector<-infos.laea$Sector
    Distance$country<-infos.laea$Country
    
    
    # Save results to distDF
    distDF <- rbind(distDF, Distance)
  }
}

## add coords to the df
coordsDF <- merge(distDF, c, by = "ID", all.x=TRUE)

# calculate number of occurrences - max distance = 100 km
data <- coordsDF %>%
  group_by(ID)  %>%
  filter(dist < 100000) # %>%  # & year == 2003
# tally()

# reorder rows
my_data2 <- data[, c(3, 4, 5, 1, 6, 7:11)]



## export
write.csv(my_data2, file = "output/allorgs_traffic-fw_100km_multypoligon.csv")


# This script serve to find out which Stakeholders (all stakeholders including administrations) are relevant to the bft areas based on a distance range
# Output of this script are the statistics on stakeholder organizations relevant every month-year to bft areas
# 
# 
setwd("C:\\DOM")


# Load libraries
library(shapefiles)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(rgeos) #### intersect() function, careful because it has conflicts with dplyr


# set parameters
crswgs4326=CRS("+init=EPSG:4326")

# import stakeholder organizations
#punctual_infos <- readShapePoints("output\\no_admins.shp",proj4string=crswgs4326,verbose=TRUE)
# with this database I have all the orgs (admins and no admins)
punctual_infos <- readShapePoints("output\\all_orgs.shp",proj4string=crswgs4326,verbose=TRUE)

# select only fishing organizations to analyze interaction with bluefin tuna
fishing_orgs <- subset(punctual_infos, Sector %in% c('Commercial fishery','Recreational fishery'))


# set CRS
infos.laea <- spTransform(fishing_orgs,CRS("+init=EPSG:3035"))

## to have lat lon of the orgs spatial points data frame
coords <- as.data.frame(infos.laea)
c <- coords %>%
  select(ID, coords.x1, coords.x2)

# Define years and month to analyse data
year <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)
month <- c("05", "06", "07")

# Create empty dataframe to save the results
distDF <- NULL

for (y in 1:length(year)){
  for (m in 1:length(month)){
    
    print(paste("Processing year", year[y], "month", month[m]))
    
    ## Import monthly bft zones previously created
    shpfile <- paste0("output/bf__", year[y], "_", month[m],".shp")
    if (!file.exists(shpfile)) next
    dynamic_zones <- readShapePoly(shpfile, proj4string=crswgs4326,verbose=TRUE) 
    zones.laea <- spTransform(dynamic_zones,CRS("+init=EPSG:3035"))
    
    ## aggregate spatial polygons into a multypoligon
    zones.uni <- gUnaryUnion(zones.laea)
    
    # Calculate distance to zones
    Distance <- gDistance(zones.uni, infos.laea, byid = TRUE)
    Distance <- as.data.frame(Distance)
    colnames(Distance)<-"dist"
    Distance$ym <- paste0(year[y],month[m])
    Distance$month <- month[m]
    Distance$year<- year[y]
    Distance$ID<-infos.laea$ID
    Distance$org_name<-infos.laea$Org_nam
    Distance$org_type<-infos.laea$Org_typ
    Distance$org_sector<-infos.laea$Sector
    Distance$country<-infos.laea$Country
    
    # Save results to distDF
    distDF <- rbind(distDF, Distance)
  }
}

## add coords to the df
coordsDF <- merge(distDF, c, by = "ID", all.x=TRUE)

# calculate number of occurrences - max distance = 100 km
data <- coordsDF %>%
  group_by(ID)  %>%
  filter(dist < 100000) # %>%  # & year == 2003
# tally()

# reorder rows
my_data2 <- data[, c(3, 4, 5, 1, 6, 7:11)]


## export
write.csv(my_data2, file = "output/allorgs_fishery-bft_100km_multypoligon.csv")




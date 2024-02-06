###################
# This script serve to find out which administrations are responsible for the bft areas based on 
# their relation with jurisdictional waters
# Output of this script are the statistics on decision makers organizations relevant every month-year to bft areas
# 
# 


setwd("C:\\DOM")

library(shapefiles)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(rgeos) #### intersect() function, careful because it has conflicts with dplyr
library(raster)


# set parameters
crswgs4326=CRS("+init=EPSG:4326")


##### import previously created layer of jurisdictional waters and related administrations
JW <- readShapePoly("output/jw_admins_new.shp",proj4string=crswgs4326,verbose=TRUE)
jw.laea <- spTransform(JW,CRS("+init=EPSG:3035"))
jw.laea <- gBuffer(jw.laea, byid=TRUE, width=0)

# select only fishing administartions to analyze interaction with bluefin tuna
fishing.laea <- subset(jw.laea, Org_type == 'Administration')

### create an ID for each row (some problem with the original data)

fishing.laea@data$ID <- seq.int(nrow(fishing.laea@data))

## to have lat lon of the orgs spatial points data frame
#punctual_infos <- readShapePoints("Stakeholders\\admins.shp",proj4string=crswgs4326,verbose=TRUE)
#punctual <- subset(punctual_infos, Sector == 'Commercial fishery')
#coords <- as.data.frame(punctual)
#c <- coords[, c(5, 7, 8)]

#c$ID <- seq.int(nrow(c))
# replace conflicting characters
#repl <- read.csv("other data\\replacement.csv", header=TRUE, sep=";", colClasses = "character") #colClasses allow to keep character and not convert to factor
#for (i in 1:nrow(repl)){
#  c$Org_nam <- str_replace_all(c$Org_nam, pattern = repl[i,1], replacement = repl[i,2]) #overwrite
#}
#--------------------------------------------

# Define years and month to analyse data
year <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)
month <- c("05", "06", "07")

# Create empty dataframe to save the results
polyDF <- NULL


for (y in 1:length(year)){
  for (m in 1:length(month)){
    
    ## Import dynamic zones previously created
    shpfile <- paste0("output\\bf__", year[y], "_",month[m] ,".shp")
    if (!file.exists(shpfile)) next
    dynamic_zones <- readShapePoly(shpfile, proj4string=crswgs4326,verbose=TRUE) 
    zones.laea <- spTransform(dynamic_zones,CRS("+init=EPSG:3035"))
    
    ## intersect
    
    pi <- raster::intersect(fishing.laea, zones.laea)
    
    # calculate areas
    pi$area <- raster::area(pi)
    
    
    # calculate percentage
    pi$percent <- pi$area/sum(pi$area)
    
    pi@data$ym <- paste0(year[y],"_",month[m])
    pi@data$month <- month[m]
    pi@data$year <- year[y]
    
    
    # Save results to distDF
    polyDF <- rbind(polyDF, pi@data)
  }
}


my_data2 <- polyDF[, c(29,30,31,2,11,9,8,6,24,25)]
## add coords to the df
#coordsDF <- merge(my_data2, c, by = "Org_nam", all.x=TRUE)
#reorder again
#order <- coordsDF[, c(2:5,1,6:10)]

##export
write.csv(my_data2, file = "output\\fish_admins_BFT_each-month.csv")

# calculate number of occurrences by org
data <- polyDF %>%
  group_by(ID)  %>%
  # filter(dist < 100000) %>%  # & year == 2003
  tally()


# mdata <- merge(fishing_admins, data, by="ID", all.x=TRUE)
# mdata <- data.frame(mdata)

#### TO SHOW WHICH ADMINS HAVE BEEN SELECTED BY EACH MONTH
org_date <- polyDF %>%
  group_by(ym)  %>%
  distinct(Org_nam, Country)# %>%
#  summarize(n())


## to count for how many months an admin have been selected

admins_sel <- polyDF %>%
  group_by(ym)  %>%
  distinct(Org_nam) %>%
  group_by(Org_nam) %>%
  summarize(n())

## add country info (it get lost with summarize(n()) function)
org_cntr <- polyDF %>% dplyr::select(Org_nam, Dpndncy, Country, NUTS_level)
admins_sel_cntr <- admins_sel %>% left_join(org_cntr, by="Org_nam") %>% unique()

## to gather by country

country_admin <- org_date %>% 
  group_by(Country) %>%
  summarize(n())


## export

write.csv(org_date, file = "output\\fish_admins_BFT_monthly.csv")

write.csv(admins_sel_cntr, file = "output\\fish_admins_BFT_sel.csv")

write.csv(country_admin, file = "output\\country_fish_admin_bft_multisel.csv")




## for fw



setwd("C:\\DOM")

library(shapefiles)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(rgeos) #### intersect() function, careful because it has conflicts with dplyr
library(raster)


# set parameters
crswgs4326=CRS("+init=EPSG:4326")


##### import previously created layer of jurisdictional waters and related administrations
JW <- readShapePoly("output/jw_admins_new.shp",proj4string=crswgs4326,verbose=TRUE)
jw.laea <- spTransform(JW,CRS("+init=EPSG:3035"))
jw.laea <- gBuffer(jw.laea, byid=TRUE, width=0)

# select only fishing administartions to analyze interaction with bluefin tuna
traffic.laea <- subset(jw.laea, Sector == 'Marine traffic')

### create an ID for each row (some problem with the original data)

traffic.laea@data$ID <- seq.int(nrow(traffic.laea@data))

## to have lat lon of the orgs spatial points data frame
#punctual_infos <- readShapePoints("Stakeholders\\admins.shp",proj4string=crswgs4326,verbose=TRUE)
#punctual <- subset(punctual_infos, Sector == 'Marine traffic')
#coords <- as.data.frame(punctual)
#c <- coords[, c(5, 7, 8)]

#c$ID <- seq.int(nrow(c))
# replace conflicting characters
#repl <- read.csv("other data\\replacement.csv", header=TRUE, sep=";", colClasses = "character") #colClasses allow to keep character and not convert to factor
#for (i in 1:nrow(repl)){
#  c$Org_nam <- str_replace_all(c$Org_nam, pattern = repl[i,1], replacement = repl[i,2]) #overwrite
#}
#--------------------------------------------

# Define years and month to analyse data
year <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)
month <- c("01","02","03","04","05", "06", "07","08","09","10","11","12")

# Create empty dataframe to save the results
polyDF <- NULL


for (y in 1:length(year)){
  for (m in 1:length(month)){
    
    ## Import dynamic zones previously created
    shpfile <- paste0("output\\fw_", year[y], "_",month[m] ,".shp")
    if (!file.exists(shpfile)) next
    dynamic_zones <- readShapePoly(shpfile, proj4string=crswgs4326,verbose=TRUE) 
    zones.laea <- spTransform(dynamic_zones,CRS("+init=EPSG:3035"))
    
    ## intersect
    
    pi <- raster::intersect(traffic.laea, zones.laea)
    
    # calculate areas
    pi$area <- raster::area(pi)
    
    
    # calculate percentage
    pi$percent <- pi$area/sum(pi$area)
    
    pi@data$ym <- paste0(year[y],"_",month[m])
    pi@data$month <- month[m]
    pi@data$year <- year[y]
    
    
    # Save results to distDF
    polyDF <- rbind(polyDF, pi@data)
  }
}


order.fw <- polyDF[, c(29,30,31,2,11,9,8,6,24,25)]
## add coords to the df
#coordsDF <- merge(my_data2, c, by = "Org_nam", all.x=TRUE)
#reorder again
#order.fw <- coordsDF[, c(2:5,1,6:10)]

##export
write.csv(order.fw, file = "output\\traffic_admins_FW_each-month.csv")

# calculate number of occurrences by org
data <- polyDF %>%
  group_by(ID)  %>%
  # filter(dist < 100000) %>%  # & year == 2003
  tally()


# mdata <- merge(fishing_admins, data, by="ID", all.x=TRUE)
# mdata <- data.frame(mdata)

#### TO SHOW WHICH ADMINS HAVE BEEN SELECTED BY EACH MONTH
org_date <- polyDF %>%
  group_by(ym)  %>%
  distinct(Org_nam, Country)# %>%
#  summarize(n())


## to count for how many months an admin have been selected

admins_sel <- polyDF %>%
  group_by(ym)  %>%
  distinct(Org_nam) %>%
  group_by(Org_nam) %>%
  summarize(n())

## add country info (it get lost with summarize(n()) function)
org_cntr <- polyDF %>% dplyr::select(Org_nam, Dpndncy, Country, NUTS_level)
admins_sel_cntr <- admins_sel %>% left_join(org_cntr, by="Org_nam") %>% unique()

## to gather by country

country_admin <- org_date %>% 
  group_by(Country_y) %>%
  summarize(n())


## export

write.csv(org_date, file = "output\\traffic_admins_fw_monthly.csv")

write.csv(admins_sel_cntr, file = "output\\traffic_admins_fw_sel.csv")

write.csv(country_admin, file = "output\\country_traffic_admin_fw_multisel.csv")





###################
# This script serve to georeference each stakeholder organization (excluding administartions) of the database 
# based on their address
# Output of this script is the Point Shapefile of the orgnizations (excluding administrations)

setwd("C:\\DOM")

library(shapefiles)
library(stringr)
#library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(ggmap) ####geocoding library
library(stringi)



# load data
### important the addresses should be written without "," otherwise there will be mismatches in rows: use other simbols (e.g. "-", "/")
### specify the "Postal_code" as character to avoid automatic deletion of numbers series starting with zeros


# for punctual infos run:
org <- read.csv("C:\\DOM\\input\\stakeholdersDB\\punctual_infos.csv", sep=",",header = TRUE, colClasses=c("Postal_code"="character"))

##################--------------------------------------

# create full street string (address, zip code, city, country)
org$location <- paste(org$Address,org$Postal_code, org$City, org$Country, sep=", ")

# replace conflicting characters using a "replacement" file
repl <- read.csv("C:\\DOM\\input\\supplementary_join\\replacement.csv", header=TRUE, sep=";", colClasses = "character") #colClasses allow to keep character and not convert to factor
for (i in 1:nrow(repl)){
  org$location <- str_replace_all(org$location, pattern = repl[i,1], replacement = repl[i,2]) #overwrite
}
#--------------------------------------------

#################################################
###################################################

# create partial street string (zip code, city, country)
org$location2 <- paste (org$Postal_code, org$City, org$Country, sep=", ")

# replace conflicting characters

for (i in 1:nrow(repl)){
  org$location2 <- str_replace_all(org$location2, pattern = repl[i,1], replacement = repl[i,2]) #overwrite
}
#--------------------------------------------

# create only city and country street string
org$location3 <- paste (org$City, org$Country, sep=", ")

# replace conflicting characters

for (i in 1:nrow(repl)){
  org$location3 <- str_replace_all(org$location3, pattern = repl[i,1], replacement = repl[i,2]) #overwrite
}
#--------------------------------------------

####################################################
################## geocode the address list(vector)

# if our vector is not made by character data type you need to convert into character all the locations name

is.character(org$location) ## ok. It is character already

## geocode 
## before it worked fine; now google restricted the API access, still it works by creating an API key: https://www.r-bloggers.com/geocoding-with-ggmap-and-the-google-api/
fishcoord <- geocode(org$location)


org$lon <- fishcoord$lon
org$lat <- fishcoord$lat


## rerun the geocoding at more general level (location2) when the lat-lon result is NA

sel <- which(is.na(org$lon))
fishcoord2 <- geocode(org$location2[sel])

org$lon[sel] <- fishcoord2$lon
org$lat[sel] <- fishcoord2$lat


## rerun the geocoding at more general level (location3) when the lat-lon result is NA

sel <- which(is.na(org$lon))
fishcoord3 <- geocode(org$location3[sel])

org$lon[sel] <- fishcoord3$lon
org$lat[sel] <- fishcoord3$lat

### clean a bit
orgs <- org[, c(1:20, 33:34)]

#########################################################
########### !optional! ##################################
######### to export the dataframe with coords infos######
## delete unwanted recently created location columns for exportation of file with coords
# org_coord <- org[, c(1:20, 24:25)]

##### all coords are found (possible problems with specific characters)

##export (to csv, row.names=FALSE)
## for punctual data
# write.table(org_coord, "C:\\R Fed\\Viewer project\\Results\\punctual_org_coords.csv", sep=";", dec=",", row.names=FALSE)
##############################################


####################################################
######################## map with leaflet ########################
################################################

library(rgeos)
library(spatstat)
library(sp)
library(raster)


# convert to spatial class
coordinates(orgs) = ~lon+lat
plot(orgs)
summary(orgs)

## set CRS
proj4string(orgs)=CRS("+init=EPSG:4326") 
####### org.laea <- spTransform(fish,CRS("+init=EPSG:3035")) ejemplo conversion sistema metrico


## create map

popup <- paste0("<strong>Organisation: </strong>", orgs$Org_name)

map <- leaflet() %>%
  addProviderTiles("Esri.OceanBasemap") %>%  # Base map
  addCircles(data=orgs, popup=~popup,
             fillOpacity = 0.5)
map  # plot map



####export spatial.points.data.frame to shapefile

writeOGR(orgs, "C:\\DOM\\output", "punctual_orgs", driver = "ESRI Shapefile")

###################
# This script serve to associate the administrative organizations with the 
# area of the jurisdictional waters for which they are responsible
# Output of this script is the polygon Shapefile of the jurisdictional waters 
# in which are addressed in the attribute table the responsible organizations (for traffic and fishing sectors)

setwd("C:\\DOM")

library(shapefiles)
library(stringr)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(ggmap) ## geocoding library
library(stringi)
library(sp)



#######################################################
##### 1. Spatial Join of points to land polys #########
#######################################################

## import punctual information of administrations
org <- readOGR(dsn="output",layer="admins")


# adjust the ID that had some problem on original df
org@data$ID <- seq.int(nrow(org@data))


# import the polygonshapefile with land geography (EU&nonEU countries + EU NUTS)

country_med <- readOGR(dsn="input/boundaries",layer="country_med")

NUTS <- readOGR(dsn="input/boundaries",layer="NUTS")

### when I do the spjoin with over() function it considers one stat_level each time 
### so I need to subset nuts layer for each stat level
NUTS0 <- subset(NUTS, STAT_LEVL_ == 0)
NUTS1 <- subset(NUTS, STAT_LEVL_ == 1)
NUTS2 <- subset(NUTS, STAT_LEVL_ == 2)
NUTS3 <- subset(NUTS, STAT_LEVL_ == 3)

# Reproject the other SpatialDataFrames so that 
# they use the same projection as NUTS layer.

org.reproj <- spTransform(org,CRS(proj4string(NUTS)))
proj4string(org.reproj)=CRS(proj4string(NUTS))


country.reproj <- spTransform(country_med,CRS(proj4string(NUTS)))
proj4string(country.reproj)=CRS(proj4string(NUTS))


## use over() function from sp package; not points.in.poly() because 
# it delete from results points that are not in the join poligons; 
# this will introduce a CNTR_ID = ISO_2 (important for extra UE countries spatial relations since they have no NUTS)

admin_country <- over(org.reproj, country.reproj)


### create an identical ID to the one of the spatial point org file for each point 
# because the over() does not create it automtically
n <- nrow(admin_country)
admin_country$ID <- c(1:n)

## Now I have a CNTR_ID with ISO_2 code (for all the points)
############# create a corresponding ISO_3 column (because marine region layers has only ISO_3 as GEO_ID)
### for this load a table with the two kinds of ISO_codes related
# download table at: http://www.sustainableworld.com/usingdata/data_issues/iso_lu.html

ISO_code <- read.csv("input\\supplementary_join\\ISO_code.csv", sep=",",header = TRUE)
ISO <- ISO_code[, c("ISOA2","ISOA3")]

## problem: it read the NA values as NA of Namibia replace NA with "not found"
admin_country$CNTR_ID <- as.character(admin_country$CNTR_ID)
c <- admin_country$CNTR_ID
admin_country$CNTR_ID <- ifelse(is.na(admin_country$CNTR_ID), "not found", c)

## join orgs infos with ISO infos

admin_ISO_2_3 <- left_join(admin_country, ISO, by = c("CNTR_ID"="ISOA2"))

## now I have ISO3 code for each admin 


#############################################
# make spatial join with NUTS layers (over considers 1 NUTS level each time)

# NUTS 0 level

NUTS0_spjoin <- over(org.reproj, NUTS0)
### create an identical ID as the one of the spatial point org file
NUTS0_spjoin$ID <- c(1:n)

# NUTS 1 level
NUTS1_spjoin <- over(org.reproj, NUTS1)
NUTS1_spjoin$ID <- c(1:n)

# NUTS 2 level
NUTS2_spjoin <- over(org.reproj, NUTS2)
NUTS2_spjoin$ID <- c(1:n)

# NUTS 3 level
NUTS3_spjoin <- over(org.reproj, NUTS3)
NUTS3_spjoin$ID <- c(1:n)

## merge infos at different geo levels


ISO_spjoin <- merge(org.reproj, admin_ISO_2_3, by="ID")
ISO_N0_spjoin <- merge(ISO_spjoin, NUTS0_spjoin, by="ID")
ISO_N01_spjoin <- merge(ISO_N0_spjoin, NUTS1_spjoin, by="ID", suffixes = c(".x",".y"))

#### important with ifelse() create a unique stat level column for NUTS depending from the NUTS level 
## of each administartion (specified on a column from admins original df)
### consider that ifelse can have problems depending from class of data (see below)
## so before I make columns I need for the ifelse() as.character

ISO_N01_spjoin@data$STAT_LEVL_.x <- as.character(ISO_N01_spjoin@data$STAT_LEVL_.x)
ISO_N01_spjoin@data$NUTS_lv <- as.character(ISO_N01_spjoin@data$NUTS_lv)
ISO_N01_spjoin@data$NUTS_ID.x <- as.character(ISO_N01_spjoin@data$NUTS_ID.x)
ISO_N01_spjoin@data$NUTS_ID.y <- as.character(ISO_N01_spjoin@data$NUTS_ID.y)

ISO_N01_spjoin@data$NUTS_code <- ifelse(ISO_N01_spjoin@data$STAT_LEVL_.x == ISO_N01_spjoin@data$NUTS_lv, ISO_N01_spjoin@data$NUTS_ID.x, ISO_N01_spjoin@data$NUTS_ID.y)


##### repeat for NUTS 2 level

ISO_N012_spjoin <- merge(ISO_N01_spjoin, NUTS2_spjoin, by="ID", suffixes = c(".y",".u"))

ISO_N012_spjoin@data$STAT_LEVL_ <- as.character(ISO_N012_spjoin@data$STAT_LEVL_)
ISO_N012_spjoin@data$NUTS_ID <- as.character(ISO_N012_spjoin@data$NUTS_ID)

ISO_N012_spjoin@data$NUTS_code <- ifelse(ISO_N012_spjoin@data$STAT_LEVL_ == ISO_N012_spjoin@data$NUTS_lv, ISO_N012_spjoin@data$NUTS_ID , ISO_N012_spjoin@data$NUTS_code)


##### repeat for NUTS 3 level

ISO_N0123_spjoin <- merge(ISO_N012_spjoin, NUTS3_spjoin, by="ID", suffixes = c(".u",".z"))

ISO_N0123_spjoin@data$NUTS_ID.z <- as.character(ISO_N0123_spjoin@data$NUTS_ID.z)

ISO_N0123_spjoin@data$NUTS_code <- ifelse(ISO_N0123_spjoin@data$STAT_LEVL_.z == ISO_N0123_spjoin@data$NUTS_lv, ISO_N0123_spjoin@data$NUTS_ID.z , ISO_N0123_spjoin@data$NUTS_code)

### now I have a complete NUTS_code column which specify the NUTS code for which each admin is responsible


####################### 
View(ISO_N0123_spjoin@data)

# for some weird reason the merge with the sp_join results data does not work
### so I will use the original dataframe (no spatial) with the administrations and join it with the jw_rel, and than 
### add this to the sp join results just created which include infos on GEO_IDs(NUTS_code etc) necessary to 
### relate orgs with jurisdictional waters polygons
### always use the org ID to join since it keeps being the same
## keep the important variables for relation with jurisdictional waters: ID, CNTR_ID, ISO3, NUTS_code
spjoin_res <- ISO_N0123_spjoin[, c(1, 19, 22,31)]

#### create a NUTS_name by relating with NUTS downlaoded infos table

NUTS_infos <- read.csv("input\\supplementary_join\\NUTS_infos.csv", sep=",",header = TRUE)

# replace conflicting characters
repl <- read.csv("input\\supplementary_join\\replacement.csv", header=TRUE, sep=";", colClasses = "character") #colClasses allow to keep character and not convert to factor
for (i in 1:nrow(repl)){
  NUTS_infos$Description <- str_replace_all(NUTS_infos$Description, pattern = repl[i,1], replacement = repl[i,2]) #overwrite
}

# clean data and adjust column name
NUTS_infos <- NUTS_infos[, c(5,6)]
names(NUTS_infos)[names(NUTS_infos) == 'Description'] <- 'NUTS_name'

# merge main df with NUTS information
spjoin_res <- merge(spjoin_res, NUTS_infos, by.x = "NUTS_code", by.y = "NUTS.Code", all.x= TRUE, duplicateGeoms = TRUE)




#####################################################################################
####################################################################################
#######  2. attach jurisdictional waters relation matrix to each administration ##############
#####################################################################################

admin <- read.csv("input\\stakeholdersDB\\admins.csv", sep=";",header = TRUE)
jw_rel <- read.csv("input\\supplementary_join\\JW_relation.csv", sep=";",header = TRUE)

# some column changed name after import and it have to be corrected to merge
colnames(org@data)[colnames(org@data)=="Org_typ"] <- "Org_type"
colnames(org@data)[colnames(org@data)=="NUTS_lv"] <- "NUTS_level"

# merge the organizations with the competences matrix
admin_rel <- merge(org, jw_rel, by = c("Country","Sector", "Org_type", "NUTS_level"), all.x=TRUE, all.y=TRUE)
admin_jw <- merge(spjoin_res, admin_rel, by = "ID")

View(admin_jw@data)

### Create a unique Geo_ID (for all orgs, EU and non-EU)
admin_jw@data$Geo_ID <- ifelse(is.na(admin_jw@data$NUTS_code), admin_jw@data$CNTR_ID, admin_jw@data$NUTS_code)
##create also a Geo_ID with ISO3_code
admin_jw@data$ISOA3 <- as.character(admin_jw@data$ISOA3)
admin_jw@data$Geo_ID3 <- ifelse(is.na(admin_jw@data$NUTS_code), admin_jw@data$ISOA3, admin_jw@data$NUTS_code)
writePointsShape(admin_jw,"output\\admins_complete_info")

#################### NOTE ################
## for laws I could use "ISO3" instead of "NUTS_level", should work
######################



############################################################################
###################### 3.jw layers join ###################################
##########################################################################


library(plyr)
# library(dplyr)
# library(rgdal)
library(tidyr)
library(xlsx)
library(sp)

# load data from csv
# admin <- read.csv("C:\\R Fed\\Viewer project\\database\\admin_rel_last (2).csv", sep=";",header = TRUE)
# admin

#### load shapefile IW:
shape_IW <- readOGR(dsn="input/boundaries", layer="IW_spjoin")

### this file is the result of the spatial join of IW multipoligon layer on ArcGIS 
### which aquired geo_id of EU and extra_EU adjacent terrestrial regions
### I want to create the geographical ID putting together the country_ID (ISO3 code) column 
### (for extra EU) with the NUTS_ID column (for EU) in the spatial df attribute table. 
### (NUTS_ID column is the result of the previous spatial join in ArcGIS)

c <- as.character(shape_IW$NUTS_ID)
x <- as.character(shape_IW$ISO_Ter1_1)

shape_IW$Geo_ID <- ifelse(is.na(c), x, c)


#### clean attribute table

IW <- shape_IW[, c("Pol_type_1","Geo_ID","ISO_Ter1_1","Territor_1")] # "Original_ID",


###### TS:

shape_TS <- readOGR(dsn="input/boundaries",layer="TS_clip")

## this is the original file from marineregions clipped on the west med in ArcGIS

## to have a column called as well Geo_ID I create it from ISO_Ter1 code

shape_TS@data$Geo_ID <- shape_TS@data$ISO_Ter1


#### Substitute "12NM" from marineregions into "territorial sea" because it's nicer

shape_TS@data$Pol_type <- rep("Territorial Sea",nrow(shape_TS))

###### clean attribute table

TS <- shape_TS[, c("Pol_type","Geo_ID","ISO_Ter1","Territory1")] # "Original_ID",


####### repeat for EEZ

shape_EEZ <- readOGR(dsn="input/boundaries",layer="eez_erase_ts_iw")

# this is the original file from marineregions clipped on the "west med" and with the surface of TS and IW 
# erased (the EEZ entirely overlap both IW and TS) in ArcGIS
##### make key colnames as the other two layers:

shape_EEZ@data$Geo_ID <- shape_EEZ@data$ISO_Ter1
shape_EEZ@data$ISO_Ter1_1 <- shape_EEZ@data$ISO_Ter1
shape_EEZ@data$Pol_type_1 <- shape_EEZ@data$Pol_type
shape_EEZ@data$Territor_1 <- shape_EEZ@data$Territory1

###### clean attribute table

EEZ <- shape_EEZ[, c("Pol_type_1","Geo_ID","ISO_Ter1_1","Territor_1")] #"Original_ID",

#### Merge all the 3 jurisdictional waters type spatialdata.frames in one 

complete_jw <- rbind(IW, TS, EEZ, makeUniqueIDs = TRUE)

# create ID
n <- nrow(complete_jw)
complete_jw$ID <- c(1:n)

##### name variables properly

colnames(complete_jw@data)[which(colnames(complete_jw@data) == 'Pol_type_1')] <- 'Jurisdiction'
colnames(complete_jw@data)[which(colnames(complete_jw@data) == 'Territor_1')] <- 'Country'
colnames(complete_jw@data)[which(colnames(complete_jw@data) == 'ISO_Ter1_1')] <- 'ISO_3'


#######
# set CRS

jw.laea <- spTransform(complete_jw,CRS("+init=EPSG:3035")) #### 

### Now make the selective join with the admin dataframe
## I can't do it directly on the spatial data because "filter" function makes permanent modifications
### consider to use inner join


TS_join <- complete_jw@data %>% 
  filter(Jurisdiction == "Territorial Sea") %>%
  full_join(filter(admin_jw@data, TS == 1), by = c("Geo_ID" = "ISOA3"))



EEZ_join <- complete_jw@data %>% 
  filter(Jurisdiction %in% c("200NM", "Disputed")) %>%
  full_join(filter(admin_jw@data, EEZ == 1), by = c("Geo_ID" = "ISOA3"))


IW_join <- complete_jw@data %>% 
  filter(Jurisdiction == "Internal waters") %>%
  full_join(filter(admin_jw@data, IW == 1), by = c("Geo_ID"="Geo_ID3"))

complete_join <- rbind.fill(IW_join, TS_join, EEZ_join)
complete_join <- unique(complete_join)

#### # # # # create one ID and clean the dataframe # # # # # # 

complete_join$ID.y <- NULL
complete_join$Geo_ID.y <- NULL
colnames(complete_join)[which(names(complete_join) == "ID.x")] <- "ID"


### some extra column in complete_join, eventually do some cleaning:  # dplyr::select()
# complete_join <- complete_join[, c()]
##########


## relate orgs joined with jw polygons ID with spatial df of jurisdictional water
admin_join <- merge(complete_jw, complete_join, by="ID", all.x = TRUE, duplicateGeoms = TRUE)


############


############

#### !!!!! many polygons repeated
### just filter for those features that include org_name, which means they were joined to some org
admin_join <- admin_join[!is.na(admin_join@data$Org_nam) ,] ###




### clean a bit, many repetitions on GEO codes
## 
admin_join <- admin_join[, c(1:5,11, 16:31, 35:36)]

#### duplicated poligons, need to create a unique ID

n <- nrow(admin_join@data)
admin_join@data$ID <- c(0:(n-1)) 


# Export
#writePolyShape(admin_join,"C:\\Dynamic Ocean Management\\Database\\Spatial_layers\\jw_admins")
rgdal::writeOGR(admin_join, "C:/DOM/output", "jw_admins", driver="ESRI Shapefile")
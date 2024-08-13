#############################################################################################
#############################################################################################
##
## TITLE: Gather and Clean GBIF Occurrences for surival analysis.
##
## AUTHOR: Georgia Thomas
##
## INTRODUCTION
##
## This script is part of a study of survival at the botanical gardens.
## This script takes 
## future analysis.
##
## DATA FILES REQUIRED
##
##
## 1. "WFOAccectedNamesWithTaxonID_.txt" [WFO names from Surival analysis with "FindPhylogenyForMBGSurvival_[MOST CURRENT_DATE].R"]
##
##
## R PACKAGES REQUIRED
## 
## 1. "rgbif"
## 2. "CoordinateCleaner"
##
## CONTENTS
##
## 1.	Read data files and require packages.
## 2. Examine and Subset data to find Species Level Names.
## 3. Find Speices Taxon Key.
## 4. Find Occurrences.
## 5. Remove Bad Cordinates
## 6. Export Dataframe
##
##
#############################################################################################
#############################################################################################


#############################################################################################
# 1. Read data files.
#############################################################################################
#set working directory
setwd("S:/HortandResearchedSharedProjects/Survival/Data/LCMS_2023")

#load required packages
require(CoordinateCleaner)
library(rgbif) 
#library(dplyr)
#library(readr) 

##Load MoBot Survival Data/Species List
SurivalTaxonList<-read.table("WFOAccectedNamesWithTaxonID_ 2024Mar08_120421 .txt", header=T)

#############################################################################################
# 2. Examine and Subset data to find Species Level Names.
#############################################################################################
head(SurivalTaxonList)
dim(SurivalTaxonList)
colnames(SurivalTaxonList)

table(SurivalTaxonList$Matched)
SurivalTaxonList<-SurivalTaxonList[SurivalTaxonList$Matched==T,]
dim(SurivalTaxonList)


##removed ones where species is blank
sum(SurivalTaxonList$specificEpithet!="")
SurivalTaxonList<-SurivalTaxonList[SurivalTaxonList$specificEpithet!="",]
dim(SurivalTaxonList)
colnames(SurivalTaxonList)

##create a species genus entry to search in GBIF
TaxonGenSpec<-paste(SurivalTaxonList$genus, SurivalTaxonList$specificEpithet, sep=" ")
length(TaxonGenSpec)


##Create a key to associated data with taxaon ID latter 
SurivalTaxonList<-data.frame(cbind(SurivalTaxonList$TaxonList.TaxonID, TaxonGenSpec))
dim(SurivalTaxonList)
colnames(SurivalTaxonList)
colnames(SurivalTaxonList)<-c("TaxonID","TaxonGenSpec")



##use a list called sp, with a collumn called Taxon
length(unique(SurivalTaxonList$TaxonGenSpec))
Taxon<-unique(SurivalTaxonList$TaxonGenSpec)
sp<-data.frame(Taxon)
sp$Taxon<-as.character(sp$Taxon)

#############################################################################################
# 3. Find Species Taxon Key.
#############################################################################################

#https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/
###rgibf credentials
# fill in your gbif.org credentials 
user <- "gthomasatmobot.org"
pwd <- "mbgGT1990" # your gbif.org password
email <- "gthomas@mobot.org" # your email 

####find Taxon Usage Key  
GBIFTaxonmyList<-name_backbone_checklist(sp$Taxon)
##combine with sp so you can see what was searched for
GBIFTaxonmyList<-cbind(sp, GBIFTaxonmyList)
colnames(GBIFTaxonmyList)[1]<-"SearchedForTaxon"
colnames(GBIFTaxonmyList)
##Save results
write.table(file=paste("LCMSGBIFTaxonmyList_", format(Sys.time(), "%Y-%m-%d %H%M%S"), ".txt", sep=""), GBIFTaxonmyList, col.names =T, row.names = F)


#Explore GBIF Taxonomy List
colnames((GBIFTaxonmyList))
table(GBIFTaxonmyList$matchType)
table(GBIFTaxonmyList$status)
table(GBIFTaxonmyList$rank)

#remove taxa that have rank higher than speices
GBIFTaxonmyList<-GBIFTaxonmyList[GBIFTaxonmyList$rank=="SPECIES",]
table(GBIFTaxonmyList$rank)


##Find if any taxon yielded no match
sum(is.na(GBIFTaxonmyList$usageKey))
GBIFTaxonmyList<-data.frame(GBIFTaxonmyList)
GBIFTaxonmyList[is.na(GBIFTaxonmyList$usageKey),]
sum(is.na(GBIFTaxonmyList$acceptedUsageKey))
sum(!is.na(GBIFTaxonmyList$acceptedUsageKey))



####################################################################
#Create table for use in Find species occurrence
##replace ussage key with acceptedUssageKey

###Create variable which list ussage keys for finding occurrences
TaxonKeyMyList<-GBIFTaxonmyList$usageKey
length(TaxonKeyMyList)
###replace synomy ussage keys with accepted usage keys
TaxonKeyMyList[!is.na(GBIFTaxonmyList$acceptedUsageKey)]<-GBIFTaxonmyList$acceptedUsageKey[!is.na(GBIFTaxonmyList$acceptedUsageKey)]

#RemoveNA
TaxonKeyMyList[is.na(TaxonKeyMyList)]
TaxonKeyMyList<-TaxonKeyMyList[!is.na(TaxonKeyMyList)]
length(TaxonKeyMyList)




#############################################################################################
# 4. Find Species Occurrences.
#############################################################################################


#A taxon key from the GBIF backbone. All included (child) and synonym taxa are included in the search, so a search for Aves with taxonKey=212 (i.e. /occurrence/search?taxonKey=212) will match all birds, no matter which species.Parameter may be repeated.
occ_download(
  pred_in("taxonKey", TaxonKeyMyList),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)


################OUtput
#<<gbif download>>
#Your download is being processed by GBIF:
#  https://www.gbif.org/occurrence/download/0016231-230810091245214
#Most downloads finish within 15 min.
#Check status with
#occ_download_wait('0044273-240314170635999')
#After it finishes, use
#d <- occ_download_get('0044273-240314170635999') %>%
#  occ_download_import()
#to retrieve your download.
#Download Info:
#  Username: gthomasatmobot.org
#E-mail: gthomas@mobot.org
#Format: SIMPLE_CSV
#Download key: 0044273-240314170635999
#Created: 2024-03-20T15:08:37.715+00:00
#Citation Info:  
#  Please always cite the download DOI when using this data.
#https://www.gbif.org/citation-guidelines
#DOI: 10.15468/dl.zxevkz
#Citation:
#  GBIF Occurrence Download https://doi.org/10.15468/dl.zxevkz Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2024-03-20
#
#

###can get dataset from website also, get the data and open in R
##name file taxon taxon_GBIF_locations
#gbifData<-data.table::fread("occurrence.txt")

##or use this from the print out: changing the DOI
gbifData <- occ_download_get('0044273-240314170635999') %>%
  occ_download_import() ####use the number given in the text from occ_download function 


##test file
dim(gbifData)
colnames(gbifData)
head(gbifData)


##understand the data pull better
table(gbifData$family)
length(unique(gbifData$speciesKey))
table(gbifData$speciesKey)
length(unique(gbifData$taxonKey))


##this data set list all synomys 
table(gbifData$taxonRank)

##only pull ones where taxon key matches
length(unique(gbifData$taxonKey))

###why do some of the taxa keys now match speices keys?
##how do I match back to figure out synonmys?  ughh!
##ignorning this problem for now.  
length(gbifData[!(gbifData$speciesKey %in% TaxonKeyMyList),]$gbifID)
gbifData[!(gbifData$speciesKey %in% TaxonKeyMyList),]
length(unique(gbifData$taxonKey))


#############################################################################################
## 5. Remove Bad Cordinates
#############################################################################################



###Clean Data from GBIF
###removed if 1 km from geographic centriod
####removes records within 0.5 degrre radions of GBIF headquarters
gbifData<-cc_gbif(gbifData, lon = "decimalLongitude", lat = "decimalLatitude",
                  species = "Taxon", buffer = 1000, geod = TRUE, verify = FALSE,
                  value = "clean", verbose = FALSE)

###removed if 1 km from geographic centriod
gbifData<-cc_cen(gbifData, lon = "decimalLongitude", lat = "decimalLatitude",
                 species = "Taxon", buffer = 1000, geod = TRUE, test = "both",
                 ref = NULL, verify = FALSE, value = "clean", verbose = FALSE)

####removes records in the Vicinity of Biodiversity Institutions
gbifData<-cc_inst(gbifData, lon = "decimalLongitude", lat = "decimalLatitude",
                  species = "Taxon", buffer = 100, geod = TRUE, ref = NULL,
                  verify = FALSE, verify_mltpl = 10, value = "clean",
                  verbose = FALSE)
### Identify Non-terrestrial Coordinates
gbifData<-cc_sea(gbifData, lon = "decimalLongitude", lat = "decimalLatitude",
                 ref = NULL, scale = 110, value = "clean", speedup = TRUE,
                 verbose = FALSE)
##### Identify Invalid lat/lon Coordinates
gbifData<-cc_val(gbifData, lon = "decimalLongitude", lat = "decimalLatitude",
                 value = "clean", verbose = FALSE)
##### Identify Zero Coordinates
gbifData<-cc_zero(gbifData, lon = "decimalLongitude", lat = "decimalLatitude",
                  buffer = 0.5, value = "clean", verbose = FALSE)



#############################################################################################
# 6. Export Dataframe
#############################################################################################

dim(gbifData)
colnames(gbifData)
##subset to collumns we will need
gbifDataSubset<-data.frame(gbifData$gbifID, gbifData$datasetKey, gbifData$occurrenceID, gbifData$family, gbifData$genus, gbifData$species, gbifData$infraspecificEpithet, gbifData$taxonRank,
                           gbifData$day, gbifData$month, gbifData$year, gbifData$taxonKey, gbifData$speciesKey, gbifData$decimalLatitude, gbifData$decimalLongitude,
                           gbifData$verbatimScientificName)

write.table(file=paste("GBIFOccurrencePointsCleaned_", format(Sys.time(), "%Y%b%d_%H%M%S"), ".txt", sep=""), gbifDataSubset, col.names =T, row.names = F)

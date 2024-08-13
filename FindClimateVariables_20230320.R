#############################################################################################
#############################################################################################
##
## TITLE: Create climate variables for survival analysis.
##
## AUTHOR: Georgia Thomas
##
## INTRODUCTION
##
## This script is part of a study of survival at the botanical gardens.
## This script takes study species GBIF occurrences and matches them with climate.
## Then calcualtes summary statiics for each speices and one sided climate variables need for survival analysis.  
##
## DATA FILES REQUIRED
##
##
## 1. "GBIFOccurrencePointsCleaned_" [GBIF Occurrence points for study spices taxa "GBIFDataSurivalAnalysisPrep[MOST CURRENT_DATE].R"]
##
##
## R PACKAGES REQUIRED
## 
## 1. data.table
## 2. terra
##
## CONTENTS
##
## 1.	Read data files and require packages.
## 2. Examine data.  
## 3. Extract climate for occurrences.
## 4. Calculate climate stats by species/taxa.  
## 5. Calculate one sided climate variables.   
##
#############################################################################################
#############################################################################################


#############################################################################################
# 1. Read data files.
#############################################################################################
##load packages
install.packages("data.table")
library(data.table)
require(terra)

#set working directory
setwd("S:/HortandResearchedSharedProjects/Survival/Data/LCMS_2023")
##Load MoBot Survival Data/Species List
##reading in table this way becasue it is really big.
gbifData<-data.table::fread("GBIFOccurrencePointsCleaned_2024Mar20_143329.txt")



#############################################################################################
# 2. Examine data.
#############################################################################################

##test file
dim(gbifData)
colnames(gbifData)
head(gbifData)


##understand the data pull better
table(gbifData$gbifData.family)
length(unique(gbifData$gbifData.speciesKey))
table(gbifData$gbifData.speciesKey)
length(unique(gbifData$gbifData.taxonKey))

##still need to do more research to understand spices keys vs. taxon keys.
##species keys don't exactly match the previous list of species keys pulled when matching taxon list.




#############################################################################################
## 3. Extract climate for occurrences.
#############################################################################################

##get spatial data
##set working directory
setwd("//mbggis10/GisData/GlobalClimateData/WorldClimData/1970_2000_v2.1data/bioclimatic_variables")

#BIO1 = Annual Mean Temperature
#BIO5 = Max Temperature of Warmest Month
#BIO6 = Min Temperature of Coldest Month
#BIO12 = Annual Precipitation
#BIO13 = Precipitation of Wettest Month
#BIO14 = Precipitation of Driest Month

##pull in spatial data
##create what was a raster stack in the version before terra
r <- rast(c("wc2.1_30s_bio_1.tif", "wc2.1_30s_bio_5.tif", "wc2.1_30s_bio_6.tif", "wc2.1_30s_bio_12.tif", "wc2.1_30s_bio_13.tif", "wc2.1_30s_bio_14.tif"))


##extract climate values at gbifData Occurrence points
#https://rdrr.io/cran/terra/man/extract.html
#Extract values from a SpatRaster for a set of locations. The locations can be a SpatVector (points, lines, polygons), 
#a matrix with (x, y) or (longitude, latitude – in that order!) coordinates, or a vector with cell numbers.
gbfiOccurrencesClimateVals<-extract(r, cbind(gbifData$gbifData.decimalLongitude, gbifData$gbifData.decimalLatitude))

gbfiOccurrencesClimateVals<-data.frame(gbifData$gbifData.occurrenceID, gbifData$gbifData.speciesKey, gbifData$gbifData.taxonKey, gbfiOccurrencesClimateVals)


###save file if you don't want to spend the time extracting it again
#set working directory
setwd("S:/HortandResearchedSharedProjects/Survival/Data/LCMS_2023")
write.table(file=paste("gbfiOccurrencesClimateVals_", format(Sys.time(), "%Y%b%d_%H%M%S"), ".txt"), gbfiOccurrencesClimateVals, col.names =T, row.names = F)

#############################################################################################
## 4. Calculate climate stats by species/taxa. 
#############################################################################################
colnames(gbfiOccurrencesClimateVals)
length(unique(gbfiOccurrencesClimateVals$gbifData.gbifData.speciesKey))
summary(gbfiOccurrencesClimateVals)

##remove where climate values are NA
gbfiOccurrencesClimateVals<-gbfiOccurrencesClimateVals[!is.na(gbfiOccurrencesClimateVals$wc2.1_30s_bio_1),]
summary(gbfiOccurrencesClimateVals)


clim_stats<-data.frame()
for(i in 1:length(unique(gbfiOccurrencesClimateVals$gbifData.gbifData.speciesKey))){
  speciesKey<-gbfiOccurrencesClimateVals$gbifData.gbifData.speciesKey[i]
  num_obs<-length(gbfiOccurrencesClimateVals$gbifData.gbifData.occurrenceID[gbfiOccurrencesClimateVals$gbifData.gbifData.speciesKey==speciesKey])
  species_obs<-gbfiOccurrencesClimateVals[gbfiOccurrencesClimateVals$gbifData.gbifData.speciesKey==speciesKey,]
  if(num_obs>30){
    
    avg_amt<-mean(species_obs$wc2.1_30s_bio_1[!species_obs$wc2.1_30s_bio_1 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_1)$out])
    med_amt<-median(species_obs$wc2.1_30s_bio_1[!species_obs$wc2.1_30s_bio_1 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_1)$out])
    amt_25<-quantile(species_obs$wc2.1_30s_bio_1[!species_obs$wc2.1_30s_bio_1 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_1)$out], c(.025))
    amt_975<-quantile(species_obs$wc2.1_30s_bio_1[!species_obs$wc2.1_30s_bio_1 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_1)$out], c(.975))
    
    
    avg_ap<-mean(species_obs$wc2.1_30s_bio_12[!species_obs$wc2.1_30s_bio_12 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_12)$out])
    med_ap<-median(species_obs$wc2.1_30s_bio_12[!species_obs$wc2.1_30s_bio_12 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_12)$out])
    ap_25<-quantile(species_obs$wc2.1_30s_bio_12[!species_obs$wc2.1_30s_bio_12 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_12)$out], c(.025))
    ap_975<-quantile(species_obs$wc2.1_30s_bio_12[!species_obs$wc2.1_30s_bio_12 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_12)$out], c(.975))
    
    
    
    avg_mtwm<-mean(species_obs$wc2.1_30s_bio_5[!species_obs$wc2.1_30s_bio_5 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_5)$out])
    med_mtwm<-median(species_obs$wc2.1_30s_bio_5[!species_obs$wc2.1_30s_bio_5 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_5)$out])
    mtwm_25<-quantile(species_obs$wc2.1_30s_bio_5[!species_obs$wc2.1_30s_bio_5 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_5)$out], c(.025))
    mtwm_975<-quantile(species_obs$wc2.1_30s_bio_5[!species_obs$wc2.1_30s_bio_5 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_5)$out], c(.975))
    
    
    avg_mtcm<-mean(species_obs$wc2.1_30s_bio_6[!species_obs$wc2.1_30s_bio_6 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_6)$out])
    med_mtcm<-median(species_obs$wc2.1_30s_bio_6[!species_obs$wc2.1_30s_bio_6 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_6)$out])
    mtcm_25<-quantile(species_obs$wc2.1_30s_bio_6[!species_obs$wc2.1_30s_bio_6 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_6)$out], c(.025))
    mtcm_975<-quantile(species_obs$wc2.1_30s_bio_6[!species_obs$wc2.1_30s_bio_6 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_6)$out], c(.975))
    
    
    
    avg_pwm<-mean(species_obs$wc2.1_30s_bio_13[!species_obs$wc2.1_30s_bio_13 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_13)$out])
    med_pwm<-median(species_obs$wc2.1_30s_bio_13[!species_obs$wc2.1_30s_bio_13 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_13)$out])
    pwm_25<-quantile(species_obs$wc2.1_30s_bio_13[!species_obs$wc2.1_30s_bio_13 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_13)$out], c(.025))
    pwm_975<-quantile(species_obs$wc2.1_30s_bio_13[!species_obs$wc2.1_30s_bio_13 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_13)$out], c(.975))
    
    avg_pdm<-mean(species_obs$wc2.1_30s_bio_14[!species_obs$wc2.1_30s_bio_14 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_14)$out])
    med_pdm<-median(species_obs$wc2.1_30s_bio_14[!species_obs$wc2.1_30s_bio_14 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_14)$out])
    pdm_25<-quantile(species_obs$wc2.1_30s_bio_14[!species_obs$wc2.1_30s_bio_14 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_14)$out], c(.025))
    pdm_975<-quantile(species_obs$wc2.1_30s_bio_14[!species_obs$wc2.1_30s_bio_14 %in% boxplot.stats(species_obs$species_obs$wc2.1_30s_bio_14)$out], c(.975))
    
    
    
    
  }else{
      avg_amt<-mean(species_obs$wc2.1_30s_bio_1)
      med_amt<-median(species_obs$wc2.1_30s_bio_1)
      amt_25<-quantile(species_obs$wc2.1_30s_bio_1, c(.025))
      amt_975<-quantile(species_obs$wc2.1_30s_bio_1, c(.975))
     
      
      avg_ap<-mean(species_obs$wc2.1_30s_bio_12)
      med_ap<-median(species_obs$wc2.1_30s_bio_12)
      ap_25<-quantile(species_obs$wc2.1_30s_bio_12, c(.025))
      ap_975<-quantile(species_obs$wc2.1_30s_bio_12, c(.975))
      
      
      
      avg_mtwm<-mean(species_obs$wc2.1_30s_bio_5)
      med_mtwm<-median(species_obs$wc2.1_30s_bio_5)
      mtwm_25<-quantile(species_obs$wc2.1_30s_bio_5, c(.025))
      mtwm_975<-quantile(species_obs$wc2.1_30s_bio_5, c(.975))
      
      
      avg_mtcm<-mean(species_obs$wc2.1_30s_bio_6)
      med_mtcm<-median(species_obs$wc2.1_30s_bio_6)
      mtcm_25<-quantile(species_obs$wc2.1_30s_bio_6, c(.025))
      mtcm_975<-quantile(species_obs$wc2.1_30s_bio_6, c(.975))
      
      
      avg_pwm<-mean(species_obs$wc2.1_30s_bio_13)
      med_pwm<-median(species_obs$wc2.1_30s_bio_13)
      pwm_25<-quantile(species_obs$wc2.1_30s_bio_13, c(.025))
      pwm_975<-quantile(species_obs$wc2.1_30s_bio_13, c(.975))
      
      avg_pdm<-mean(species_obs$wc2.1_30s_bio_14)
      med_pdm<-median(species_obs$wc2.1_30s_bio_14)
      pdm_25<-quantile(species_obs$wc2.1_30s_bio_14, c(.025))
      pdm_975<-quantile(species_obs$wc2.1_30s_bio_14, c(.975))
      
    }
    newrow<-cbind(speciesKey, num_obs, 
                  avg_amt, med_amt, amt_25, amt_975, 
                  avg_ap, med_ap, ap_25, ap_975, 
                  avg_mtwm, med_mtwm, mtwm_25, mtwm_975, 
                  avg_mtcm, med_mtcm, mtcm_25, mtcm_975, 
                  avg_pwm, med_pwm, pwm_25, pwm_975, 
                  avg_pdm, med_pdm, pdm_25, pdm_975 
                  
                  
                  )
    clim_stats<-rbind(clim_stats, newrow)
    
  }


colnames(clim_stats)<-c(
  "speciesKey", "num_obs", 
  "avg_amt", "med_amt", "amt_25", "amt_975", 
  "avg_ap", "med_ap", "ap_25", "ap_975", 
  "avg_mtwm", "med_mtwm", "mtwm_25", "mtwm_975", 
  "avg_mtcm", "med_mtcm", "mtcm_25", "mtcm_975", 
  "avg_pwm", "med_pwm", "pwm_25", "pwm_975", 
  "avg_pdm", "med_pdm", "pdm_25", "pdm_975" 
)


write.table(file=paste("gbfiOccurrencesClimateStats_", format(Sys.time(), "%Y%b%d_%H%M%S"), ".txt"), clim_stats, col.names =T, row.names = F)


#############################################################################################
## 5. Calculate one sided climate variables. 
#############################################################################################


#still needs to be developed.  

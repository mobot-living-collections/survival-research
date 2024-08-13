#############################################################################################
#############################################################################################
##
## TITLE: Create Survival Data from LCMS exports.
##
## AUTHOR: Georgia Thomas
##
## INTRODUCTION
##
## This script is part of a study of survival at the botanical gardens.
## This script takes raw data from the LCMS (MBG's plant record database) and creates survival data
## useful for analysis.
##
## DATA FILES REQUIRED
##
## Ten data tables are pulled from the Microsoft SQL Server PlantRecordsProd.
##
## 1. "Taxon" [Taxon level data]
## 2. "Accession_20230705" [Accession level data]
## 3. "Planting" [Planting level data]
## 4. "Inventory" [Inventory level data]
## 5. "vw_PR_Inventories_ActionItem_Propagation" [Query in LCMS mirror that links all action item types to inventories]
## 6. "Area" [Defines AreaIDs]
## 7. "PlantStatus" [Defines PlantStatusIDs]
## 8. "DeathCause" [Defines Death Cause]
## 9. "PotSize" [Defines Pot Sizes]
## 10. "ActionItem" [Use to determine if Action Items are void]
## 11. "AccessionCollectionCoordinate" [Use to find coordiantes of provenance]
## 12. "Family" [Use to find family name]
## 13. "Habit" [Use to find habit name]
## 14. ""qry_ConservationSignStatus2_20230731.txt"" [Conservation status by Taxon]

##
## CONTENTS
##
## 1.	Read files.
## 2. Examine and format data.
## 3. Create Survival data.
## 4. Examine Survival data.  Edit and Censor data.  
## 5. Merge Survival data with relevant variables of interest from the LCMS. 
## 6. Export Dataframe
##
#############################################################################################
#############################################################################################


#############################################################################################
# 1. Read data files.
#############################################################################################
#set working directory
setwd("S:/HortandResearchedSharedProjects/Survival/Data/LCMS_2023")

#require packages for establishing database connections
require(odbc)
require(DBI)

##Establish database connection 
##the dsn file we have leads to only a read only database
#dsn file directs to PlantRecordsProd
db_conn <- dbConnect(odbc::odbc(), filedsn="S:/HortandResearchedSharedProjects/LCMS/LCMSDataConnect.dsn",pwd="PlantRecordsReadOnly")

##look at tables
dbListTables(db_conn)


#reading in data
Taxon<-dbGetQuery(db_conn, 'SELECT * FROM Taxon')
dim(Taxon)
head(Taxon)

Accession<-dbGetQuery(db_conn, 'SELECT * FROM Accession')
dim(Accession)
head(Accession)

Planting<-dbGetQuery(db_conn, 'SELECT * FROM Planting')
dim(Planting)
head(Planting)

Inventory<-dbGetQuery(db_conn, 'SELECT * FROM Inventory')
dim(Inventory)
head(Inventory)


Area<-dbGetQuery(db_conn, 'SELECT * FROM Area')
dim(Area)
head(Area)

PlantStatus<-dbGetQuery(db_conn, 'SELECT * FROM PlantStatus')
dim(PlantStatus)
head(PlantStatus)

DeathCause<-dbGetQuery(db_conn, 'SELECT * FROM DeathCause')
dim(DeathCause)
head(DeathCause)

PotSize<-dbGetQuery(db_conn, 'SELECT * FROM PotSize')
dim(PotSize)
head(PotSize)

ActionItem<-dbGetQuery(db_conn, 'SELECT * FROM ActionItem')
dim(ActionItem)
head(ActionItem)

CollectionCoordinat<-dbGetQuery(db_conn, 'SELECT * FROM AccessionCollectionCoordinate')
dim(CollectionCoordinat)
head(CollectionCoordinat)


Family<-dbGetQuery(db_conn, 'SELECT * FROM Family')
dim(Family)
head(Family)


Habit<-dbGetQuery(db_conn, 'SELECT * FROM Habit')
dim(Habit)
head(Habit)


InventoryRelationToPlantingKey<-dbGetQuery(db_conn, 'SELECT * FROM vw_PR_Inventories_ActionItem_Propagation')
dim(InventoryRelationToPlantingKey)
head(InventoryRelationToPlantingKey)

ConservationSignStatus<-dbGetQuery(db_conn, 'SELECT * FROM 
(ConservationSignStatus INNER JOIN ConservationStatus ON ConservationSignStatus.ConservationSignStatusID = ConservationStatus.ConservationSignStatusID) 
INNER JOIN TaxonConservationStatus ON ConservationStatus.ConservationStatusID = TaxonConservationStatus.ConservationStatusID')
dim(ConservationSignStatus)
head(ConservationSignStatus)


#############################################################################################
# 2. Examine and format data.
#############################################################################################
 
##Remove inactive data
Taxon<-Taxon[Taxon$Active==1,]
Planting<-Planting[Planting$Active==1,]
Accession<-Accession[Accession$Active==1,]

###changing generic column names
colnames(Area)[colnames(Area) == "Name"] <- "AreaName"
colnames(PlantStatus)[colnames(PlantStatus) == "Name"] <- "PlantStatusName"
colnames(DeathCause)[colnames(DeathCause) == "Name"] <- "DeathCauseName"
colnames(Family)[colnames(Family) == "Name"] <- "FamilyName"
colnames(Habit)[colnames(Habit) == "Name"] <- "HabitName"


###remove unnecessary inventories
##data includes off site properties and desert botanical garden
###looking at only inventories in main collections 
# 1 is mbg  #11 is ortli
head(Inventory$PropertyID)
Inventory<-Inventory[Inventory$PropertyID==1|Inventory$PropertyID==11,]
unique(Inventory$AreaID)

###Find and remove any InventoryRelationToPlantingKey that are Void
##Action Satus ID of 5 =Closed-Void
length(ActionItem$ActionStatusID[ActionItem$ActionStatusID==5])
length(ActionItem$ActionStatusID)
length(InventoryRelationToPlantingKey$ActionItemID[InventoryRelationToPlantingKey$ActionItemID %in% ActionItem$ActionItemID[ActionItem$ActionStatusID==5]])
head(InventoryRelationToPlantingKey$ActionItemID[InventoryRelationToPlantingKey$ActionItemID %in% ActionItem$ActionItemID[ActionItem$ActionStatusID==5]])
length(InventoryRelationToPlantingKey$ActionItemID[!(InventoryRelationToPlantingKey$ActionItemID %in% ActionItem$ActionItemID[ActionItem$ActionStatusID==5])])
dim(InventoryRelationToPlantingKey)
InventoryRelationToPlantingKey<-InventoryRelationToPlantingKey[!(InventoryRelationToPlantingKey$ActionItemID %in% ActionItem$ActionItemID[ActionItem$ActionStatusID==5]),]
dim(InventoryRelationToPlantingKey)


##STILL NOT CONVINCED THIS SECTION IS CORRECT
##examine plantings
##Every row in plantings should represent a unique planting
length(unique(Planting$PlantingID))
length((Planting$PlantingID))
#the read only planting number should also be unique
length(unique(Planting$ReadOnlyPlantingNumber))
##how many
length((Planting$PlantingID))-length(unique(Planting$ReadOnlyPlantingNumber))
##which ones are not unique?
data.frame(table(Planting$ReadOnlyPlantingNumber))[data.frame(table(Planting$ReadOnlyPlantingNumber))$Freq>1,]
temp<-data.frame(table(Planting$ReadOnlyPlantingNumber))[data.frame(table(Planting$ReadOnlyPlantingNumber))$Freq>1,]
###remove the ones that are not unique?  Cannot figure out why this happen
Planting<-Planting[!(Planting$ReadOnlyPlantingNumber %in% temp$Var1),]
dim(Planting[!(Planting$ReadOnlyPlantingNumber %in% temp$Var1),])




#############################################################################################
## 3. Create survival data.
#############################################################################################

###Data needed to understand inventories and plantings
InventoryPlanting<-merge(InventoryRelationToPlantingKey, Inventory, by="InventoryID", all=F)
InventoryPlanting<-merge(InventoryPlanting, Planting, by="PlantingID", all=F)
InventoryPlanting<-merge(InventoryPlanting, data.frame(PlantStatusID=PlantStatus$PlantStatusID, PlantStatusName=PlantStatus$PlantStatusName), by="PlantStatusID", all.x=T)
InventoryPlanting<-merge(InventoryPlanting, data.frame(DeathCauseID=DeathCause$DeathCauseID, DeathCauseName=DeathCause$DeathCauseName), by="DeathCauseID", all.x=T)
InventoryPlanting<-merge(InventoryPlanting, data.frame(AreaID=Area$AreaID, AreaName=Area$AreaName), by="AreaID", all.x=T)
InventoryPlanting<-merge(InventoryPlanting, data.frame(PotSizeID=PotSize$PotSizeID, PotSizeName=PotSize$PotSizeName), by="PotSizeID", all.x=T)

colnames(InventoryPlanting)
dim(InventoryPlanting)
length(InventoryPlanting$ReadOnlyPlantingNumber)
length(unique(InventoryPlanting$ReadOnlyPlantingNumber))

###Create Subset of Inventories that are outside
##These inventories are not in a greenhouse or at the nursery.
##At this point in time Hardy Plant Nursery Inventory data is not of high enough quality to count as outside.
##Plus plants at the nursery are often in prop
##We just want to examine plants planted outside on the garden grounds.



###Removing Indoor Inventories before planted on garden grounds 
unique(InventoryPlanting$AreaName)
InventoryPlantingOutside<-InventoryPlanting[InventoryPlanting$AreaName!="Desert House",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Old Climatron",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Hoop House 1",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Hoop House 2" ,]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Hoop House 3",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Hoop House 4",]  
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Orchid Show",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Brookings Center",]  
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Cuttings Prop House",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Unknown",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Ridgway Building - Indoors",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="National Council of Garden Clubs",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Corridor",]  
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse C1",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse B4",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Ridgway Dining Area",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse C2",]   
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse B1",]   
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Kemper Center for Home Gardening Building",] 
InventoryPlantingOutside<-InventoryPlantingOutside[!is.na(InventoryPlantingOutside$AreaName),]  
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse D4",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="For Your Consideration Pile",]  
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Linnean House",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse D1",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse A2",]   
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse B3",]  
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse A1",]   
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Shoenberg Temperate House",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse D3",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse A7",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse B2",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse C4",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse A4",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse D2",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse C3",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="CCSD Seed Bank" ,]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse Indoors",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse A6",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Commerce Bank Education Building",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Emerson Electric Conservation Center Indoors",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="MBG Seed Bank",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Kemper Experimental Garden",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Kemper Summer Plant House",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse Plant Transfer Area",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse A3",] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouses Headhouse" ,]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Main Greenhouse" ,]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Seed Prop House" ,] 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Nursery",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse A5",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Cohen Amphitheater",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Bayer Research Center",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Kemper Greenhouse",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Piper Observatory Plantings",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Shade House",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Climatron",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Greenhouse Outdoors",]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Kemper Entry Court" ,]
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$AreaName!="Alfred Avenue Nursery",]
#Property ID of 11 is ortli, remove 
InventoryPlantingOutside<-InventoryPlantingOutside[InventoryPlantingOutside$PropertyID!=11,]
###Check current Inventory Area names
unique(InventoryPlantingOutside$AreaName)


###examine data a bit more
sort(unique(InventoryPlantingOutside$AreaName))
dim(InventoryPlanting)
dim(InventoryPlantingOutside)
length(unique((InventoryPlantingOutside$ReadOnlyPlantingNumber)))
table(InventoryPlantingOutside$PlantStatusName)



##What does Pot Size look like
###how many plants have pot sizes?  
unique(InventoryPlantingOutside$PotSizeName)
sum(!is.na(InventoryPlanting$PotSizeName))

####If you get new data change date
datadate<-as.POSIXct(Sys.Date())
class(datadate)
###Change IneventoryDate to Date
class(InventoryPlantingOutside$InventoryDate)




######
##Find the accession year from the Read only planting number
class(InventoryPlantingOutside$ReadOnlyPlantingNumber)
PlantingYear<-substr(InventoryPlantingOutside$ReadOnlyPlantingNumber, start = 1, stop = 4)
####
#for the purposes of this study all plants accessioned prior to 2012 will be included as truncated data.
##LCMS was introduced in roughly 2012
length(InventoryPlantingOutside$PlantingID[PlantingYear>=2012])
length(InventoryPlantingOutside$PlantingID[PlantingYear<2012])

#########################################################################################################################
####loop to calculate the survival of each planting from the inventories as the difference from first recorded as observed alive outside to death or present day
#######1 full life recorded
#######0 have not observed full life 
####2 Unknown

pb <- txtProgressBar(1, length(unique(InventoryPlantingOutside$PlantingID)), style = 3)
Survival<-c()#days alive outside
PlantingID<-c()#LCMSPlantingID
Censored<-c()
AreaInitial<-c()
AreaFinal<-c()
DeathCause<-c()
DeathDate<-as.Date(character(0))
PlantCountInital<-c()
PlantCountFinal<-c()
FirstInventoryAliveOutside<-as.Date(character(0))

for(i in 1:length(unique(InventoryPlantingOutside$PlantingID))){
  pID<-unique(InventoryPlantingOutside$PlantingID)[i]
  PlantingID<-c(PlantingID, pID)
  pInventories<-InventoryPlantingOutside[InventoryPlantingOutside$PlantingID==pID,]
  ##want to find first and last area listed
  AreaInitial<-c(AreaInitial, pInventories[pInventories$InventoryDate==min(pInventories$InventoryDate, na.rm = T),]$AreaName[1])
  AreaFinal<-c(AreaFinal, pInventories[pInventories$InventoryDate==max(pInventories$InventoryDate, na.rm = T),]$AreaName[1])
  PlantCountInital<-c(PlantCountInital, pInventories[pInventories$InventoryDate==min(pInventories$InventoryDate, na.rm = T),]$PlantCount[1])
  PlantCountFinal<-c(PlantCountFinal, pInventories[pInventories$InventoryDate==max(pInventories$InventoryDate, na.rm = T),]$PlantCount[1])
  alive<-pInventories[pInventories$PlantStatusName=="Alive",]
  dead<-pInventories[pInventories$PlantStatusName=="Dead",]
  if(length(alive$InventoryID)>=1){alive<-min(alive$InventoryDate)
     FirstInventoryAliveOutside<-c(FirstInventoryAliveOutside, alive)  
    if(length(dead$InventoryID)>=1){
        DeathCause<- c(DeathCause, dead[dead$InventoryDate==min(dead$InventoryDate),]$DeathCauseName[1])
        DeathDate<- c(DeathDate, dead[dead$InventoryDate==min(dead$InventoryDate),]$InventoryDate[1])
        dead<-min(dead$InventoryDate)
        dayssurvived<-difftime(dead, alive, units = "days")
        Survival<-c(Survival, dayssurvived)
        Censored<-c(Censored, 1)}
      else
      {dead<-NA
          dayssurvived<-difftime(datadate, alive, unit="days") 
          Survival<-c(Survival, dayssurvived)
          Censored<-c(Censored,0)
          DeathCause<- c(DeathCause, NA)
         DeathDate<- c(DeathDate, NA)}
  }
  else{
    FirstInventoryAliveOutside<-c(FirstInventoryAliveOutside, NA)
    Survival<-c(Survival, NA)
    Censored<-c(Censored, NA)
    DeathCause<-c(DeathCause, NA)
    DeathDate<-c(DeathDate, NA)
  }
  setTxtProgressBar(pb, i)
}  


##Check if the lengths of the data are correct
length(unique(InventoryPlantingOutside$PlantingID))
length((PlantingID))


##find pot size before planting
##this requires a larger set of inventories because not just limited to alive and outside
PotSizeFinal<-c()
for(i in 1:length(unique(InventoryPlantingOutside$PlantingID))){
  pID<-unique(InventoryPlantingOutside$PlantingID)[i]
  pInventories<-InventoryPlanting[InventoryPlanting$PlantingID==pID,]
  pInventories<-pInventories[!is.na(pInventories$PotSizeName),]
  if(length(pInventories$InventoryID)>=1){
    PotSizeFinal<-c(PotSizeFinal, pInventories[pInventories$InventoryDate==max(pInventories$InventoryDate, na.rm = T),]$PotSizeName[1])
  } else 
  {PotSizeFinal<-c(PotSizeFinal,NA)}
  setTxtProgressBar(pb, i)
}



##Check if the lengths of the data are correct
length(unique(InventoryPlantingOutside$PlantingID))
length(PotSizeFinal)

  
##Create a dataframe of the new data  
SurvivalData<-data.frame(PlantingID, AreaInitial, AreaFinal, PlantCountInital, PlantCountFinal, Survival, Censored, DeathCause, DeathDate, FirstInventoryAliveOutside, PotSizeFinal)

#save survival data
setwd("S:/HortandResearchedSharedProjects/Survival/Data/LCMS_2023")
write.table(file=paste("LCMSSurvialDataIncludesAllNOTCENSORED_", format(Sys.time(), "%Y%b%d_%H%M%S"), ".txt"), SurvivalData, col.names =T, row.names = F)



#############################################################################################################################################
### 4. Examine Survival data.  Edit and Censor data.
##############################################################################################################################################

##Remove examples with NA for surival
##This typically happend if the plant was never listed as alive outside
##Often plant was a direct sow or a mistake
###Or the first inventory in the system is a dead.  
dim(SurvivalData)
sum(is.na(SurvivalData$Survival))
SurvivalData<-SurvivalData[!is.na(SurvivalData$Survival),]
dim(SurvivalData)


####Understand more about the FirstInventoryAliveOutside to check the data
summary(SurvivalData$FirstInventoryAliveOutside)
sum(SurvivalData$FirstInventoryAliveOutside < as.Date('2012/01/01',format='%Y/%m/%d'), na.rm = T)
dim(SurvivalData)

###Find out it moved inside after it was outside
class(SurvivalData$FirstInventoryAliveOutside)
sort(unique(InventoryPlanting$AreaName))
insideAreas<-sort(unique(InventoryPlanting$AreaName))[!(sort(unique(InventoryPlanting$AreaName)) %in% sort(unique(InventoryPlantingOutside$AreaName)))]
InventoryPlantingInside<-InventoryPlanting[InventoryPlanting$AreaName %in% insideAreas,]
table(InventoryPlantingInside$AreaName)
####loop to find suspect planting IDs
pb <- txtProgressBar(1, length(unique((SurvivalData$PlantingID))), style = 3)
MovingInsideOutsidePlantingID<-c()
for(i in 1:length(unique(SurvivalData$PlantingID))){
  PlantingID<-SurvivalData$PlantingID[i]
  pInventories<-InventoryPlantingInside[InventoryPlantingInside$PlantingID==PlantingID,]
  pInventories$InventoryDate<-as.Date(pInventories$InventoryDate)
  pInventories<-pInventories[pInventories$InventoryDate>SurvivalData$FirstInventoryAliveOutside[i], ]
  if(length(pInventories$InventoryID)>=1){MovingInsideOutsidePlantingID<-c(MovingInsideOutsidePlantingID, PlantingID)}
  setTxtProgressBar(pb, i)
}
length(MovingInsideOutsidePlantingID)
dim(SurvivalData)
SurvivalData<-SurvivalData[!(SurvivalData$PlantingID %in% MovingInsideOutsidePlantingID),]
dim(SurvivalData)

##Find if ever had a status other than alive and dead
##After it was moved outside
table(InventoryPlantingOutside$PlantStatusName)
InventoryPlantingOutsideNOTAorD<-InventoryPlantingOutside[!(InventoryPlantingOutside$PlantStatusName %in% c("Alive", "Dead")),]
table(InventoryPlantingOutsideNOTAorD$PlantStatusName)
pb <- txtProgressBar(1, length(unique((SurvivalData$PlantingID))), style = 3)
ProblematicInventories<-c()
for(i in 1:length(unique(SurvivalData$PlantingID))){
  PlantingID<-SurvivalData$PlantingID[i]
  pInventories<-InventoryPlantingOutsideNOTAorD[InventoryPlantingOutsideNOTAorD$PlantingID==PlantingID,]
  pInventories$InventoryDate<-as.Date(pInventories$InventoryDate)
  pInventories<-pInventories[pInventories$InventoryDate>SurvivalData$FirstInventoryAliveOutside[i], ]
  if(length(pInventories$InventoryID)>=1){ProblematicInventories<-rbind(ProblematicInventories, pInventories)}
  setTxtProgressBar(pb, i)
}
dim(ProblematicInventories)
length(unique(ProblematicInventories$PlantingID))
table(ProblematicInventories$PlantStatusName)
table(ProblematicInventories$PlantStatusName[ProblematicInventories$PlantStatusName!="Transferred"])
length(ProblematicInventories$PlantingID[ProblematicInventories$PlantStatusName!="Transferred"])
RemovePlantings<-unique(ProblematicInventories$PlantingID[ProblematicInventories$PlantStatusName!="Transferred"])
##which ones might we remove
length(unique(ProblematicInventories$PlantingID[ProblematicInventories$PlantStatusName!="Transferred"]))
##remove those with a status other than transferred
SurvivalData<-SurvivalData[!(SurvivalData$PlantingID %in% unique(ProblematicInventories$PlantingID[ProblematicInventories$PlantStatusName!="Transferred"])),]
dim(SurvivalData)
#####lengths do not seem right here
##still need to censor or remove anything that was transferred.  As the transfers were mostly weird in the end I decided to remove them 
ProblematicInventories[ProblematicInventories$PlantStatusName=="Transferred",]
length(SurvivalData[!(SurvivalData$PlantingID %in% unique(ProblematicInventories$PlantingID[ProblematicInventories$PlantStatusName=="Transferred"])),]$PlantingID)
SurvivalData<-SurvivalData[!(SurvivalData$PlantingID %in% unique(ProblematicInventories$PlantingID[ProblematicInventories$PlantStatusName=="Transferred"])),]


##Censor based on death cause
#######1 full life recorded
#######0 have not observed full life 
###right censor based on death cause being human managment 

#Examine Output
unique(SurvivalData$DeathCause)
table(SurvivalData$DeathCause, SurvivalData$Censored)
# "seed failed to germinate" does not correspond with alive 
##remove these samples
##looking at samples more they are often annuals
sum(SurvivalData$DeathCause=="seed failed to germinate"&!is.na(SurvivalData$DeathCause))
SurvivalData[SurvivalData$DeathCause=="seed failed to germinate"&!is.na(SurvivalData$DeathCause),]
SurvivalData<-SurvivalData[SurvivalData$DeathCause!="seed failed to germinate"|is.na(SurvivalData$DeathCause),]
dim(SurvivalData)

###all seeds used is also not a death cause that works if all plants were alive outside at one point
##examining data and removing
sum(SurvivalData$DeathCause=="all seed used" &!is.na(SurvivalData$DeathCause))
SurvivalData[SurvivalData$DeathCause=="all seed used" &!is.na(SurvivalData$DeathCause),]
SurvivalData<-SurvivalData[SurvivalData$DeathCause!="all seed used"|is.na(SurvivalData$DeathCause) ,]
dim(SurvivalData)


####In this case the propogation data was recorded incorrectly for cuttings
##The cuttings should have been made into a new planting and not recorded on the garden grounds as alive
sum(SurvivalData$DeathCause=="cutting failed" &!is.na(SurvivalData$DeathCause))
SurvivalData[SurvivalData$DeathCause=="cutting failed" &!is.na(SurvivalData$DeathCause),]
SurvivalData<-SurvivalData[SurvivalData$DeathCause!="cutting failed"|is.na(SurvivalData$DeathCause) ,]
dim(SurvivalData)



###Change to censor the data
SurvivalData$Censored[SurvivalData$DeathCause=="function in garden no longer met"&!is.na(SurvivalData$DeathCause)]<-0
SurvivalData$Censored[SurvivalData$DeathCause=="vandalism"&!is.na(SurvivalData$DeathCause)]<-0
SurvivalData$Censored[SurvivalData$DeathCause=="theft"&!is.na(SurvivalData$DeathCause)]<-0
SurvivalData$Censored[SurvivalData$DeathCause=="outgrew space"&!is.na(SurvivalData$DeathCause)]<-0
SurvivalData$Censored[SurvivalData$DeathCause=="mechanical damage"&!is.na(SurvivalData$DeathCause)]<-0
SurvivalData$Censored[SurvivalData$DeathCause=="invasive"&!is.na(SurvivalData$DeathCause)]<-0
table(SurvivalData$DeathCause, SurvivalData$Censored)


###I was worried about including root bound plants but think this could be considered a natural cause.
##We were not able to manage the plants correctly  
sum(SurvivalData$DeathCause=="root bound" &!is.na(SurvivalData$DeathCause))
SurvivalData[SurvivalData$DeathCause=="root bound" &!is.na(SurvivalData$DeathCause),]

###I was also worried about graft failed
###decided based on notes it is fine
##however, might want to consider this type of specimen to see if included in analysis?  
sum(SurvivalData$DeathCause=="graft failed" &!is.na(SurvivalData$DeathCause))
SurvivalData[SurvivalData$DeathCause=="graft failed" &!is.na(SurvivalData$DeathCause),]



##################################################################################################################################
#5. Merge Survival data with relevant variables of interest from the LCMS.
####################################################################################################################################

###Merge with relevant Planting info
colnames(Planting)
SurvivalData<-merge(SurvivalData, data.frame(PlantingID=Planting$PlantingID, ReadOnlyPlantingNumber=Planting$ReadOnlyPlantingNumber, AccessionID=Planting$AccessionID, ReadOnlyPlantingNumber=Planting$ReadOnlyPlantingNumber 
                  ), by="PlantingID", all.x=T)

##Check in anything doesn't match
sum(is.na(SurvivalData$AccessionID))

#Merge with relevant Accession Level Data
dim(SurvivalData)
colnames(Accession)
SurvivalData<-merge(SurvivalData, data.frame(AccessionID=Accession$AccessionID, TaxonID=Accession$TaxonID, ProvenanceID=Accession$ProvenanceID, 
                                             ReadonlyAccessionNumber=Accession$ReadonlyAccessionNumber, 
                                             SourceID=Accession$SourceID,
                                             AccessionYear=Accession$AccessionYear,
                                             CollectionCountryCode=Accession$CollectionCountryCode), by="AccessionID", all.x=T)
dim(SurvivalData)

###Check to see if any did not match
sum(is.na(SurvivalData$TaxonID))
SurvivalData[is.na(SurvivalData$TaxonID),]
##This is because the Taxon is not active and it trickles down to the planting not being active
dim(SurvivalData)
SurvivalData<-SurvivalData[!is.na(SurvivalData$TaxonID),]
dim(SurvivalData)

####Lets look at Provenance
#ProvenanceID	Name	Description	ZincSize	Code
#1 Cultivated	Plant not of a wild source.	Regular	C
#2	Unknown	Insufficient data to determine Provenance Type.	Regular	U
#3	Wild	Plant of wild source.	Collected	W
#4	Cultivated from wild	Propagule from a wild source plant in cultivation.	Regular	W/C
#5	To be fixed	To be investigated and fixed.	Regular	X
table(SurvivalData$ProvenanceID)
sum(is.na(SurvivalData$ProvenanceID))
SurvivalData$ProvenanceID[SurvivalData$ProvenanceID==1]<-"Cultivated"
SurvivalData$ProvenanceID[SurvivalData$ProvenanceID==2]<-"Unknown"
SurvivalData$ProvenanceID[SurvivalData$ProvenanceID==3]<-"Wild"
SurvivalData$ProvenanceID[SurvivalData$ProvenanceID==4]<-"Cultivated from wild"
SurvivalData$ProvenanceID[SurvivalData$ProvenanceID==5]<-"To be fixed"
table(SurvivalData$ProvenanceID)
colnames(SurvivalData)[colnames(SurvivalData) == "ProvenanceID"] <- "ProvenanceName"

###Merge with Collection Location
colnames(CollectionCoordinat)
SurvivalData<-merge(SurvivalData, data.frame(AccessionID=CollectionCoordinat$AccessionID, Latitude=CollectionCoordinat$Latitude, 
                                             Longitude=CollectionCoordinat$Longitude, GPSAccuracy=CollectionCoordinat$GPSAccuracy), by="AccessionID", all.x=T)
dim(SurvivalData)

sum(!is.na(SurvivalData$Latitude))
sum(!is.na(SurvivalData$Longitude))

#Merge with relevant Taxon Level Data
colnames(Taxon)
head(Taxon[!is.na(Taxon$Parentage) & Taxon$Parentage!="",]$Parentage)
head(Taxon[!is.na(Taxon$Group) & Taxon$Group!="",]$Group)
SurvivalData<-merge(SurvivalData, data.frame(TaxonID=Taxon$TaxonID, FamilyID=Taxon$FamilyID, Genus=Taxon$Genus, GenusHybrid=Taxon$GenusHybrid, Species=Taxon$Species, 
                                             SpeciesHybrid=Taxon$SpeciesHybrid, InfraspecificName=Taxon$InfraspecificName, InfraspecificRankID=Taxon$InfraspecificRankID,
                                             CultivarName=Taxon$CultivarName, SeriesTradename=Taxon$SeriesTradename, Parentage=Taxon$Parentage, HabitID=Taxon$HabitID, 
                                             LifeCycleDurationID=Taxon$LifeCycleDurationID, ComputedTaxonScientificName=Taxon$ComputedTaxonScientificName,
                                             ComputedTaxonName=Taxon$ComputedTaxonName), 
                    by="TaxonID", all.x=T)
dim(SurvivalData)
sum(is.na(SurvivalData$ComputedTaxonScientificName))
sum(SurvivalData$ComputedTaxonScientificName=="")


###Conservation status by signage
colnames(ConservationSignStatus)
head(ConservationSignStatus$ConservationSignStatusName)
dim(SurvivalData)
SurvivalData<-merge(SurvivalData, data.frame(ConservationSignStatusName=ConservationSignStatus$ConservationSignStatusName, 
                                             TaxonID=ConservationSignStatus$TaxonID), by="TaxonID", all.x=T)
dim(SurvivalData)
sum(!is.na(SurvivalData$ConservationSignStatusName))


###Merge with Family
SurvivalData<-merge(SurvivalData, data.frame(FamilyID=Family$FamilyID, FamilyName=Family$FamilyName), by="FamilyID", all.x=T)
dim(SurvivalData)
sum(is.na(SurvivalData$FamilyName))

###Merge with Haibt 
SurvivalData<-merge(SurvivalData, Habit,  by="HabitID", all.x=T)
dim(SurvivalData)
sum(is.na(SurvivalData$HabitID))
sum(is.na(SurvivalData$HabitName))

##Define life cylce durration 
table(SurvivalData$LifeCycleDurationID)
sum(is.na(SurvivalData$LifeCycleDurationID))
SurvivalData$LifeCycleDurationID[SurvivalData$LifeCycleDurationID==1]<-"Perennial"
SurvivalData$LifeCycleDurationID[SurvivalData$LifeCycleDurationID==2]<-"Annual"
SurvivalData$LifeCycleDurationID[SurvivalData$LifeCycleDurationID==3]<-"Biennial"
table(SurvivalData$LifeCycleDurationID)
colnames(SurvivalData)[colnames(SurvivalData) == "LifeCycleDurationID"] <- "LifeCycleDurationName"



###################################################################################################################################################################
#6. Export Dataframe
##################################################################################################################################################################
#save survival data
setwd("S:/HortandResearchedSharedProjects/Survival/Data/LCMS_2023")
write.table(file=paste("LCMSSurvialData_", format(Sys.time(), "%Y%b%d_%H%M%S"), ".txt"), SurvivalData, col.names =T, row.names = F)




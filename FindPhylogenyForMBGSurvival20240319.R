#############################################################################################
#############################################################################################
##
## TITLE: Create Phylogeny for Survival data pulled from LCMS.
##
## AUTHOR: Georgia Thomas
##
## INTRODUCTION
##
## This script is part of a study of survival at the botanical gardens.
## This script takes survival data pulled from the LCMS and matches names to a phylogeny for
## future analysis.
##
## DATA FILES REQUIRED
##
##
## 1. "LCMSSurvialData_[MOST CURRENT_DATE].txt" [Survival data create from LCMS with "FindSurvivalMoBot_[MOST CURRENT_DATE].R"]
## 2. "family_list_for_V.PhyloMaker2.csv" [Family List Used for matching to create phylogeny. Source: https://github.com/jinyizju/Supplementary-files-of-the-V.PhyloMaker2-paper]
## 3. "classification.csv" [World Flora Online Backbone downloaded and unziped 2024/03/07, v.2023.06	Jun. 25, 2023]
## 4. InfraspecificRank [table in the LCMS]
##
##
##
##
##
## CONTENTS
##
## 1.	Read files.
## 2. Match names with World Flora
## 3. Create Phylogeny.
##
#############################################################################################
#############################################################################################


#############################################################################################
# 1. Read data files.
#############################################################################################


##to install V.PhloMaker2
install.packages("devtools")
library(devtools)
devtools::install_github("jinyizju/V.PhyloMaker2")


#Preliminaries
#Load Packages
library(WorldFlora) #https://cran.r-project.org/web/packages/WorldFlora/index.html
library(V.PhyloMaker2) #https://www.sciencedirect.com/science/article/pii/S2468265922000580 #https://github.com/jinyizju/Supplementary-files-of-the-V.PhyloMaker2-paper
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
InfraspecificRank<-dbGetQuery(db_conn, 'SELECT * FROM InfraspecificRank')
dim(InfraspecificRank)
head(InfraspecificRank)




#set working directory
setwd("S:/HortandResearchedSharedProjects/Survival/Data/LCMS_2023")
SurvivalData<-read.table("LCMSSurvialData_ 2024Mar06_161942 .txt", header=T)


#set working directory
setwd("S:/HortandResearchedSharedProjects/Survival/Data/WFO_Backbone")
V.PHyloMaker2.Families<-read.table("family_list_for_V.PhyloMaker2.csv")
WFO.remember(WFO.file = "classification.csv", WFO.data = "WFO.data", WFO.pos = 1)


#############################################################################################
# 2. Match names with World Flora
#############################################################################################
#All names in the user’s species list that are considered as synonyms in the plant database should be replaced
# with their accepted names in the plant database.

##find infrapecific rank names
colnames(InfraspecificRank)
SurvivalData<-merge(SurvivalData, data.frame(InfraspecificRankID=InfraspecificRank$InfraspecificRankID, Infraspecific.rank=InfraspecificRank$Abbreviation), by="InfraspecificRankID", all.x=T)

#to keep our lives simple we are going to remove all hybrids from this study
#remove Genus Hybrids
#remove Species Hybrids 
colnames(SurvivalData)
dim(SurvivalData)
table(SurvivalData$GenusHybrid)
SurvivalData2<-SurvivalData[SurvivalData$GenusHybrid==F,]
dim(SurvivalData)
table(SurvivalData$SpeciesHybrid)
SurvivalData<-SurvivalData[SurvivalData$SpeciesHybrid==F,]
dim(SurvivalData)


##format data for matching spec.name = "spec.name", Genus = "Genus", Species = "Species",
#Infraspecific.rank = "Infraspecific.rank", Infraspecific = "Infraspecific"

##Don't understand why I couldn't make the WFO.match function work when data was split into its parts
#TaxonList<-unique(SurvivalData[, c("TaxonID", "Genus", "Species", "InfraspecificName", "Infraspecific.rank")])
#colnames(TaxonList)
#colnames(TaxonList)<-c("TaxonID", "Genus", "Species", "Infraspecific", "Infraspecific.rank")
#colnames(TaxonList)

TaxonList<-unique(SurvivalData[, c("TaxonID", "ComputedTaxonScientificName")])
colnames(TaxonList)<-c("TaxonID", "spec.name")


#Standardize plant names according to World Flora Online taxonomic backbone
TaxonListWFO<-WFO.match(spec.data = TaxonList[, c("spec.name")], WFO.data = WFO.data, Fuzzy=0)


###Choose just one accepted name for match.  
TaxonListWFO_One<-  WFO.one(WFO.result = TaxonListWFO, priority = "Accepted",
                            spec.name = NULL, Auth.dist = NULL, First.dist = NULL,
                            verbose = TRUE, counter = 1000)




WFOAccectedNamesWithTaxonID<-data.frame(cbind(TaxonList$TaxonID, TaxonListWFO_One))

setwd("S:/HortandResearchedSharedProjects/Survival/Data/LCMS_2023")
write.table(file=paste("WFOAccectedNamesWithTaxonID_", format(Sys.time(), "%Y%b%d_%H%M%S"), ".txt"), WFOAccectedNamesWithTaxonID, col.names =T, row.names = F)


#############################################################################################
# 3. Create Phylogeny.
#############################################################################################

##explore data prior to creating a phylogny
table(TaxonListWFO_One$Matched)
PhyTaxonList<-TaxonListWFO_One[TaxonListWFO_One$Matched==T,]
dim(PhyTaxonList)


#paper on V.PhyloMaker2

##Notes for prepartion of data for input into phylogony###
##format of fields needed when creating phylogony: species,genus,family,species.relative,genus.relative
#Because taxa with terminal branches in each mega-tree are species-level taxa (i.e. binomials), infraspecific taxa (e.g. subspecies, variety, and forma) in
#the user’s species list should be combined with their parental species.
#Duplicate names should be removed.
##data must be put in this order with rows names species,genus,family,species.relative,genus.relative
#species is formatted "Genus spices"
species<-paste(PhyTaxonList$genus, PhyTaxonList$specificEpithet, sep=" ")
PhyTaxonList<-cbind(PhyTaxonList, species)
PhyTaxonList<-unique(PhyTaxonList[,c("family", "genus", "species")])
species.relative<-rep(NA, length(PhyTaxonList$family))
genus.relative<-rep(NA, length(PhyTaxonList$family))
PhyTaxonList<-data.frame(cbind(PhyTaxonList$species, PhyTaxonList$genus, PhyTaxonList$family, species.relative, genus.relative))
colnames(PhyTaxonList)<-c("species","genus","family","species.relative","genus.relative")
#A hybrid sign in front of a genus name should be removed, and a hybrid species should be indicated with “X” (e.g. Abelia_X_grandiflora)



#Species in the user’s species list that do not have a family name in column ‘family’ will not be included in a phylogeny
#generated by V.PhyloMaker2.
PhyTaxonList[!(PhyTaxonList$family %in% V.PHyloMaker2.Families$V1),]
##fix the ones that are obvious
PhyTaxonList$family[PhyTaxonList$family=="Viburnaceae"]<-"Adoxaceae"
PhyTaxonList[!(PhyTaxonList$family %in% V.PHyloMaker2.Families$V1),]
table(PhyTaxonList[!(PhyTaxonList$family %in% V.PHyloMaker2.Families$V1),]$family)
##all of the families left are mosses or blank
##drop them, vasuclar plant phylogyny
PhyTaxonList<-PhyTaxonList[(PhyTaxonList$family %in% V.PHyloMaker2.Families$V1),]

##examin list and formatting one last time.  
dim(PhyTaxonList)
colnames(PhyTaxonList)

# generate a phylogeny for the species list
tree <- phylo.maker(sp.list = PhyTaxonList, tree = GBOTB.extended.TPL, nodes = nodes.info.1.TPL, scenarios = "S3")

#save files
setwd("S:/HortandResearchedSharedProjects/Survival/Data/LCMS_2023")
write.tree(tree$scenario.3, "Phy_LCMSSurvialData_2024Mar06_161942.tre")
##save table output of tree as a table
write.table(file=paste("TreeSpeciesList_", format(Sys.time(), "%Y%b%d_%H%M%S"), ".txt", sep=""), WFOAccectedNamesWithTaxonID, col.names =T, row.names = F)




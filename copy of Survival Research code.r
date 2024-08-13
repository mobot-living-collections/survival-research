

####################################################################################################
####################################################################################################
##
##              SURVIVAL ANALYSIS OF PLANTS GROWN OUTDOORS IN THE MISSOURI BOTANICAL GARDEN
##                                         FROM WILD COLLECTED PROPAGULES 
##
## PURPOSE OF THE SCRIPT
## This script fits Cox models to data on survival of plantings grown in the
## Missouri Botanical Garden from wild collected propagules.
##
## DATA FILES REQUIRED TO RUN THIS SCRIPT
## 1. File with survival data: "Appendix_C_MBGSurvivalAnalysisData.csv"
## 2. File with phylogeny: "Appendix_B_MGBSurvivialPhylogeny.tre"
##
## CONTENTS OF THIS SCRIPT
## 1. Preliminaries.
## 2. Prepare and examine variables and objects to fit Cox models.
## 3. Fit mixed effects Cox models.
## 4. Model comparison in terms of empirical support as measured by Akaike information criterion.
## 5. Graphically explore predictions, in terms of survial curves, for selected models.
## 6. Statistical significance and quality of estimates of the standard deviation of species
##    random effects in the best models.
## 7. Compare the survival of threatened and non-threatened ("Other") species taxa.
##
####################################################################################################
####################################################################################################


####################################################################################################
# 1. Preliminaries
####################################################################################################

#Load required packages.
library(coxme)
library(corrplot)
library(ape)

#Set working directory.
#setwd("C:/_transfer/Review/GeorgiaThomas/Data") #Ivan's directory for data
#setwd("C:/Users/georgia/Documents/mbg/Climate/PublicationData") #Georgia Home Directory 

#Read and examine survival data.
survival.data <- read.delim("Appendix_C_MBGSurvivalAnalysisData.csv", header=T, sep=",")
dim(survival.data)
head(survival.data)
summary(survival.data)

#Read and examine phylogeny.
ptree <- read.tree("Appendix_B_MGBSurvivialPhylogeny.tre") 
class(ptree)
attributes(ptree)
ptree$tip.label
length(ptree$tip.label) #410 species in the phylogeny
#The following two lines produce a useful figure when saved in pdf format:
#par(mar=c(5, 4, 4, 2) + 0.1) #default
par(mar=c(1,1,1,1))
plot.phylo(ptree, type="fan", font=4, cex=0.2, label.offset=1)

#Calculate the variance-covariance matrix representing the pruned phylogeny.
ptree.vcv <- vcv(ptree, corr=T)
dim(ptree.vcv)
ptree.vcv[1:5,1:5]
rownames(ptree.vcv)


####################################################################################################
# 2. Prepare and examine variables and objects to fit Cox models. 
####################################################################################################

################################################
# 2.1. Create a dummy variable indicating growth form.  
#################################################

#Create dummy variable for growth form, assigning a value of one to
#non-woody forms (forbs, herbs and graminoids), and a value of zero
#to woody forms (lianas, shrubs, subshrubs and trees).
table(survival.data$HabitGroup)
growth.form <- rep(0, nrow(survival.data))
growth.form[survival.data$HabitGroup == "Forb/Herb" | survival.data$HabitGroup == "Graminoid"] <- 1
summary(growth.form)
table(growth.form)

#Examine the taxa assigned to each growth form category.
unique(survival.data$Spec.taxa.tpl[growth.form==0])
length(unique(survival.data$Spec.taxa.tpl[growth.form==0])) # 98 woody taxa
unique(survival.data$Spec.taxa.tpl[growth.form==1])
length(unique(survival.data$Spec.taxa.tpl[growth.form==1])) # 312 non-woody taxa

#Plot the phylogeny for species showing growth form.
windows(8.5,11)
growth.form.color <- rep("brown", times=length(ptree$tip.label))
index.growth.form <- match(ptree$tip.label, survival.data$Spec.taxa.tpl)
growth.form.color[(growth.form)[index.growth.form]== 1] <- "green"
#par(mar=c(5, 4, 4, 2) + 0.1) #default
par(mar=c(1,1,1,1))
plot.phylo(ptree, type="fan", font=4, cex=0.2, label.offset=1, tip.color=growth.form.color)
legend("topleft", c("Woody", "Non-woody"), text.col=c("brown", "green"), cex=0.9)
dev.off()

################################################
# 2.2. Create and examine an object of class "Surv", the response variable in all models,
#      examine survival curves for woody and non-woody plantings and examine the
#      frequency of right censored and "event" plantings.  
#################################################

plant.surv <- Surv(survival.data$survival, survival.data$lastrecordedalive)
class(plant.surv)
dim(plant.surv)
attributes(plant.surv)
summary(plant.surv)

#Estimate and plot overall survival curve.
ps.fh <- survfit(plant.surv ~ 1, conf.type="log-log", type="fh")
summary(ps.fh)
ps.fh$time
ps.fh$n.risk
ps.fh$n.event
par(mar=c(5, 4, 4, 2) + 0.1) #default
plot(ps.fh, xlab="Days", ylab="Survival probability")

#Estimate and plot survival curves as a function of growth form.
ps.gf.fh <- survfit(plant.surv ~ growth.form, type="fh")
par(mar=c(5, 4, 4, 2) + 0.1) #default
plot(ps.gf.fh, col=c("brown", "green"), xlab="Days", ylab="Survival probability")
legend("topright", c("Woody", "Non-woody"), col=c("brown", "green"), cex=1, lwd=1)

#Number of right censored and "event" plantings. 
sum(plant.surv[,2]== 0) #right censored: 924
sum(plant.surv[,2]== 1) #"event": 260

#Number of right censored and "event" woody plantings.
sum(growth.form == 0 & plant.surv[,2]== 0) #right censored: 411
sum(growth.form == 0 & plant.surv[,2]== 1) #"event": 50

#Number of right censored and "event" non-woody plantings.
sum(growth.form == 1 & plant.surv[,2]== 0) #right censored: 513
sum(growth.form == 1 & plant.surv[,2]== 1) #"event": 210


####################################################################################################
# 3. Fit Cox models.
####################################################################################################

#Define three sets of climatic variables, corresponding to the three approaches to measure climatic
#differences between MBG and the distribution of plants in the wild (see Methods and Fig. 2):

#i) the difference between the climate at MBG and the collection locality for the propagules of each planting:
local.haveGBIF.var <- paste("survival.data$", c("difamt_local", "difmtcm_local", "difmtwm_local", "difap_local", "difpdm_local", "difpwm_local"), sep="")

#ii) the proportion of the climatic distribution of each species that was less extreme than MBG’s climate:
dif.GBIF.var <- paste("survival.data$", c("difamt_GBIF", "difmtcm_GBIF", "difmtwm_GBIF", "difap_GBIF", "difpdm_GBIF", "difpwm_GBIF"))

#iii) the difference between the climate at MBG and the median climate across the occurrences of a species: 
prop.GBIF.var <- paste("survival.data$", c("prop_amt_GBIF", "propabov_mtcm_GBIF", "propbel_mtwm_GBIF", "propbel_ap_GBIF", "propbel_pdm_GBIF", "propbel_pwm_GBIF"), sep="")

################################################
# 3.1. Fit Cox models with no random effects, these models are used to test the
#      statistical significance of random effects in section 6 of this script.
################################################

################################################
# 3.1.1. Fit models for set local.haveGBIF.var.
################################################

models.local.haveGBIF.var.nr <- as.list(rep(NA, times=63))
set.climatic.variables <- local.haveGBIF.var
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	for (j in 1:ncol(index))
	{
		model.formula <- (as.formula(paste("plant.surv ~", paste(c(set.climatic.variables[index[,j]], "growth.form"), collapse="+"))))
		models.local.haveGBIF.var.nr[[count]] <- tryCatch(coxph(model.formula, x=T), error = function(e) print(e))
		print(count)
		count <- count + 1
	}
}

#save results, after setting the appropriate working directory
save(list=c("models.local.haveGBIF.var.nr"), file=paste("models.local.haveGBIF.var.nr", ".RData", sep=""))

################################################
# 3.1.2. Fit models for set prop.GBIF.var.
################################################

models.prop.GBIF.var.nr <- as.list(rep(NA, times=63))
set.climatic.variables <- prop.GBIF.var
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	for (j in 1:ncol(index))
	{
		model.formula <- (as.formula(paste("plant.surv ~", paste(c(set.climatic.variables[index[,j]], "growth.form"), collapse="+"))))
		models.prop.GBIF.var.nr[[count]] <- tryCatch(coxph(model.formula, x=T), error = function(e) print(e))
		print(count)
		count <- count + 1
	}
}

#save results, after setting the appropriate working directory 
save(list=c("models.prop.GBIF.var.nr"), file=paste("models.prop.GBIF.var.nr", ".RData", sep=""))

################################################
# 3.1.3. Fit models for set dif.GBIF.var.
################################################

models.dif.GBIF.var.nr <- as.list(rep(NA, times=63))
set.climatic.variables <- dif.GBIF.var
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	for (j in 1:ncol(index))
	{
		model.formula <- (as.formula(paste("plant.surv ~", paste(c(set.climatic.variables[index[,j]], "growth.form"), collapse="+"))))
		models.dif.GBIF.var.nr[[count]] <- tryCatch(coxph(model.formula, x=T), error = function(e) print(e))
		print(count)
		count <- count + 1
	}
}

#save results, after setting the appropriate working directory 
save(list=c("models.dif.GBIF.var.nr"), file=paste("models.dif.GBIF.var.nr", ".RData", sep=""))

################################################
# 3.2. Fit mixed effects Cox models with species random effects.
################################################

################################################
# 3.2.1. Fit models for set local.haveGBIF.var.
################################################

models.local.haveGBIF.var.species <- as.list(rep(NA, times=63))
set.climatic.variables <- local.haveGBIF.var
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	for (j in 1:ncol(index))
	{
		model.formula <- (as.formula(paste("plant.surv ~", paste(c(set.climatic.variables[index[,j]], "growth.form", "(1 | survival.data$Spec.taxa.tpl)"), collapse="+"))))
		models.local.haveGBIF.var.species[[count]] <- tryCatch(coxme(model.formula, x=T), error = function(e) print(e))
		print(count)
		count <- count + 1
	}
}

#save results, after setting the appropriate working directory
save(list=c("models.local.haveGBIF.var.species"), file=paste("models.local.haveGBIF.var.species", ".RData", sep=""))

################################################
# 3.2.2. Fit models for set prop.GBIF.var.
################################################

models.prop.GBIF.var.species <- as.list(rep(NA, times=63))
set.climatic.variables <- prop.GBIF.var
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	for (j in 1:ncol(index))
	{
		model.formula <- (as.formula(paste("plant.surv ~", paste(c(set.climatic.variables[index[,j]], "growth.form", "(1 | survival.data$Spec.taxa.tpl)"), collapse="+"))))
		models.prop.GBIF.var.species[[count]] <- tryCatch(coxme(model.formula, x=T), error = function(e) print(e))
		print(count)
		count <- count + 1
	}
}

#Six models require initial vairance values different to default values:
models.prop.GBIF.var.species[c(20,32,45,47,57,61)]
#The following lines use explicit initial vairance values obtained from models
#with similar sets of explanatory variables:
#model 20:
model.formula <- plant.surv ~ survival.data$propabov_mtcm_GBIF + survival.data$propbel_mtwm_GBIF + survival.data$propbel_pdm_GBIF + survival.data$propbel_pwm_GBIF + growth.form + (1 | survival.data$Spec.taxa.tpl)
models.prop.GBIF.var.species[[c(20)]] <- coxme(model.formula, x=T, vinit=c(2.211632))
#model 32:
model.formula <- plant.surv ~ survival.data$prop_amt_GBIF + survival.data$propbel_pdm_GBIF + survival.data$propbel_pwm_GBIF + growth.form + (1 | survival.data$Spec.taxa.tpl)
models.prop.GBIF.var.species[[c(32)]] <- coxme(model.formula, x=T, vinit=c(1))
#model 45:
model.formula <- plant.surv ~ survival.data$prop_amt_GBIF + survival.data$propbel_ap_GBIF + growth.form + (1 | survival.data$Spec.taxa.tpl)
#models.prop.GBIF.var.species[[c(45)]] <- coxme(model.formula, x=T, vinit=c(2.329090))
models.prop.GBIF.var.species[[c(45)]] <- coxme(model.formula, x=T, vinit=c(0.5))
#model 47:
model.formula <- plant.surv ~ survival.data$prop_amt_GBIF + survival.data$propbel_pwm_GBIF + growth.form + (1 | survival.data$Spec.taxa.tpl)
#models.prop.GBIF.var.species[[c(47)]] <- coxme(model.formula, x=T, vinit=c(2.329090))
models.prop.GBIF.var.species[[c(47)]] <- coxme(model.formula, x=T, vinit=c(1.1))
#model 57:
model.formula <- plant.surv ~ survival.data$propbel_pdm_GBIF + survival.data$propbel_pwm_GBIF + growth.form + (1 | survival.data$Spec.taxa.tpl)
#models.prop.GBIF.var.species[[c(57)]] <- coxme(model.formula, x=T, vinit=c(2.329090))
models.prop.GBIF.var.species[[c(57)]] <- coxme(model.formula, x=T, vinit=c(1.1))
#model 61:
model.formula <- plant.surv ~ survival.data$propbel_ap_GBIF + growth.form + (1 | survival.data$Spec.taxa.tpl)
#models.prop.GBIF.var.species[[c(61)]] <- coxme(model.formula, x=T, vinit=c(2.329090))
models.prop.GBIF.var.species[[c(61)]] <- coxme(model.formula, x=T, vinit=c(1.1))

#save results, after setting the appropriate working directory
save(list=c("models.prop.GBIF.var.species"), file=paste("models.prop.GBIF.var.species", ".RData", sep=""))

################################################
# 3.2.3. Fit models for set dif.GBIF.var
################################################

models.dif.GBIF.var.species <- as.list(rep(NA, times=63))
set.climatic.variables <- dif.GBIF.var
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	for (j in 1:ncol(index))
	{
		model.formula <- (as.formula(paste("plant.surv ~", paste(c(set.climatic.variables[index[,j]], "growth.form", "(1 | survival.data$Spec.taxa.tpl)"), collapse="+"))))
		models.dif.GBIF.var.species[[count]] <- tryCatch(coxme(model.formula, x=T), error = function(e) print(e))
		print(count)
		count <- count + 1
	}
}

#save results, after setting the appropriate working directory
save(list=c("models.dif.GBIF.var.species"), file=paste("models.dif.GBIF.var.species", ".RData", sep=""))

################################################
# 3.3. Fit mixed effects Cox models with species random effects that are correlated
#      according to phylogenetic relationships
################################################


################################################
# 3.3.1. Fit models for set local.haveGBIF.var.
################################################

models.local.haveGBIF.var.species.cor <- as.list(rep(NA, times=63))
set.climatic.variables <- local.haveGBIF.var
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	for (j in 1:ncol(index))
	{
		model.formula <- (as.formula(paste("plant.surv ~", paste(c(set.climatic.variables[index[,j]], "growth.form", "(1 | survival.data$Spec.taxa.tpl)"), collapse="+"))))
		models.local.haveGBIF.var.species.cor[[count]] <- tryCatch(coxme(model.formula, x=T, varlist=coxmeMlist(ptree.vcv, rescale=F)), error = function(e) print(e))
		print(count)
		count <- count + 1
	}
}

#save results, after setting the appropriate working directory
save(list=c("models.local.haveGBIF.var.species.cor"), file=paste("models.local.haveGBIF.var.species.cor", ".RData", sep=""))

################################################
# 3.3.2. Fit models for set prop.GBIF.var
################################################

models.prop.GBIF.var.species.cor <- as.list(rep(NA, times=63))
set.climatic.variables <- prop.GBIF.var
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	for (j in 1:ncol(index))
	{
		model.formula <- (as.formula(paste("plant.surv ~", paste(c(set.climatic.variables[index[,j]], "growth.form", "(1 | survival.data$Spec.taxa.tpl)"), collapse="+"))))
		models.prop.GBIF.var.species.cor[[count]] <- tryCatch(coxme(model.formula, x=T, varlist=coxmeMlist(ptree.vcv, rescale=F)), error = function(e) print(e))
		print(count)
		count <- count + 1
	}
}

#save results, after setting the appropriate working directory
save(list=c("models.prop.GBIF.var.species.cor"), file=paste("models.prop.GBIF.var.species.cor", ".RData", sep=""))

################################################
# 3.3.3. Fit models for set dif.GBIF.var
################################################

models.dif.GBIF.var.species.cor <- as.list(rep(NA, times=63))
set.climatic.variables <- dif.GBIF.var
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	for (j in 1:ncol(index))
	{
		model.formula <- (as.formula(paste("plant.surv ~", paste(c(set.climatic.variables[index[,j]], "growth.form", "(1 | survival.data$Spec.taxa.tpl)"), collapse="+"))))
		models.dif.GBIF.var.species.cor[[count]] <- tryCatch(coxme(model.formula, x=T, varlist=coxmeMlist(ptree.vcv, rescale=F)), error = function(e) print(e))
		print(count)
		count <- count + 1
	}
}

#save results, after setting the appropriate working directory
save(list=c("models.dif.GBIF.var.species.cor"), file=paste("models.dif.GBIF.var.species.cor", ".RData", sep=""))


####################################################################################################
# 4. Model comparison in terms of empirical support as measured by Akaike information criterion (AIC). 
####################################################################################################

################################################
# 4.1. If needed, load lists with model results.
################################################

#set working directory with model results
#setwd("C:/_transfer/Review/GeorgiaThomas/CoxModelsResults") #Ivan's directory for model results
#setwd("C:/Users/georgia/Documents/mbg/Climate/PublicationData") 

#load models with no random effects
load("models.local.haveGBIF.var.nr.RData")
load("models.prop.GBIF.var.nr.RData")
load("models.dif.GBIF.var.nr.RData")

#load models with species as random effects
load("models.local.haveGBIF.var.species.RData")
load("models.prop.GBIF.var.species.RData")
load("models.dif.GBIF.var.species.RData")

#load models with species as correlated random effects, according to phylogenetic relationships
load("models.local.haveGBIF.var.species.cor.RData")
load("models.prop.GBIF.var.species.cor.RData")
load("models.dif.GBIF.var.species.cor.RData")

################################################
# 4.1. Define function to obtain AIC values.
################################################

#Create a function to obtain AIC values from each model,
#as calculated here, larger AIC values indicate stronger
#empirical support.
ExtractModelAIC <- function(x){
  loglik <- x$loglik + c(0, 0, x$penalty)
  chi1 <- 2*diff(loglik[1:2]) 
  chi2 <- 2*diff(loglik[c(1,3)])
  c(chi1 - 2*x$df[1], chi2 - 2*x$df[2])
}

################################################
# 4.2. Obtain and plot model AIC values.
################################################

################################################
# 4.2.1. Obtain AIC values. 
################################################

#Obtain AIC values for different sets of models.
AIC.models.local.haveGBIF.species <- lapply((models.local.haveGBIF.var.species), ExtractModelAIC)
AIC.models.local.haveGBIF.species <- unlist(lapply(AIC.models.local.haveGBIF.species, function(x) x[1]))
AIC.models.prop.GBIF.species <- lapply((models.prop.GBIF.var.species), ExtractModelAIC)
AIC.models.prop.GBIF.species <- unlist(lapply(AIC.models.prop.GBIF.species, function(x) x[1]))
AIC.models.dif.GBIF.species <- lapply((models.dif.GBIF.var.species), ExtractModelAIC)
AIC.models.dif.GBIF.species <- unlist(lapply(AIC.models.dif.GBIF.species, function(x) x[1]))
AIC.models.local.haveGBIF.species.cor <- lapply((models.local.haveGBIF.var.species.cor), ExtractModelAIC)
AIC.models.local.haveGBIF.species.cor <- unlist(lapply(AIC.models.local.haveGBIF.species.cor, function(x) x[1]))
AIC.models.prop.GBIF.species.cor <- lapply((models.prop.GBIF.var.species.cor), ExtractModelAIC)
AIC.models.prop.GBIF.species.cor <- unlist(lapply(AIC.models.prop.GBIF.species.cor, function(x) x[1]))
AIC.models.dif.GBIF.species.cor <- lapply((models.dif.GBIF.var.species.cor), ExtractModelAIC)
AIC.models.dif.GBIF.species.cor <- unlist(lapply(AIC.models.dif.GBIF.species.cor, function(x) x[1]))

#Place all AIC values in a single object.
AIC.best.model <- max(c(AIC.models.local.haveGBIF.species, AIC.models.prop.GBIF.species, AIC.models.dif.GBIF.species,
	AIC.models.local.haveGBIF.species.cor, AIC.models.prop.GBIF.species.cor, AIC.models.dif.GBIF.species.cor), na.rm=T) 

#Obtain AIC differences among models.
Delta.AIC <- c(AIC.models.local.haveGBIF.species, AIC.models.prop.GBIF.species, AIC.models.dif.GBIF.species,
	AIC.models.local.haveGBIF.species.cor, AIC.models.prop.GBIF.species.cor, AIC.models.dif.GBIF.species.cor) - AIC.best.model
summary(Delta.AIC)

#Make a list with all model results.
all.models.GBIF <- c(models.local.haveGBIF.var.species, models.prop.GBIF.var.species, models.dif.GBIF.var.species,
	models.local.haveGBIF.var.species.cor, models.prop.GBIF.var.species.cor, models.dif.GBIF.var.species.cor)

#Find the best model.
which(Delta.AIC>=0)
all.models.GBIF[which(Delta.AIC>=0)]

#Find the models with highest empirical support.
which(Delta.AIC >= -3)
all.models.GBIF[which(Delta.AIC >= -3)]

################################################
# 4.2.2. Enumerate the climatic variables in each model.
################################################

#Determine the number of combiations of climatic variables
#and the number of variables in each combination 
number.size.var.comb <- matrix(NA, nrow=6, ncol=2)
colnames(number.size.var.comb) <- c("number.of.combinations", "number.of.variables.in.each.combination")
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	number.size.var.comb[count,1] <- ncol(index)
	number.size.var.comb[count,2] <- nrow(index)
	count <- count + 1
}
number.size.var.comb

#Create a matrix showing the climatic variables included
#in each model (i.e., in each combination of climatic variables).
cum.number.of.combinations <- c(0,cumsum(number.size.var.comb[,1]))
cum.number.of.variable.rows <- cumsum(number.size.var.comb[,1] * number.size.var.comb[,2])
var.comb <- matrix(NA, nrow=cum.number.of.variable.rows[length(cum.number.of.variable.rows)], ncol=2)
colnames(var.comb) <- c("Combination (model)", "Variables")
count <- 1
for(i in 6:1)
{
	index <- combn(6,i)
	var.comb[which(is.na(var.comb[,1]))[1]:cum.number.of.variable.rows[count],1] <- rep((cum.number.of.combinations[count]+1):cum.number.of.combinations[count+1], each=nrow(index))
	var.comb[which(is.na(var.comb[,2]))[1]:cum.number.of.variable.rows[count],2] <- as.vector(index)
	count <- count + 1
}
var.comb

################################################
# 4.2.3. Plot AIC values and the corresponding climatic varialbes in each model.
################################################

#Plot Delta AIC Values.
windows(7,7)
plot(1:63, Delta.AIC[1:63], ylim=range(Delta.AIC), col="blue",
	xlab="", ylab=expression(paste(Delta, " AIC")), type="n", pch=19, bty="n", xaxt="n", yaxt="n")
#abline(v=seq(5,63,5), col="gray")
rect(cum.number.of.combinations[1]+0.5, range(Delta.AIC)[1], cum.number.of.combinations[2]+0.5, range(Delta.AIC)[2], col="gray80", border="gray80")
rect(cum.number.of.combinations[3]+0.5, range(Delta.AIC)[1], cum.number.of.combinations[4]+0.5, range(Delta.AIC)[2], col="gray80", border="gray80")
rect(cum.number.of.combinations[5]+0.5, range(Delta.AIC)[1], cum.number.of.combinations[6]+0.5, range(Delta.AIC)[2], col="gray80", border="gray80")
points(1:63, Delta.AIC[1:63], col="blue", type="o", pch=19)
points(1:63, Delta.AIC[64:(63*2)], col="red", type="o", pch=19, cex=0.5)
points(1:63, Delta.AIC[127:(63*3)], col="orange", type="o")
points(1:63, Delta.AIC[190:(63*4)], col="blue", type="o", pch=25, bg="blue")
points(1:63, Delta.AIC[253:(63*5)], col="red", type="o", pch=25, cex=0.5, bg="red")
points(1:63, Delta.AIC[316:(63*6)], col="orange", type="o", pch=25)
axis(1, at=1:63, labels=F)
axis(1, at=seq(5,63,5), labels=T, tcl=-0.8)
axis(2, at=seq(0, -120, -20), labels=seq(0, 120, 20))
axis(3, at=1:63, labels=F)
axis(3, at=seq(5,63,5), labels=T, tcl=-0.8)
mtext(side=c(1,3), "Models", line=3)
#show legend in separate plot
plot(1:63, Delta.AIC[1:63], ylim=c(-10,0),
	xlab="", ylab="", type="n", pch=19, bty="n", xaxt="n", yaxt="n")
legend(40, -0.1, c("Local (s)", "Median (s)", "Proportion (s)", "Local (phy)", "Median (phy)", "Proportion (phy)"),
	col=c("blue", "orange", "red"), pch=c(19, 21, 19, 25, 25, 25), pt.bg=c(NA, NA, NA, "blue", NA, "red"), pt.cex=c(1,1,0.5), lty=1, bg="white")

#Graphically explore varaiables in models
color.models <- rep("gray60", times=nrow(var.comb))
color.models[!is.na(match(var.comb[,1], which(Delta.AIC >= -3)))] <- "black"
point.size.models <- rep(0.5, times=nrow(var.comb))
point.size.models[!is.na(match(var.comb[,1], which(Delta.AIC >= -3)))] <- 1
windows(7,7)
plot(var.comb, type="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0.7,6.3))
rect(cum.number.of.combinations[1]+0.5, 0, cum.number.of.combinations[2]+0.5, 7, col="gray80", border="gray80")
rect(cum.number.of.combinations[3]+0.5, 0, cum.number.of.combinations[4]+0.5, 7, col="gray80", border="gray80")
rect(cum.number.of.combinations[5]+0.5, 0, cum.number.of.combinations[6]+0.5, 7, col="gray80", border="gray80")
par(new=T)
plot(var.comb, type="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0.7,6.3))
#abline(v=cum.number.of.combinations+0.5, col="gray")
axis(1, at=1:63, labels=F)
axis(1, at=seq(5,63,5), labels=T, tcl=-0.8)
#axis(3, at=1:63, labels=F)
#axis(3, at=seq(5,63,5), labels=F, tcl=-0.8)
axis(3, at=cum.number.of.combinations+0.5, labels=F, col="gray60", line=0.5)
axis(3, at=c(1,4.2,14.5,32.5,50,60.5), labels=6:1, tcl=0, lwd=0, col.axis="gray60", line=0.2)
axis(2, at=1:6, 
     labels=c(expression(paste(Delta, " mat")), expression(paste(Delta, " mtcm")), expression(paste(Delta, " mtwm")),
     expression(paste(Delta, " map")), expression(paste(Delta, " pdm")), expression(paste(Delta, " pwm"))), las=2)
#points(var.comb[,1], var.comb[,2], pch=19, cex=0.5, bty="o", col="gray60")
points(var.comb[,1], var.comb[,2], pch=19, cex=point.size.models, bty="o", col=color.models)
mtext(side=1, "Models", line=3)
abline(v=which(Delta.AIC >= -3), lty=3)


####################################################################################################
# 5.  Graphically explore predictions, in terms of survival curves, for selected models.
####################################################################################################

################################################
# 5.1. Select and examine a model to explore graphically. 
################################################

#best model
which(Delta.AIC>=0)
all.models.GBIF[[which(Delta.AIC>=0)]]
focal.model <- all.models.GBIF[[which(Delta.AIC>=0)]]

#other models with high empirical support
which(Delta.AIC >= -3)
Delta.AIC[Delta.AIC >= -3]
all.models.GBIF[which(Delta.AIC >= -3)]
#Delta.AIC[198]
#focal.model <- all.models.GBIF[[198]]

summary(focal.model)
focal.model$coef #fixed coefficients
focal.model$frail #random effects
#focal.model$frail$'survival.data.Taxon/survival.data.AccessionID'

################################################
# 5.2. Estimate the base hazard function, cumulative base hazard function and base survival function,
  and plot the base survival function (section 5.5, page 64 in Moore, 2016, Applied Survival Analysis Using R).
################################################

#Estimate and plot overall survival curve for the focal model.
focal.model.fh <- survfit(focal.model$y ~ 1, conf.type="log-log", type="fh")
summary(focal.model.fh)
focal.model.fh$time
focal.model.fh$n.risk
focal.model.fh$n.event
plot(focal.model.fh)

#Estimate the base hazard function.
o <- order(focal.model$y)
#model.1$y[o,1]
#model.1$x[o,]
h0 <- rep(NA, length(focal.model.fh$time))
for (i in 1:length(focal.model.fh$time))
{
  h0[i] <- focal.model.fh$n.event[i]/sum(exp((focal.model$x[o,])[focal.model$y[o,1]>=focal.model.fh$time[i],]%*%focal.model$coef))
}

#Estimate cumulative base hazard function.
H0 <- cumsum(h0)

#Estimate base survival function.
S0 <- exp(-H0)

#Plot the base survival function. 
S0.col <- "blue" #define the color of the survival curve
plot(focal.model.fh$time, S0, type="l", xlab="Time in days", ylab="Survival probability",
	cex.axis=1.2, cex.lab=1.2, bty="n", xlim=c(0,max(focal.model.fh$time)), ylim=c(0,1), col=S0.col)
axis(1, at=focal.model.fh$time, labels=F, tck=0.01) #ticks indicating data points

################################################
# 5.3. Plot the predicted survival function for different values of explanatory variables,
       comparing it to the base survival function.
################################################

#Select the model coefficients of interest.
focal.coef <- focal.model$coef

#Find maximum value of each explanatory variable of interest.
#max(survival.data$difmtcm_local, na.rm = T)
#max(survival.data$difmtwm_local, na.rm = T)
#max(survival.data$difpwm_local, na.rm = T)
max(survival.data$difmtcm_local, na.rm = T)
max(survival.data$difmtwm_local, na.rm = T)
max(survival.data$difpdm_local, na.rm = T)

#Define the log hazard ratio by entering particular values of explanatory variables,
#keeping in mind extrapolation will be incurred if the maxima obtained in the previous
#block of code are exceeded.
difamt_local <- 0
difmtcm_local <- 2
difmtwm_local <- 0
difpdm_local <- 0
gf_value <- 1 #growth form value: a value of zero produces predictions for woody plants, and a value of one for non-woody plants (see section 2.1.)
log.hazard.ratio <- sum(c(difamt_local, difmtcm_local,difmtwm_local,difpdm_local, gf_value) * focal.coef) #calculate the log hazard ratio

#Alternatively, the log hazard ratio may be defined directly, with no reference to explanatory variables;
#this may be useful to explore how log hazard ratios relate to predicted survival curves.
#log.hazard.ratio <- 7 #enter an arbitrary log hazard ratio

#Calculate the survival curve corresponding to the hazard ratio defined above.
focal.model.valofInterst <- S0^exp(log.hazard.ratio) 

#Plot the base survival function and add the survival curve
#corresponding to the log hazard ratio defined above.
S0.col <- "blue" #define the color of the survival curve
plot(focal.model.fh$time, S0, type="l", xlab="Time in days", ylab="Survival probability",
	cex.axis=1.2, cex.lab=1.2, bty="n", xlim=c(0,max(focal.model.fh$time)), ylim=c(0,1), col=S0.col)
axis(1, at=focal.model.fh$time, labels=F, tck=0.01) #ticks indicating data points
#Add the survival curve corresponding to the log hazard ratio defined above. 
lhr.col <- "red" #define the color of the survival curve
points(focal.model.fh$time, focal.model.valofInterst, type="l", col=lhr.col)

###############################################################
# 5.4. Plot the survival curve for a particular species, given the log hazard ratio defined above
#      in section 5.3, comparing it to the base survival function.
##############################################################

#Choose a species of interest from the following set of species included in the analysis:
names(focal.model$frail$survival.data.Spec.taxa.tpl)
species <- "Adiantum_pedatum"

#Obtain the value of the random effect for the single species selected above.
randomeffect <- focal.model$frail$survival.data.Spec.taxa.tpl[species]

#Calculate the survival curve for the single species selected above
#and the log hazard ratio defined in section 5.3.
focal.model.species <- S0^exp(log.hazard.ratio + 1*randomeffect)

#Plot the base survival function and add the survival curve
#corresponding to the species selected above and the
#log hazard ratio defined in section 5.3.
S0.col <- "blue" #define the color of the survival curve
plot(focal.model.fh$time, S0, type="l", xlab="Time in days", ylab="Survival probability",
	cex.axis=1.2, cex.lab=1.2, bty="n", xlim=c(0,max(focal.model.fh$time)), ylim=c(0,1), col=S0.col)
axis(1, at=focal.model.fh$time, labels=F, tck=0.01) #ticks indicating data points
#Add the survival curve corresponding to the species and log hazard ratio defined above. 
lhr.col <- "red" #define the color of the survival curve
points(focal.model.fh$time, focal.model.species, type="l", col=lhr.col)


####################################################################################################
# 6. Statistical significance and quality of estimates of the standard deviation of species
     random effects in the best models (see Appendix E).
####################################################################################################

################################################
# 6.1. Monte-Carlo simulations to test the statistical significance of the estimates of the
#      standard deviation of the species random effects in the best models (see Appendix E).
################################################

################################################
# 6.1.1. Model 2
################################################

#This is Model 2:
models.local.haveGBIF.var.species[2]

#Define model formula.
model.formula <- plant.surv ~ survival.data$difamt_local + survival.data$difmtcm_local + 
    survival.data$difmtwm_local + survival.data$difap_local + survival.data$difpdm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)

#Fit model using Monte-Carlo simulations to refine the estimate of the integrated partial
#log-likelihood.
model.to.examine <- coxme(model.formula, x=T, refine.n=100000, refine.detail=T)
#Examine the correction that needs to be added to the integrated partial log-likelihood
#of the model, and the standard deviation of the correction.
model.to.examine$refine
#Add the correction to obtain a refined estimate of the integrated partial log-likelihood
#of the model.
Loglik.model.to.examine <- model.to.examine$loglik[2] + model.to.examine$refine[1]
Loglik.model.to.examine

#Test the statistical significance of species random effects by comparing the integrated
#partial log-likelihood of the model to the log-likelihood of a model with the same fixed
#effects but without random (fitted in section 3.1., above).
pchisq(2*(Loglik.model.to.examine - models.local.haveGBIF.var.nr[[2]]$loglik[2]), 1, lower.tail=F) #p-value

################################################
# 6.1.2. Model 4
################################################

#This is Model 4:
models.local.haveGBIF.var.species[4]

#Define model formula.
model.formula <- plant.surv ~ survival.data$difamt_local + survival.data$difmtcm_local + 
    survival.data$difmtwm_local + survival.data$difpdm_local + survival.data$difpwm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)

#Fit model using Monte-Carlo simulations to refine the estimate of the integrated partial
#log-likelihood.
model.to.examine <- coxme(model.formula, x=T, refine.n=100000, refine.detail=T)
#Examine the correction that needs to be added to the integrated partial log-likelihood
#of the model, and the standard deviation of the correction.
model.to.examine$refine
#Add the correction to obtain a refined estimate of the integrated partial log-likelihood
#of the model.
Loglik.model.to.examine <- model.to.examine$loglik[2] + model.to.examine$refine[1]
Loglik.model.to.examine

#Test the statistical significance of species random effects by comparing the integrated
#partial log-likelihood of the model to the log-likelihood of a model with the same fixed
#effects but without random (fitted in section 3.1., above).
pchisq(2*(Loglik.model.to.examine - models.local.haveGBIF.var.nr[[4]]$loglik[2]), 1, lower.tail=F) #p-value

################################################
# 6.1.3. Model 8
################################################

#This is Model 8:
models.local.haveGBIF.var.species[8]

#Define model formula.
model.formula <- plant.surv ~ survival.data$difamt_local + survival.data$difmtcm_local + 
    survival.data$difmtwm_local + survival.data$difap_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)

#Fit model using Monte-Carlo simulations to refine the estimate of the integrated partial
#log-likelihood.
model.to.examine <- coxme(model.formula, x=T, refine.n=100000, refine.detail=T)
#Examine the correction that needs to be added to the integrated partial log-likelihood
#of the model, and the standard deviation of the correction.
model.to.examine$refine
#Add the correction to obtain a refined estimate of the integrated partial log-likelihood
#of the model.
Loglik.model.to.examine <- model.to.examine$loglik[2] + model.to.examine$refine[1]
Loglik.model.to.examine

#Test the statistical significance of species random effects by comparing the integrated
#partial log-likelihood of the model to the log-likelihood of a model with the same fixed
#effects but without random (fitted in section 3.1., above).
pchisq(2*(Loglik.model.to.examine - models.local.haveGBIF.var.nr[[8]]$loglik[2]), 1, lower.tail=F) #p-value

################################################
# 6.1.4. Model 9
################################################

#This is Model 9:
models.local.haveGBIF.var.species[9]

#Define model formula.
model.formula <- plant.surv ~ survival.data$difamt_local + survival.data$difmtcm_local + 
    survival.data$difmtwm_local + survival.data$difpdm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)

#Fit model using Monte-Carlo simulations to refine the estimate of the integrated partial
#log-likelihood.
model.to.examine <- coxme(model.formula, x=T, refine.n=100000, refine.detail=T)
#Examine the correction that needs to be added to the integrated partial log-likelihood
#of the model, and the standard deviation of the correction.
model.to.examine$refine
#Add the correction to obtain a refined estimate of the integrated partial log-likelihood
#of the model.
Loglik.model.to.examine <- model.to.examine$loglik[2] + model.to.examine$refine[1]
Loglik.model.to.examine

#Test the statistical significance of species random effects by comparing the integrated
#partial log-likelihood of the model to the log-likelihood of a model with the same fixed
#effects but without random (fitted in section 3.1., above).
pchisq(2*(Loglik.model.to.examine - models.local.haveGBIF.var.nr[[9]]$loglik[2]), 1, lower.tail=F) #p-value

################################################
# 6.1.5. Model 18
################################################

#This is Model 18:
models.local.haveGBIF.var.species[18]

#Define model formula.
model.formula <- plant.surv ~ survival.data$difmtcm_local + survival.data$difmtwm_local +
    survival.data$difap_local + survival.data$difpdm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)

#Fit model using Monte-Carlo simulations to refine the estimate of the integrated partial
#log-likelihood.
model.to.examine <- coxme(model.formula, x=T, refine.n=100000, refine.detail=T)
#Examine the correction that needs to be added to the integrated partial log-likelihood
#of the model, and the standard deviation of the correction.
model.to.examine$refine
#Add the correction to obtain a refined estimate of the integrated partial log-likelihood
#of the model.
Loglik.model.to.examine <- model.to.examine$loglik[2] + model.to.examine$refine[1]
Loglik.model.to.examine

#Test the statistical significance of species random effects by comparing the integrated
#partial log-likelihood of the model to the log-likelihood of a model with the same fixed
#effects but without random (fitted in section 3.1., above).
pchisq(2*(Loglik.model.to.examine - models.local.haveGBIF.var.nr[[18]]$loglik[2]), 1, lower.tail=F) #p-value

################################################
# 6.1.6. Model 20
################################################

#This is Model 20:
models.local.haveGBIF.var.species[20]

#Define model formula.
model.formula <- plant.surv ~ survival.data$difmtcm_local + survival.data$difmtwm_local +
    survival.data$difpdm_local + survival.data$difpwm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)

#Fit model using Monte-Carlo simulations to refine the estimate of the integrated partial
#log-likelihood.
model.to.examine <- coxme(model.formula, x=T, refine.n=100000, refine.detail=T)
#Examine the correction that needs to be added to the integrated partial log-likelihood
#of the model, and the standard deviation of the correction.
model.to.examine$refine
#Add the correction to obtain a refined estimate of the integrated partial log-likelihood
#of the model.
Loglik.model.to.examine <- model.to.examine$loglik[2] + model.to.examine$refine[1]
Loglik.model.to.examine

#Test the statistical significance of species random effects by comparing the integrated
#partial log-likelihood of the model to the log-likelihood of a model with the same fixed
#effects but without random (fitted in section 3.1., above).
pchisq(2*(Loglik.model.to.examine - models.local.haveGBIF.var.nr[[20]]$loglik[2]), 1, lower.tail=F) #p-value

################################################
# 6.1.7. Model 33
################################################

#This is Model 33:
models.local.haveGBIF.var.species[33]

#Define model formula.
model.formula <- plant.surv ~ survival.data$difmtcm_local + survival.data$difmtwm_local +
    survival.data$difap_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)

#Fit model using Monte-Carlo simulations to refine the estimate of the integrated partial
#log-likelihood.
model.to.examine <- coxme(model.formula, x=T, refine.n=100000, refine.detail=T)
#Examine the correction that needs to be added to the integrated partial log-likelihood
#of the model, and the standard deviation of the correction.
model.to.examine$refine
#Add the correction to obtain a refined estimate of the integrated partial log-likelihood
#of the model.
Loglik.model.to.examine <- model.to.examine$loglik[2] + model.to.examine$refine[1]
Loglik.model.to.examine

#Test the statistical significance of species random effects by comparing the integrated
#partial log-likelihood of the model to the log-likelihood of a model with the same fixed
#effects but without random (fitted in section 3.1., above).
pchisq(2*(Loglik.model.to.examine - models.local.haveGBIF.var.nr[[33]]$loglik[2]), 1, lower.tail=F) #p-value

################################################
# 6.1.8. Model 34
################################################

#This is Model 34:
models.local.haveGBIF.var.species[34]

#Define model formula.
model.formula <- plant.surv ~ survival.data$difmtcm_local + survival.data$difmtwm_local +
    survival.data$difpdm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)

#Fit model using Monte-Carlo simulations to refine the estimate of the integrated partial
#log-likelihood.
model.to.examine <- coxme(model.formula, x=T, refine.n=100000, refine.detail=T)
#Examine the correction that needs to be added to the integrated partial log-likelihood
#of the model, and the standard deviation of the correction.
model.to.examine$refine
#Add the correction to obtain a refined estimate of the integrated partial log-likelihood
#of the model.
Loglik.model.to.examine <- model.to.examine$loglik[2] + model.to.examine$refine[1]
Loglik.model.to.examine

#Test the statistical significance of species random effects by comparing the integrated
#partial log-likelihood of the model to the log-likelihood of a model with the same fixed
#effects but without random (fitted in section 3.1., above).
pchisq(2*(Loglik.model.to.examine - models.local.haveGBIF.var.nr[[34]]$loglik[2]), 1, lower.tail=F) #p-value

################################################
# 6.2. Monte-Carlo simulations to examine the quality of estimates of the standard deviation
#      of species random effects in the best models (see Appendix E).
################################################

#Define the model formula for one of the best models (i.e., choose one of the models with
#highest empirical support, listed below):
#for model 2:
model.formula <- plant.surv ~ survival.data$difamt_local + survival.data$difmtcm_local + 
    survival.data$difmtwm_local + survival.data$difap_local + survival.data$difpdm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)
#for model 4:
model.formula <- plant.surv ~ survival.data$difamt_local + survival.data$difmtcm_local + 
    survival.data$difmtwm_local + survival.data$difpdm_local + survival.data$difpwm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)
#for model 8:
model.formula <- plant.surv ~ survival.data$difamt_local + survival.data$difmtcm_local + 
    survival.data$difmtwm_local + survival.data$difap_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)
#for model 9:
model.formula <- plant.surv ~ survival.data$difamt_local + survival.data$difmtcm_local + 
    survival.data$difmtwm_local + survival.data$difpdm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)
#for model 18:
model.formula <- plant.surv ~ survival.data$difmtcm_local + survival.data$difmtwm_local +
    survival.data$difap_local + survival.data$difpdm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)
#for model 20:
model.formula <- plant.surv ~ survival.data$difmtcm_local + survival.data$difmtwm_local +
    survival.data$difpdm_local + survival.data$difpwm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)
#for model 33:
model.formula <- plant.surv ~ survival.data$difmtcm_local + survival.data$difmtwm_local +
    survival.data$difap_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)
#for model 34:
model.formula <- plant.surv ~ survival.data$difmtcm_local + survival.data$difmtwm_local +
    survival.data$difpdm_local + 
    growth.form + (1 | survival.data$Spec.taxa.tpl)

#Once a model formula is defined, calculate the integrated partial log-likelihood (IPL) using
#the Laplace approximation, and the respective correction based on Monte-Carlo simulations,
#for several fixed values of the standard deviation of the species random effects, nearby the
#point estimate in the model. 
pe.ss <- as.numeric(VarCorr(model.to.examine))^0.5 #point estimate of standard deviation of species random effects
pe.ss 
ss <- seq(pe.ss - 0.6, pe.ss + 0.6, 0.05)
tmat <- matrix(0, nrow=length(ss), ncol=3)
for (i in 1:length(ss)){
	model.fixed.ss <- coxme(model.formula, x=T, vfixed=ss[i]^2, refine.n=100000)
	tmat[i,] <- c(diff(model.fixed.ss$loglik[1:2]), model.fixed.ss$refine)
}

#Plot the results.
temp1 <- tmat[,1] + tmat[,2] #corrected integrated partial log-likelihood (IPL)
temp2 <- tmat[,1] + tmat[,2] + cbind(-2*tmat[,3], 2*tmat[,3]) # 0.95 confidence interval for the IPL 
matplot(ss, cbind(tmat[,1], temp1), pch=c(17, 21), col=c("blue", "red"),
	ylim=range(tmat[,1], temp2),
	#ylim=range(tmat[tmat[,1]>0,1], temp2[temp2>0]),
	xlab="Standard deviation of species random effects",
	ylab="Log-likelihood (IPL - Null)")
#abline(v=pe.ss, lty=3)
segments(ss, temp2[,1], ss, temp2[,2], lty=1, col="red")
lines(smooth.spline(ss, temp1, df=5), col="red")
#lines(smooth.spline(ss[tmat[,1]>0], temp1[tmat[,1]>0], df=5), col="red")
spline.data.temp1 <- smooth.spline(ss, temp1, df=5)
abline(h= diff(model.to.examine$loglik[1:2]) - qchisq(.95, 1)/2, lty=2, col="blue")
#abline(v=ss[which.max(temp1)], lty=3, col="red")
lines(smooth.spline(ss, tmat[,1], df=5), col="blue")
#lines(smooth.spline(ss[tmat[,1]>0], tmat[tmat[,1]>0,1], df=5), col="blue")
#abline(h= max(temp1) - qchisq(.95, 1)/2, lty=3, col="red")
abline(h= max(spline.data.temp1$y) - qchisq(.95, 1)/2, lty=3, col="red")
legend("bottomright", title="Model 18", c("Laplace", "Monte-Carlo"), pch=c(17,21), lty=1, col=c("blue", "red"), cex=1)


####################################################################################################
# 7. Compare the survival of threatened and non-threatened ("Other") species taxa (see Appendix F).
####################################################################################################

################################################
# 7.1. Summarize the conservation status information per species taxa.
################################################

status.taxon <- table(survival.data$Conservation.Concern, survival.data$Spec.taxa.tpl)
summary(colSums(status.taxon>0)) #make sure all species taxa have only one conservation status

#distribution of species taxa among conservation status categories
par(mar=c(5, 4, 4, 2) + 0.1) #default
barplot(rowSums(status.taxon>0), ylab="Species", names.arg=c("CR", "EN", "Other", "VU"))
text(x=c(0.7,1.9,3.1,4.3), y=rowSums(status.taxon>0)+c(10,10,-20,10), labels=rowSums(status.taxon>0))

#examine critically endangered species taxa
status.taxon[1,(status.taxon>0)[1,]==T]
names(status.taxon[1,(status.taxon>0)[1,]==T])

#examine endangered species taxa
status.taxon[2,(status.taxon>0)[2,]==T]
names(status.taxon[2,(status.taxon>0)[2,]==T])

#examine vulnerable species taxa
status.taxon[4,(status.taxon>0)[4,]==T]
names(status.taxon[4,(status.taxon>0)[4,]==T])

#examine "other" species taxa
status.taxon[3,(status.taxon>0)[3,]==T]
names(status.taxon[3,(status.taxon>0)[3,]==T])

################################################
# 7.2. Group species random effects by conservation status, according to one of the best models (see Appendix F).
################################################

#Select one of the best models, by running only one the the lines of code below:
#focal.model <- models.local.haveGBIF.var.species[[2]]
#focal.model <- models.local.haveGBIF.var.species[[4]]
#focal.model <- models.local.haveGBIF.var.species[[8]]
focal.model <- models.local.haveGBIF.var.species[[9]]
#focal.model <- models.local.haveGBIF.var.species[[18]]
#focal.model <- models.local.haveGBIF.var.species[[20]]
#focal.model <- models.local.haveGBIF.var.species[[33]]
#focal.model <- models.local.haveGBIF.var.species[[34]]

#obtain random effects for critically endangered species taxa
index.CR <- match(names(status.taxon[1,(status.taxon>0)[1,]==T]), names(focal.model$frail$survival.data.Spec.taxa.tpl))
focal.model$frail$survival.data.Spec.taxa.tpl[index.CR]
status.taxon[1,(status.taxon>0)[1,]==T]

#obtain random effects for endangered species taxa
index.E <- match(names(status.taxon[2,(status.taxon>0)[2,]==T]), names(focal.model$frail$survival.data.Spec.taxa.tpl))
focal.model$frail$survival.data.Spec.taxa.tpl[index.E]
status.taxon[2,(status.taxon>0)[2,]==T]

#obtain random effects for vulnerable species taxa
index.V <- match(names(status.taxon[4,(status.taxon>0)[4,]==T]), names(focal.model$frail$survival.data.Spec.taxa.tpl))
focal.model$frail$survival.data.Spec.taxa.tpl[index.V]
status.taxon[4,(status.taxon>0)[4,]==T]

#obtain random effects for "other" species taxa
index.Other <- match(names(status.taxon[3,(status.taxon>0)[3,]==T]), names(focal.model$frail$survival.data.Spec.taxa.tpl))
focal.model$frail$survival.data.Spec.taxa.tpl[index.Other]
status.taxon[3,(status.taxon>0)[3,]==T]

################################################
# 7.3. Graphically compare the survival of threatened and non-threatened ("Other") species taxa.
################################################

#create boxplot
boxplot(list(focal.model$frail$survival.data.Spec.taxa.tpl[index.CR],
	focal.model$frail$survival.data.Spec.taxa.tpl[index.E],
	focal.model$frail$survival.data.Spec.taxa.tpl[index.V],
	focal.model$frail$survival.data.Spec.taxa.tpl[index.Other]),
	names=c("","","",""), ylab="Random species effect", outline=F, ylim=c(-2,4),
	col=c("red","violetred","orange","green"), border=c("red4","violetred4","orange4","green4"))
abline(h = 0, lty=3)
axis(side=1, at=1, labels="CR", col.axis="red")
axis(side=1, at=2, labels="EN", col.axis="violetred")
axis(side=1, at=3, labels="VU", col.axis="orange")
axis(side=1, at=4, labels="Other", col.axis="green")
#add "jittered" data points to boxplot
points(rep(1,3), focal.model$frail$survival.data.Spec.taxa.tpl[index.CR], pch=19, cex=1, col="gray70")
points(rep(2,3), focal.model$frail$survival.data.Spec.taxa.tpl[index.E], pch=19, cex=1, col="gray70")
points(3+runif(18,-0.4,0.4), focal.model$frail$survival.data.Spec.taxa.tpl[index.V], pch=19, cex=0.4, col="gray70")
points(4+runif(386,-0.4,0.4), focal.model$frail$survival.data.Spec.taxa.tpl[index.Other], pch=19, cex=0.4, col="gray70")

################################################
# 7.4. Use a t-test to compare the survival of threatened and non-threatened ("Other") species taxa (see Appendix F).
################################################

#create vector with random species effects
random.species.effects <- c(focal.model$frail$survival.data.Spec.taxa.tpl[index.CR],
	focal.model$frail$survival.data.Spec.taxa.tpl[index.E],
	focal.model$frail$survival.data.Spec.taxa.tpl[index.V],
	focal.model$frail$survival.data.Spec.taxa.tpl[index.Other])

#create corresponding vector of conservation status
conservation.status <- rep(c("CR", "EN", "VU", "Other"), times=c(3,3,19,385))

#create vector of random species effects for threatened and not threatened species taxa
Threatened.species.taxa <- random.species.effects[conservation.status != "Other"]
Other.species.taxa <- random.species.effects[conservation.status == "Other"]

#perform t test, not assuming equality of variances
t.test(Threatened.species.taxa, Other.species.taxa)








rm(list=ls())
options(scipen=10)
tmpDays = 1:365  #Period of the study
displayTime = FALSE

# Start the clock!
ptm <- proc.time()

source("CreateNames.r") #Build all of the array names.  There are a lot of multidimensional arrays in this model.

##############   Model scenarios.  #####################
print("******  Model scenarios from base case  *************")

useAustenCh = FALSE
if(useAustenCh==TRUE) 
{
  print("******  Uses Thomas estimates for Harbor seal chinook consumption.  *************")
  
  print("******  Consumption of Chinook is 0.023 vs 0.21 without Thomas study. *************")
}
noHSorSSL_Lactaion = FALSE
if(noHSorSSL_Lactaion==TRUE) 
{
  print("******  No lactation or gestation costs for Harbor seals of Stellers  *************")  
}
############################################################
nSim = 100
simCnt = 1
sensivityCVRange = c(0.02, 0.1, 0.2)
sensitivityParNames = c(paste(predatorNames[1:4],'klAlpha'), paste(predatorNames[1:4],'mass'), paste(predatorNames[1:3],'abund'), paste(predatorNames[1:3],'Age0'), paste(predatorNames[1:4],'cFrac'), 'smoltLength', 'alphaOneill')
nPars = length(sensitivityParNames)
simDevs = array(NA, c(3,nSim,nPars+2))
sensitivityMethod = "NA"
sensitivityCV = 1
sensitivityPar = -1
rand_devs = array(NA, c(3,nSim,nPars))
parCnt = 1

#These were for the original sensitivity analysis
print(simCnt)

#The is a lot of the basic population biology
#In some instances the "create" file does nothing more than read in a "lookup" table.
#In other instances there are long calculations that are made based on models in the literature.
source("CreatePreyAvailability.r") #The time when different aged Chinook are present in inland waters.
source("CreateMaturityAtAge.r") #Maturity at age
source("CreateSexAgeRatio.r") #Sex and age ratios for the predators
source("CreateInlandPredatorPresence.r")  #Probability of being in inland waters
source("CreatePredatorAbundance.r") #Numbers at age of the predators

source("CreateWeightAtAgeArrays.r")  #Weights of the predators
source("CreatePreySelectivity.r")  #The prey in the diets of the predators.

#These are the energetic models
source("CreateEfficiencyModel.r")  #Calculate efficiency
source("CreateBreedingSeasons.r")  #These breeding seasons change the activity costs
source("CreatePuppingCosts.r")  #These are the costs of producing pups
source("CreateKleiberParameters.r") #All of the Kleiber model parameters
source("CreateActivityFractions.r") #Activity fractions
source("CreateActivityCosts.r") #The activity costs based on the Kleiber model
source("CreateProductionCosts.r")  #For some species the production costs depend on the activity costs calculations and therefore they must be done first
source("CreateGrowthCosts.r") #For some species the growth costs depend on the activity costs calculations and therefore they must be done first
source("CreatePredatorEnergyDemand.r") #Bioenergetics model

#This is chinook growth and avaialability.
source("CreateChinookEnergyContent.r")
source("CreateConsumptionOfChinookByPredators.r")

#These are all of the model plots
source("plot_annualConsumption.r")
source("plot_preyConsumedByNumBiomass.r")
source("plot_pinnipedProjectedImpacts.r")
source("plot_smoltSurvival.r")
source("table_predatorChinookAgeSelectivity.r")
source("table_DailyEnergyDemand.r")
source("table_dailySmoltConsumption.r")
source("outputResultsSection.r")

print(proc.time() - ptm)

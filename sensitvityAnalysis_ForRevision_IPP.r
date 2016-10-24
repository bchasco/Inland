options(scipen=10)
tmpDays = 1:365  #Period of the study
displayTime = FALSE

#Get the default
source("buildModel.r")
#Total biomass consumed
baseBiomass = 0
for(h in 1:nPredator)
  for(i in 1:nAge[h])
    for(s in 1:2)
      for(y in nYear:nYear)
        baseBiomass = baseBiomass + sum(CH_hisayt[[h]][i,s,,y,]*0.0000049*t(Len[,2:6])^3.14)

baseNumbers = sum(CH_hisayt[[1]][,,,nYear,]) + sum(CH_hisayt[[2]][,,,nYear,]) + sum(CH_hisayt[[3]][,,,nYear,]) + sum(CH_hisayt[[4]][,,,nYear,])


# Start the clock!
ptm <- proc.time()

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

sensitivityCV = 3
sensivityCVRange = c(0.02, 0.1, 0.2)
sensitivityParNames = c(paste(predatorNames[1:4],'klAlpha'), paste(predatorNames[1:4],'mass'), paste(predatorNames[1:3],'abund'), paste(predatorNames[1:3],'Age0'), paste(predatorNames[1:4],'cFrac'), 'smoltLength', 'alphaOneill', 'betaOneill')
nSim = 200
cvs = as.data.frame(array(NA, c(10000,7)))
sensitivityMethod = "IPP"
sensitivity=TRUE

#The is a lot of the basic population biology
source("CreateNames.r") #Build all of the array names
source("CreatePreyAvailability.r")
source("CreateMaturityAtAge.r") #Maturity at age
source("CreateSexAgeRatio.r") #Sex and age ratios
source("CreateInlandPredatorPresence.r")  #Probability of being in inland waters

simCnt = 1
for(sensitivityCV in 1:3)
{
  for(sensitivityPar in 1:length(sensitivityParNames))
  {
    for(sim in 1:nSim)
    {
      #Predator attributes
      source("CreatePredatorAbundance.r") #Numbers at age of the predators
      source("CreateWeightAtAgeArrays.r")  #Weights of the predators
      #These are the energetic models
      source("CreateEfficiencyModel.r")  #Calculate efficiency
      source("CreateBreedingSeasons.r")  #These breeding seasons change the activity costs
      source("CreatePuppingCosts.r")  #These breeding seasons change the activity costs
      source("CreateKleiberParameters.r") #All of the Kleiber model parameters
      source("CreateActivityFractions.r") #Activity fractions
      source("CreateActivityCosts.r") #The activity costs based on the Kleiber model
      source("CreateProductionCosts.r")  #For some species the production costs depend on the activity costs calculations and therefore they must be done first
      source("CreateGrowthCosts.r") #For some species the growth costs depend on the activity costs calculations and therefore they must be done first
      source("CreatePredatorEnergyDemand.r") #Bioenergetics model
      
      
      
      #Chinook diet stuff
      source("CreatePreySelectivity.r")  #The prey in the diets of the predators.
      source("CreateChinookEnergyContent.r")
      source("CreateConsumptionOfChinookByPredators.r")

      #Total biomass consumed
      sensitivityBiomass = 0
      for(h in 1:nPredator)
        for(i in 1:nAge[h])
          for(s in 1:2)
            for(y in nYear:nYear)
              sensitivityBiomass = sensitivityBiomass + sum(CH_hisayt[[h]][i,s,,y,]*0.0000049*t(Len[,2:6])^3.14)
      
      sensitivityNumbers = sum(CH_hisayt[[1]][,,,nYear,]) + sum(CH_hisayt[[2]][,,,nYear,]) + sum(CH_hisayt[[3]][,,,nYear,]) + sum(CH_hisayt[[4]][,,,nYear,])

      cvs[simCnt,1] = sensitivityParNames[sensitivityPar]
      cvs[simCnt,2] = sensivityCVRange[sensitivityCV]
      cvs[simCnt,5] = simCnt
      cvs[simCnt,6] = sensitivityNumbers
      cvs[simCnt,7] = sensitivityBiomass
      simCnt = simCnt + 1
    }
  }
}

IPPcvs = cvs 
write.table(IPPcvs,"IPPcvs.out")

print(proc.time() - ptm)

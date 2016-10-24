rm(list=ls())
options(scipen=10)
tmpDays = 1:365  #Period of the study
displayTime = FALSE

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
ageDistSensitivty = 2

#The is a lot of the basic population biology
source("CreateNames.r") #Build all of the array names
source("CreatePreyAvailability.r")
source("CreateMaturityAtAge.r") #Maturity at age
source("CreateSexAgeRatio.r") #Sex and age ratios
source("CreateInlandPredatorPresence.r")  #Probability of being in inland waters
sensivityNumbers = array(NA,c(3,3,4,nYear))
sensivityBiomass = array(NA,c(3,3,4,nYear))
paFrac = c(0.5,1,1.5)
for(pa in 1:3)
{
  
  source("CreatePredatorAbundance.r") #Numbers at age of the predators
  for(h in 1:3)
    N_hisyt[[h]] = N_hisyt[[h]]*paFrac[pa]

  source("CreateWeightAtAgeArrays.r")  #Weights of the predators
  #These are the energetic models
  source("CreateEfficiencyModel.r")  #Calculate efficiency
  source("CreateBreedingSeasons.r")  #These breeding seasons change the activity costs
  source("CreatePuppingCosts.r")  #These breeding seasons change the activity costs
  source("CreateKleiberParameters.r") #All of the Kleiber model parameters
  source("CreateActivityFractions.r") #Activity fractions
  for(am in 1:3)
	{

		source("CreateActivityCosts.r") #The activity costs based on the Kleiber model
		source("CreateProductionCosts.r")  #For some species the production costs depend on the activity costs calculations and therefore they must be done first
		source("CreateGrowthCosts.r") #For some species the growth costs depend on the activity costs calculations and therefore they must be done first
		source("CreatePredatorEnergyDemand.r") #Bioenergetics model

    smoltModel=2 #1=small, 2=average, 3=large
    smoltSelectivityModel=2 #1=50%, 2=100%, 3=150%
  
    #Chinook diet stuff
    source("CreatePreySelectivity.r")  #The prey in the diets of the predators.
    source("CreateChinookEnergyContent.r")
    source("CreateConsumptionOfChinookByPredators.r")
      
    #Numbers consumed by each predator
    Numbers_spy = array(0,c(3,nPredator,nYear))
    myCols = c("lightblue","orange", "black", "darkgrey")
    maxNum = 0
    for(s in 1:3)
    {
      for(y in 1:nYear)
      {  
        for(p in 1:nPredator)
        {
          Numbers_spy[s,p,y] = Numbers_spy[s,p,y] + sum(CH_hisayt[[p]][,,,y,FramSeasons[[s]]])
        }
        if(sum(Numbers_spy[s,,y])>maxNum) maxNum = sum(Numbers_spy[s,,y])
      }
        
    }

    #Total biomass consumed
    Biomass_spy = 0
    for(h in 1:nPredator)
      for(i in 1:nAge[h])
        for(s in 1:2)
          for(y in 1:nYear)
            Biomass_spy[s,p,y] = Biomass_spy[s,p,y] + sum(CH_hisayt[[1]][1,1,,40,]*0.0000049*t(Len[,2:6])^3.14)

    


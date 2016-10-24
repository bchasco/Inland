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

#The is a lot of the basic population biology
source("CreateNames.r") #Build all of the array names
source("CreatePreyAvailability.r")
source("CreateMaturityAtAge.r") #Maturity at age
source("CreateSexAgeRatio.r") #Sex and age ratios
source("CreateInlandPredatorPresence.r")  #Probability of being in inland waters
sensivityNumbers = array(NA,c(3,3,4,nYear))
sensivityBiomass = array(NA,c(3,3,4,nYear))
paFrac = c(0.5,1,1.5)

am = 2 #activity model is average 
smoltModel=2 #1=small, 2=average, 3=large
smoltSelectivityModel=2 #1=50%, 2=100%, 3=150%

for(ad in 2:2)
{
  for(pa in 2:2)
  {
  ageDistSensitivty = ad
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
	source("CreateActivityCosts.r") #The activity costs based on the Kleiber model
		source("CreateProductionCosts.r")  #For some species the production costs depend on the activity costs calculations and therefore they must be done first
		source("CreateGrowthCosts.r") #For some species the growth costs depend on the activity costs calculations and therefore they must be done first
		source("CreatePredatorEnergyDemand.r") #Bioenergetics model

  
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

    #These weights are from Ford et al. 1989, except for smolts
    wtCH_a = c(20/1000,1.2,3.7,8.1,10.5)
    maxBio = 0
    #Biomass consumed by each predator
    Biomass_spy = array(0,c(3,nPredator,nYear))
    for(s in 1:3)
    {
      for(y in 1:nYear)
      {  
        for(p in 1:nPredator)
          for(a in 1:nChAge)
          {
            if(a==1)
              Biomass_spy[s,p,y] = Biomass_spy[s,p,y] + sum(CH_hisayt[[p]][,,a,y,FramSeasons[[s]]])*0.0000049*mean(Len[unlist(FramSeasons[[s]]),2])^3.14
            if(a>1)
              Biomass_spy[s,p,y] = Biomass_spy[s,p,y] + sum(CH_hisayt[[p]][,,a,y,FramSeasons[[s]]])*wtCH_a[a]
          }
        if(sum(Biomass_spy[s,,y])>maxBio) maxBio = sum(Biomass_spy[s,,y])
      }
    }
    
    sensivityNumbers[pa,ad,,] = apply(Numbers_spy,c(2,3),sum)
    sensivityBiomass[pa,ad,,] = apply(Biomass_spy,c(2,3),sum)
  }
}


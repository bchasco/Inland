predatorNames = c("Harbor seals", "Steller sea lion", "CA sea lion", "Killer whales") #Predator names
shortName = c('HS', 'SSL', 'CSL', 'KW')
rangeNames = c("lwr", "upr")   #Range of values for different things
sexNames = c("male", "female")  #Sexes
seasonNames = c("breed", "nonbreed")  #Two broad seasonal categories
ageNames = paste("Ocean", 0:4, sep="")  #Names of the chinook ocean ages
ch_lengths = seq(5,1500,10) #Length of chinook, this is used to determine the predator consumption by age
FramNames = c("May_June","July_Sept","Oct_April") #Names for the FRAM seasons

days = 1:365  #days in a year
years = 1970:2015 #years in the study

#The are calendar days within the FRAM seasons
FramSeasons = list(
	May_June = 122:182,
  July_Sept = 183:274,
  Oct_April = c(1:121,275:365))

#These are the dimensions based on the names given above
nPredator = length(predatorNames) #Dimension of the predator names
nRange = length(rangeNames) #dimension of the range names
nSex = length(sexNames) #Dimension of the sex names
nSeason = length(seasonNames)  #Dimension of the season names
nAge = length(ageNames)  #Dimension of the chinook ages
nLength = length(ch_lengths)  #Dimension of the Chinook lengths
nDays = length(days)
nYear = length(years)

#Names of years
yearNames = years

#The problem is there is disconnect between the stages in the population dynamics model
#and the activity stages that are reported.  Nearly every paper has activities divided into 
#juveniles, sub-adults, and adults
#HS, SSL, CSL, KW
stageNames = list()
for(h in 1:nPredator)
	stageNames[[h]] = c("juv","subadult","adult")

#stageNames in a ragged array by speciess
#HS, SSL, CSL, KW
#You must include the newborn age class, that is if there are 25 age classes, make it 26 for the pups.
nAge = c(26,25,11,25)

#Number of Chinook ocean ages: 0, 1, 2, 3, 4
nChAge = 5
chAgeNames = 0:(nChAge-1)

#The activity array is also jagged.  Not all of the predator have the 
#same activities
#activityNames in a ragged array by speciess
nActivity = c(3,2,2,1)
activityNames = list()
#Howard et al. 2010
activityNames = c(activityNames,list(array(c("surface","dive","haulout"),nActivity[1])))
#Winship et al. 2002, Noren et al. 2009
activityNames = c(activityNames,list(array(c("water","land"),nActivity[2])))
#Weis and Harvey 2008
activityNames = c(activityNames,list(array(c("at_sea","on_shore"),nActivity[3])))
#Noren et al. 2011
#activityNames = c(activityNames,list(array(c("foraging","travelling","resting","socializing"),nActivity[4])))

activityNames = c(activityNames,list(array(c("life"),nActivity[4])))

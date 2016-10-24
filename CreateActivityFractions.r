#Fraction of time spent doing each activity by stage, sex, and season
#Activity parameters for Kleiber model
#Alpha parameters ranges
fa_hbrkjs = list()
for(h in 1:nPredator)
  fa_hbrkjs[[h]] = array(NA,
                      c(nSeason,nRange,length(stageNames[[h]]),nActivity[h],nSex)) 

#average of the range
fa_hbkjs = list()
for(h in 1:nPredator)
  fa_hbkjs[[h]] = array(NA,
                      c(nSeason,length(stageNames[[h]]),nActivity[h],nSex))


#First start by filling in the harbor seal fractions based on Howard et al. 2010
#adult, female
fa_hbrkjs[[1]][1,,3,,2] =  matrix(c(0,40.3,46.1,81,13.6,20.6)/100,2,nActivity[1])
fa_hbrkjs[[1]][2,,3,,2] =  matrix(c(3.9,74.5,14.9,63.6,10.6,32.5)/100,2,nActivity[1])

fa_hbkjs[[1]][1,3,,2] =    
  colMeans(fa_hbrkjs[[1]][1,,3,,2])/sum(colMeans(fa_hbrkjs[[1]][1,,3,,2]))
fa_hbkjs[[1]][2,3,,2] =  
  colMeans(fa_hbrkjs[[1]][2,,3,,2])/sum(colMeans(fa_hbrkjs[[1]][2,,3,,2]))

#sub-adult, female
fa_hbrkjs[[1]][1,,2,,2] =  matrix(c(0,40.3,46.1,81,13.6,20.6)/100,2,nActivity[1])
fa_hbrkjs[[1]][2,,2,,2] =  matrix(c(0,40.3,46.1,81,13.6,20.6)/100,2,nActivity[1])

fa_hbkjs[[1]][1,2,,2] =  
  colMeans(fa_hbrkjs[[1]][1,,2,,2])/sum(colMeans(fa_hbrkjs[[1]][1,,2,,2]))
fa_hbkjs[[1]][2,2,,2] =  
  colMeans(fa_hbrkjs[[1]][2,,2,,2])/sum(colMeans(fa_hbrkjs[[1]][2,,2,,2]))

#pups, female
fa_hbrkjs[[1]][1,,1,,2] =  matrix(c(30.1,38.7,3.4,7.4,57.9,62.5)/100,2,nActivity[1])
fa_hbrkjs[[1]][2,,1,,2] =  matrix(c(30.1,38.7,3.4,7.4,57.9,62.5)/100,2,nActivity[1])

fa_hbkjs[[1]][1,1,,2] =  
  colMeans(fa_hbrkjs[[1]][1,,1,,2])/sum(colMeans(fa_hbrkjs[[1]][1,,1,,2]))
fa_hbkjs[[1]][2,1,,2] =  
  colMeans(fa_hbrkjs[[1]][2,,1,,2])/sum(colMeans(fa_hbrkjs[[1]][2,,1,,2]))

#adult, male
fa_hbrkjs[[1]][1,,3,,1] =  matrix(c(0,73.6,17.1,78.3,9.3,22)/100,2,nActivity[1])
fa_hbrkjs[[1]][2,,3,,1] =  matrix(c(0,27.7,51,72.7,21.3,33.1)/100,2,nActivity[1])

fa_hbkjs[[1]][1,3,,1] =  
  colMeans(fa_hbrkjs[[1]][1,,3,,1])/sum(colMeans(fa_hbrkjs[[1]][1,,3,,1]))
fa_hbkjs[[1]][2,3,,1] =  
  colMeans(fa_hbrkjs[[1]][2,,3,,1])/sum(colMeans(fa_hbrkjs[[1]][2,,3,,1]))

#sub-adult, male
fa_hbrkjs[[1]][1,,2,,1] =  matrix(c(0,27.7,51,72.7,21.3,33.1)/100,2,nActivity[1])
fa_hbrkjs[[1]][2,,2,,1] =  matrix(c(0,27.7,51,72.7,21.3,33.1)/100,2,nActivity[1])

fa_hbkjs[[1]][1,2,,1] =  
  colMeans(fa_hbrkjs[[1]][1,,2,,1])/sum(colMeans(fa_hbrkjs[[1]][1,,2,,1]))
fa_hbkjs[[1]][2,2,,1] =  
  colMeans(fa_hbrkjs[[1]][2,,2,,1])/sum(colMeans(fa_hbrkjs[[1]][2,,2,,1]))

#pups, male
fa_hbrkjs[[1]][1,,1,,1] =  matrix(c(30.1,38.7,3.4,7.4,57.9,62.5)/100,2,nActivity[1])
fa_hbrkjs[[1]][2,,1,,1] =  matrix(c(30.1,38.7,3.4,7.4,57.9,62.5)/100,2,nActivity[1])

fa_hbkjs[[1]][1,1,,1] =  
  colMeans(fa_hbrkjs[[1]][1,,1,,1])/sum(colMeans(fa_hbrkjs[[1]][1,,1,,1]))
fa_hbkjs[[1]][2,1,,1] =  
  colMeans(fa_hbrkjs[[1]][2,,1,,1])/sum(colMeans(fa_hbrkjs[[1]][2,,1,,1]))

###Now fill in the Steller sea lion arrays based on Winship et al. 2002
#adult, female
fa_hbrkjs[[2]][1,,3,,2] =  matrix(c(0,0.77,0.23,1),nRange,nActivity[2])
fa_hbrkjs[[2]][2,,3,,2] =  matrix(c(0.73,0.90,0.10,0.27),nRange,nActivity[2])

fa_hbkjs[[2]][1,3,,2] =  colMeans(fa_hbrkjs[[2]][1,,3,,2])/sum(colMeans(fa_hbrkjs[[2]][1,,3,,2]))
fa_hbkjs[[2]][2,3,,2] =  colMeans(fa_hbrkjs[[2]][2,,3,,2])/sum(colMeans(fa_hbrkjs[[2]][2,,3,,2]))

#sub-adult, female
fa_hbrkjs[[2]][1,,2,,2] =  matrix(c(0.37,0.76,0.24,0.63),nRange,nActivity[2])
fa_hbrkjs[[2]][2,,2,,2] =  matrix(c(0.37,0.76,0.24,.63),nRange,nActivity[2])

fa_hbkjs[[2]][1,2,,2] =  colMeans(fa_hbrkjs[[2]][1,,2,,2])/sum(colMeans(fa_hbrkjs[[2]][1,,2,,2]))
fa_hbkjs[[2]][2,2,,2] =  colMeans(fa_hbrkjs[[2]][2,,2,,2])/sum(colMeans(fa_hbrkjs[[2]][2,,2,,2]))

#pups, female
fa_hbrkjs[[2]][1,,1,,2] =  matrix(c(0,0,1,1),nRange,nActivity[2])
fa_hbrkjs[[2]][2,,1,,2] =  matrix(c(0,0,1,1),nRange,nActivity[2])

fa_hbkjs[[2]][1,1,,2] =  colMeans(fa_hbrkjs[[2]][1,,1,,2])/sum(colMeans(fa_hbrkjs[[2]][1,,1,,2]))
fa_hbkjs[[2]][2,1,,2] =  colMeans(fa_hbrkjs[[2]][2,,1,,2])/sum(colMeans(fa_hbrkjs[[2]][2,,1,,2]))

#adult, male
fa_hbrkjs[[2]][1,,3,,1] =  matrix(c(0,0,1,1),nRange,nActivity[2])
fa_hbrkjs[[2]][2,,3,,1] =  matrix(c(0.55,0.85,.15,.45),nRange,nActivity[2])

fa_hbkjs[[2]][1,3,,1] =  colMeans(fa_hbrkjs[[2]][1,,3,,1])/sum(colMeans(fa_hbrkjs[[2]][1,,3,,1]))
fa_hbkjs[[2]][2,3,,1] =  colMeans(fa_hbrkjs[[2]][2,,3,,1])/sum(colMeans(fa_hbrkjs[[2]][1,,3,,1]))

#sub-adult, male
fa_hbrkjs[[2]][1,,2,,1] =  matrix(c(0.37,.76,0.24,0.63),nRange,nActivity[2])
fa_hbrkjs[[2]][2,,2,,1] =  matrix(c(0.37,.76,0.24,0.63),nRange,nActivity[2])

fa_hbkjs[[2]][1,2,,1] =  colMeans(fa_hbrkjs[[2]][1,,2,,1])/sum(colMeans(fa_hbrkjs[[2]][1,,2,,1]))
fa_hbkjs[[2]][2,2,,1] =  colMeans(fa_hbrkjs[[2]][2,,2,,1])/sum(colMeans(fa_hbrkjs[[2]][2,,2,,1]))

#pups, male
fa_hbrkjs[[2]][1,,1,,1] =  matrix(c(0,0,1,1),nRange,nActivity[2])
fa_hbrkjs[[2]][2,,1,,1] =  matrix(c(0,0,1,1),nRange,nActivity[2])

fa_hbkjs[[2]][1,1,,1] =  colMeans(fa_hbrkjs[[2]][1,,1,,1])/sum(colMeans(fa_hbrkjs[[2]][1,,1,,1]))
fa_hbkjs[[2]][2,1,,1] =  colMeans(fa_hbrkjs[[2]][2,,1,,1])/sum(colMeans(fa_hbrkjs[[2]][2,,1,,1]))


#California sea lions and killer whales are much simpler than the other two species

#This is for California sea lion
for(b in 1:nSeason)
{
  for(s in 1:nSex)
  {  
    for(k in 1:length(stageNames[[4]]))
    {
      fa_hbkjs[[3]][b,k,,s] = c(0.564, 0.436)
    }
  }
}

#This is for killer whales
for(b in 1:nSeason)
{
  for(s in 1:nSex)
  {  
    for(k in 1:length(stageNames[[4]]))
    {
      #fa_hbkjs[[4]][b,k,,s] = c(0.21,	0.704,	0.068,	0.018)
      fa_hbkjs[[4]][b,k,,s] = c(1)
    }
  }
}


#Probability of lactating
pLC = read.table("pLC.dat", skip=1, header=FALSE)
pLC_hst = list()
for(h in 1:nPredator)
{
  pLC_hst[[h]] = array(0, c(nSex,nDays))
  pLC_hst[[h]][2,] = pLC[,h]
}


#Probability of being fecund
pF_hs = list()
pF_hs[[1]] = array(0, c(nSex))
pF_hs[[2]] = array(0, c(nSex))
pF_hs[[3]] = array(0, c(nSex))
pF_hs[[4]] = array(0, c(nSex))
#Harbor seal females
pF_hs[[1]][2] = 0.91
#Steller sea lion females
pF_hs[[2]][2] = 0.63

#********************!!!!!!!!!!!!!!!!!!!!!!!!
#There are no production costs for either the California sea lion of killer whale models.
#**************************!!!!!!!!!!!!!!!!!!

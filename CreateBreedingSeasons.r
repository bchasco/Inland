#In all of the paper the fraction of time is either breeding for male,
#or pupping costs for female.  The arrays are simply 0's and 1's denote breeding (1) or 
#non-breeding (0)

#Probabilty of gestation happening on a given day.
pPC = read.table("pPC.dat", skip=1, header=FALSE)
pPC_hst = list()
for(h in 1:nPredator)
{
  pPC_hst[[h]] = array(0, c(nSex,nDays))
  pPC_hst[[h]][2,] = pPC[,h]
}


pb_hbst = list()
HS = as.matrix(read.table("HS_SeasonByDay.dat", header=FALSE))
SSL = as.matrix(read.table("SSL_SeasonByDay.dat", header=FALSE))

#Breeding season
for(h in 1:nPredator)
{
  pb_hbst[[h]] = array(NA,
                            c(nSeason,nSex,nDays)) 
}

#Breeding season
pb_hbst[[1]][1,,] = as.matrix(t(HS))
#Non-breeding season
pb_hbst[[1]][2,,] = as.matrix(t(1-HS))

#Breeding season
pb_hbst[[2]][1,,] = as.matrix(t(SSL))
#Non-breeding season
pb_hbst[[2]][2,,] = as.matrix(t(1-SSL))


#Like killer whales the activity difference for CSL are not 
#consider in the bioenergetics model in the literature
#Breeding season
pb_hbst[[3]][1,,] = 0
#Non-breeding season
pb_hbst[[3]][2,,] = 1

#There doesn't really appear to be a difference in activity for killer whales
#Breeding season
pb_hbst[[4]][1,,] = 0
#Non-breeding season
pb_hbst[[4]][2,,] = 1


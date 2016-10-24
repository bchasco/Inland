
#Activity parameters for Klieber model
#Alpha parameters ranges
aa_hbrkjs = list()
for(i in 1:nPredator)
  aa_hbrkjs[[i]] = array(NA,
                      c(nSeason,nRange,length(stageNames[[i]]),nActivity[i],nSex))
#average of the range
aa_hbkjs = list()
for(i in 1:nPredator)
  aa_hbkjs[[i]] = array(NA,
                      c(nSeason,length(stageNames[[i]]),nActivity[i],nSex))

#Alpha parameters ranges
bb_hbrkjs = list()
for(i in 1:nPredator)
  bb_hbrkjs[[i]] = array(NA,
                      c(nSeason,nRange,length(stageNames[[i]]),nActivity[i],nSex)) 
#average of the range
bb_hbkjs = list()
for(i in 1:nPredator)
  bb_hbkjs[[i]] = array(NA,
                      c(nSeason,length(stageNames[[i]]),nActivity[i],nSex)) 

kleiberErr = rnorm(1,0,sensivityCVRange[sensitivityCV])

#Using data from Howard et. al 2010 parameterize the Kleiber model
aHS = read.table("HS_Kleiber_alpha.dat", header=FALSE)
bHS = read.table("HS_Kleiber_beta.dat", header=FALSE)
for(h in 1:1)
{
  for(b in 1:nSeason)
  {
    for(s in 1:nSex)
    {  
      for(k in 1:length(stageNames[[h]]))
      {
        for(j in 1:nActivity[[h]])
        {
          for(r in 1:nRange)
          { 
            aa_hbrkjs[[h]][b,r,k,j,s] = aHS[j,r] * 86400
            bb_hbrkjs[[h]][b,r,k,j,s] = bHS[j,r]
          }
          aa_hbkjs[[h]][b,k,j,s] = mean(aa_hbrkjs[[h]][b,,k,j,s])
          #if(sensitivityPar!=1)
          #  aa_hbkjs[[h]][b,k,j,s] = rnorm(1,aa_hbkjs[[h]][b,k,j,s],aa_hbkjs[[h]][b,k,j,s]*sensivityCVRange[sensitivityCV])
          bb_hbkjs[[h]][b,k,j,s] = mean(bb_hbrkjs[[h]][b,,k,j,s])
        }
      }
    }
  }
}


#Using data from Winship et. al 2002 parameterize the Kleiber model
#1st line [[SSL]][breeding, lwr, , ,male]  first three are land: juv, sub, ad; then water: juv, sub, ad
aa_hbrkjs[[2]][1,1,,,1] = c(2.5,1.75,2.5,2.5,1.75,2.5)
aa_hbrkjs[[2]][2,1,,,1] = c(2.5,1.75,2.5,2.5,1.75,1.0)
aa_hbrkjs[[2]][1,2,,,1] = c(3.5,2.25,5.5,3.5,2.25,3.5)
aa_hbrkjs[[2]][2,2,,,1] = c(3.5,2.25,5.5,3.5,2.25,1.4)

aa_hbrkjs[[2]][1,1,,,2] = c(2.5,1.75,2.5,2.5,1.75,1.0)
aa_hbrkjs[[2]][2,1,,,2] = c(2.5,1.75,2.5,2.5,1.75,1.0)
aa_hbrkjs[[2]][1,2,,,2] = c(3.5,2.25,5.5,3.5,2.25,1.4)
aa_hbrkjs[[2]][2,2,,,2] = c(3.5,2.25,5.5,3.5,2.25,1.4)

for(h in 2:2)
{
  for(b in 1:nSeason)
  {
    for(s in 1:nSex)
    {  
      for(k in 1:length(stageNames[[h]]))
      {
        for(j in 1:nActivity[[h]])
        {
          aa_hbkjs[[h]][b,k,j,s] = mean(aa_hbrkjs[[h]][b,,k,j,s])*293000
          #if(sensitivityPar!=1)
          #  aa_hbkjs[[h]][b,k,j,s] = rnorm(1,aa_hbkjs[[h]][b,k,j,s],aa_hbkjs[[h]][b,k,j,s]*sensivityCVRange[sensitivityCV])
          bb_hbkjs[[h]][b,k,j,s] = 0.75
        }
      }
    }
  }
}


#Based on Weis and Harvey 2008 
CSL_aa = matrix(c(5, 2, 5, 2), nActivity[3], nSex) 
for(b in 1:nSeason)
{
  for(s in 1:nSex)
  {  
    for(k in 1:length(stageNames[[3]]))
    {
      for(j in 1:nActivity[[3]])
      {
        aa_hbkjs[[3]][b,k,j,s] = CSL_aa[j,s] * 61 * 1000 * 4.184
        #if(sensitivityPar!=1)
        #  aa_hbkjs[[3]][b,k,j,s] = rnorm(1,aa_hbkjs[[3]][b,k,j,s],aa_hbkjs[[3]][b,k,j,s]*sensivityCVRange[sensitivityCV])
        
        bb_hbkjs[[3]][b,k,j,s] = 0.75
      }
    }
  }
}



#Based on Noren 2011
#To get these Kleiber multipliers you need to look at the activity fractions and the activity costs
#She doesn't say outright what these are, you have to back calculate.  Kind of pain in the ass to make them comparable to the other Kleiber multipliers.
#KW_aa = matrix(c(7.24, 7.45, 7.16, 6.87, 4.994, 5.05, 4.90 ,4.75), nActivity[4], nSex) 
KW_aa = matrix(c((495.9+413.2)/2,(495.9+413.2)/2), nActivity[4], nSex) #There is only one acitvity and two sexes, with no breeding activity
for(b in 1:nSeason)
{
  for(s in 1:nSex)
  {  
    for(k in 1:length(stageNames[[4]]))
    {
      for(j in 1:nActivity[[4]])
      {
        aa_hbkjs[[4]][b,k,j,s] = KW_aa[j,s] * 4.184 * 1000
        #if(sensitivityPar!=1)
        #  aa_hbkjs[[4]][b,k,j,s] = rnorm(1,aa_hbkjs[[4]][b,k,j,s],aa_hbkjs[[4]][b,k,j,s]*sensivityCVRange[sensitivityCV])
        
        bb_hbkjs[[4]][b,k,j,s] = 0.75
      }
    }
  }
}

if(sensitivityMethod=="IPP")
  if(sensitivityPar<=4)
{
    #Pinniped predator based on the sensitiity parameter
    myPred = sensitivityPar

    #record the sensitivity parameter
    cvs[simCnt,3] = mean(aa_hbkjs[[myPred]]) + mean(aa_hbkjs[[myPred]])*kleiberErr 
    cvs[simCnt,4] = mean(aa_hbkjs[[myPred]])
    aa_hbkjs[[myPred]] = aa_hbkjs[[myPred]] + aa_hbkjs[[myPred]]*kleiberErr 
    
  }

if(sensitivityMethod=="RPSS")# & sensitivityPar!=1)
{
  parCnt=1
  for(h in 1:nPredator)
  {
    aa_hbkjs[[h]] = aa_hbkjs[[h]] + aa_hbkjs[[h]]*rand_devs[sensitivityCV,simCnt,parCnt] 
    simDevs[sensitivityCV,simCnt,parCnt] = rand_devs[sensitivityCV,simCnt,parCnt]
    parCnt = parCnt + 1
  }
}

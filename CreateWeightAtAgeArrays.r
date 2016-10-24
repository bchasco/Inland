#Predator weights
wt_his = list()

#This is weight at age data is from the literature

#
HSwt = read.table("HSwt.dat", header=TRUE)
#Steller sea lion weight from Winship et al. 2006
SSLwt = read.table("SSLwt.dat", header=TRUE)
#California sea lion weight from Winship et al. 2006
CSLwt = read.table("CSLwt.dat", header=TRUE)
#Kille whale wt are taken from Noren et al. 2011, figure 1.
KWwt = read.table("KWwt.dat", header=TRUE)

wt_his[[1]] = array(as.matrix(HSwt),
                         c(nAge[1],nSex)) 
wt_his[[2]] = array(as.matrix(SSLwt),
                         c(nAge[2],nSex))
wt_his[[3]] = array(as.matrix(CSLwt),
                         c(nAge[3],nSex)) 
wt_his[[4]] = array(as.matrix(KWwt[,1:2]),
                         c(nAge[4],nSex))

#Snesitivity analysis
#if(sensitivityPar!=2)
#{
  predWtErr = rnorm(1,0,sensivityCVRange[sensitivityCV])
  if(sensitivityMethod=="IPP")
    if(sensitivityPar>4 & sensitivityPar<=8)
  {
      myPred = sensitivityPar - 4
      cvs[simCnt,3] = mean(wt_his[[myPred]]) + mean(wt_his[[myPred]])* predWtErr
      cvs[simCnt,4] = mean(wt_his[[myPred]])
      wt_his[[myPred]] = wt_his[[myPred]] + wt_his[[myPred]]* predWtErr
      
  }

  if(sensitivityMethod=="RPSS")# & sensitivityPar!=2)
  {
    parCnt = 5
    for(h in 1:nPredator)
    {
      wt_his[[h]] = wt_his[[h]] + wt_his[[h]] * rand_devs[sensitivityCV,simCnt,parCnt] 
      simDevs[sensitivityCV,simCnt,parCnt] = rand_devs[sensitivityCV,simCnt,parCnt]
      parCnt = parCnt + 1
    }
    
  }
      
  #}

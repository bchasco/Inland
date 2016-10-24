#All of the data and files can be found in
#https://www.dropbox.com/home/PSC%20project%202015/marine%20mammal%20data
#Just to maintain a little order, I have exported the data from this website and 
#done some transformations on the raw data, to make the abundances specific to Puget Sound

#Raw harbor seals
HS = read.table("Abundance_HS.dat", header=TRUE)
HS = rowSums(HS[,2:6])*1.53


Pearson4percentDecline = HS
for(i in 31:46)
  Pearson4percentDecline[i] = Pearson4percentDecline[i-1] * 0.96

#HS = HS$Pearson.trend


#Raw Steller, this is just for Washington waters
SSL = read.table("Abundance_SSL.dat", header=TRUE)
#Based on Eric MARSS analysis
SSL = exp(SSL[,2])

#Raw CSL - this is for the whole west coast
CSL = read.table("Abundance_CSL.dat", header=TRUE)
CSL = CSL[,3]

#Raw killer whale
KW = read.table("Abundance_KW.dat", header=TRUE)
KW = KW[,2]

if(length(KW)!=length(years) | length(CSL)!=length(years) | length(SSL)!=length(years) | length(HS)!=length(years))
{
	print("There is an error in the length of one of the predator abundance vectors.")
	print("Start with file createPredatorAbundance, and see what may be wrong.")
	print(paste("The predator vectors are ",c(length(HS), length(CSL), length(SSL), length(KW))))
	break
}

N_hy = list()
#KW does not need correction factors
N_hy[[1]] = HS
N_hy[[4]] = KW

#CSL and SSL need correction factors.  These corrections will be based on a navy survey done in 2013/2014
#These multipliers are based on the NMFS report NOAA TM-28 for california sea lions. 
N_hy[[2]] = SSL/max(SSL)*109*2
N_hy[[3]] = CSL/max(CSL)*1200*2

predAbundanceErr = rnorm(1,0,sensivityCVRange[sensitivityCV])
if(sensitivityMethod=="IPP")
   if(sensitivityPar>8 & sensitivityPar<=11)
{
  myPred = sensitivityPar - 8
  cvs[simCnt,3] = sum(N_hy[[myPred]]) + sum(N_hy[[myPred]]) * predAbundanceErr
  cvs[simCnt,4] = sum(N_hy[[myPred]])
  N_hy[[myPred]] = N_hy[[myPred]] + N_hy[[myPred]] * predAbundanceErr
  
}
if(sensitivityMethod=="RPSS")# & sensitivityPar!=3)
{
  parCnt = 9
  for(h in 1:3)
  {
    N_hy[[h]] = N_hy[[h]] + N_hy[[h]] * rand_devs[sensitivityCV,simCnt,parCnt]
    simDevs[sensitivityCV,simCnt,parCnt] = rand_devs[sensitivityCV,simCnt,parCnt]
    parCnt = parCnt + 1
  }
  
}  

#These multiply the time series of abundance by: 1) proportions at age and sex from the CreateSexAgeRation.r file,
#and, 2) the presence of predator from the CreateInlandPredatorPresence.r file.
N_hisyt = list()
for(h in 1:4)
{
  N_hisyt[[h]] = array(0,
                  c(nAge[h],nSex,nYear,nDays))
  
  for(s in 1:nSex)
  {
    for(y in 1:nYear)
    {
      for(i in 1:nAge[h])
      {
        N_hisyt[[h]][i,s,y,] = N_hy[[h]][y] * psi_ht[[h]] * hi_hisy[[h]][i,s,y]
      }
    }
  }
}

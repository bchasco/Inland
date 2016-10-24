#This length data is a combination of average adult lengths and juvenile lengths 
Len = read.table("aveSmoltLengths.dat", skip = 1, header=FALSE)
lenErr = rnorm(1,0,sensivityCVRange[sensitivityCV])
if(sensitivityMethod=="IPP")
  if(sensitivityPar==19)
{
  cvs[simCnt,3] = mean(Len[92:213,2]) + mean(Len[92:213,2]) * predAbundanceErr
  cvs[simCnt,4] = mean(Len[92:213,2])
  Len[92:213,2] = Len[92:213,2] + Len[92:213,2]*lenErr
  
}

if(sensitivityMethod=="RPSS")# & sensitivityPar!=6)
{
  parCnt = 19
  Len[92:213,2] = Len[92:213,2] + Len[92:213,2]*rand_devs[sensitivityCV,simCnt,parCnt]  
}  
simDevs[sensitivityCV,simCnt,parCnt] = rand_devs[sensitivityCV,simCnt,parCnt]

#Parameters from Sandie O'Neill's 2014 paper 
#Literature data
Ch_Epars= c(1.1e-5, 3.122)
oneillAlphaErr = rnorm(1,0,sensivityCVRange[sensitivityCV])
oneillBetaErr = rnorm(1,0,sensivityCVRange[sensitivityCV])

if(sensitivityMethod=="IPP")
  if(sensitivityPar==20)
{
  cvs[simCnt,3] = Ch_Epars[1] + Ch_Epars[1] * oneillAlphaErr
  cvs[simCnt,4] = Ch_Epars[1] 
  Ch_Epars[1] = Ch_Epars[1] + Ch_Epars[1] * oneillAlphaErr
}

#if(sensitivityMethod=="IPP"  & sensitivityPar==21)
#{
#  cvs[simCnt,3] = Ch_Epars[2] + Ch_Epars[2] * oneillBetaErr
#  cvs[simCnt,4] = Ch_Epars[2]
#  Ch_Epars[2] = Ch_Epars[2] + Ch_Epars[2] * oneillBetaErr
#}

if(sensitivityMethod=="RPSS")# & sensitivityPar!=7)
{
  parCnt = 20
  Ch_Epars[1] = Ch_Epars[1] + Ch_Epars[1] * rand_devs[sensitivityCV,simCnt,parCnt]
  simDevs[sensitivityCV,simCnt,parCnt] = rand_devs[sensitivityCV,simCnt,parCnt]

#  parCnt = parCnt + 1
#  Ch_Epars[2] = Ch_Epars[2] + Ch_Epars[2] * rand_devs[sensitivityCV,simCnt,parCnt]
#  simDevs[sensitivityCV,simCnt,parCnt] = rand_devs[sensitivityCV,simCnt,parCnt]
}

#Energy content by age in joules.  Oneill et al calculate the energy in kcal.
Ech_a = Ch_Epars[1]*(Len[,2:6]*10)^Ch_Epars[2]*1000*4.184


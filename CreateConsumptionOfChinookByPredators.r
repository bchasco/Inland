#Fraction of the Chinook in the diet.  Right now this just varies by day of the year.  But presumably this might vary by age or stage.
phi_hiyt = list()
phi = read.table("phi_hiyt.dat", header=TRUE)

chinookFracErr = rnorm(1, 0, sensivityCVRange[sensitivityCV])
  
for(h in 1:4) #season
{
  phi_hiyt[[h]] = array(0,
                      c(nAge[h], nYear, nDays))
  for(y in 1:nYear)
    for(i in 1:nAge[h])
    {
      phi_hiyt[[h]][i,y,] = phi[,h+1]
    }
}

if(sensitivityMethod=="IPP")
  if(sensitivityPar>14 & sensitivityPar<=18)
{
  myPred = sensitivityPar - 14
  cvs[simCnt,3] = min(mean(phi_hiyt[[myPred]]) + mean(phi_hiyt[[myPred]])*chinookFracErr,0.99)
  cvs[simCnt,4] = mean(phi_hiyt[[myPred]])
  phi_hiyt[[myPred]] = phi_hiyt[[myPred]] + phi_hiyt[[myPred]]*chinookFracErr

  if(myPred==4)  
    for(y in 1:nYear)
      for(i in 1:nAge[h])
      {
        phi_hiyt[[h]][i,y,phi_hiyt[[h]][i,y,]>1] = 1
      }
}
if(sensitivityMethod=="RPSS")
{
  parCnt = 15
  for(h in 1:4)
  {
    for(y in 1:nYear)
      for(i in 1:nAge[h])
      {
        phi_hiyt[[h]][i,y,] = min(phi_hiyt[[h]][i,y,] + phi_hiyt[[h]][i,y,]*rand_devs[sensitivityCV,simCnt,parCnt],0.99)
      }
    simDevs[sensitivityCV,simCnt,parCnt] = rand_devs[sensitivityCV,simCnt,parCnt]
    parCnt = parCnt + 1  
  }
}

CH_hisayt = list()
#phi is the diet fraction
#nu is the age distribution of the diet fraction, see  

for(h in 1:4) #season
{
  CH_hisayt[[h]] = array(NA,
                              c(nAge[h], nSex, nChAge, nYear, nDays))
  #for(y in 1:nYear)
  {
    for(i in 1:(nAge[h]))
    {
      for(s in 1:nSex)
      {
        for(a in 1:5)
          CH_hisayt[[h]][i,s,a,,] = t(as.matrix(t(E_hisyt[[h]][i,s,,]*phi_hiyt[[h]][i,,]))*as.vector((nu_hisat[[h]][i,s,a,]*psi_ht[[h]])/Ech_a[,a]))
      } #end sex
    }# end age
  }# end year
}#end predator



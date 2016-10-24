#Multiply prey availability with the prey selectivity.
#Read in prey selectivity

sel = read.table("preySelectivity.dat", header=TRUE)
sel[,3] = as.numeric(as.character(sel[,3]))

age0Err = rnorm(1,0,sensivityCVRange[sensitivityCV])

#harbor seals
if(sensitivityMethod=="IPP")
  if(sensitivityPar>11 & sensitivityPar<=14)
{
  #Age 0
    #Pinniped predator based on the sensitiity parameter
    myPred = sensitivityPar-11
    #deviate
    dev = sel[sel[,3]==myPred,4]*age0Err

    #record the sensitivity parameter
    cvs[simCnt,3] = max(0,sel[sel[,3]==myPred,4] + dev)[1]
    cvs[simCnt,4] = (sel[sel[,3]==myPred,4])[1]
    
    #Change the selectivity parameter for the 
    sel[sel[,3]==myPred,4] = max(0,sel[sel[,3]==myPred,4] + dev)
    #Evenly distribute the proportions to the none age 0 age classes
    sel[sel[,3]==myPred,5:8] = rep((1-sel[sel[,3]==myPred,4])/4,4)
}

if(sensitivityMethod=="RPSS")# & sensitivityPar!=4)
{
  #Age 0
  parCnt = 12
  for(h in 1:3)
  {
    #deviate
    sel[sel[,3]==h,4] = sel[sel[,3]==h,4] + rand_devs[sensitivityCV,simCnt,parCnt] * sel[sel[,3]==h,4]
    #Evenly distribute the proportions to the none age 0 age classes
    sel[sel[,3]==h,5:8] = rep((1-sel[sel[,3]==h,4])/4,4)
    simDevs[sensitivityCV,simCnt,parCnt] = rand_devs[sensitivityCV,simCnt,parCnt]
    parCnt = parCnt + 1
  }
}

nu_hisat = list()
#Proportion of chinook ages in the diets
for(h in 1:4)
{
  nu_hisat[[h]] = array(NA,c(nAge[h],nSex,nChAge,nDays))
  for(s in 1:nSex)
  {
    for(i in 1:(nAge[h]))
    {
      for(a in 1:nChAge)
      {
        #Multiply selectivity by presence.
        #Selectivity is the size that is preferred and vartheta is the probability of the prey size being present.
        #The combination of the two is the probability of the prey being consumed.
        nu_hisat[[h]][i,s,a,] =  sel[sel$predatorID==h,a+3]#*vartheta[,a]
      }
      #You have to do this jinky transposing to normalize.
      nu_hisat[[h]][i,s,,] =  t(t(nu_hisat[[h]][i,s,,])/rowSums(t(nu_hisat[[h]][i,s,,])))
    }
  }
}


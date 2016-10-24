par(mfcol=c(2,2), mai=c(1,1.,0.4,0.1), mgp=c(3.0,1,0))
sampleDay = c(182,365,365,182)
for(p in 1:4)
{
  
  CH = CH_hisayt[[p]][,1,,nYear,sampleDay[p]] + CH_hisayt[[p]][,2,,nYear,sampleDay[p]]
  N = N_hisyt[[p]][,1,nYear,sampleDay[p]] + N_hisyt[[p]][,2,nYear,sampleDay[p]]
  
  juvAndAdult = matrix(0, nAge[p], 2)
  juvAndAdult[,1] = CH[,1]/N
  juvAndAdult[,2] = rowSums(CH[,2:nChAge])/N
  
  
  matplot(juvAndAdult, type="l", lty=1, 
          ylab="Chinook consumed",
          xlab="Age",
          las=1,
          main=predatorNames[p])
  #if(p == 3)
  #{
  ##  legend(10,max(na.omit(agX),na.omit(agXFem))*0.95,legend=ageNames, lty=1, col=1:nAge, bty="n")
    
  #}
}

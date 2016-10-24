#Calculate the gestation costs.
PC_his = list()
PC_his[[1]] = array(0, c(nAge[1],nSex))

#Harbor seal costs
PC_his[[1]][,2] = 93000000

#Steller sea lion costs, you need to multiply by 1000000 because original calculation is kJ/g and we need J/kg
PC_his[[2]] = array(0, c(nAge[2],nSex))
PC_his[[2]][,2] = mean(wt_his[[2]][1,])*(0.023*39.330 + (1-0.023)*(1-0.759)*(17.99))*1/(0.92*0.2)*1000000

#The are no pupping costs for CSL or  killer whales
PC_his[[3]] = array(0, c(nAge[3],nSex))
PC_his[[4]] = array(0, c(nAge[4],nSex))


#Calculate the lactation costs.
LC_his = list()

LC_his[[1]] = array(0, c(nAge[1],nSex))
#Harbor seal costs
LC_his[[1]][,2] = 24000000*wt_his[[1]][,2]^0.75

#This is a pretty clutchy way to calculate this, but Winship says that the lactation costs equate to the 10% decrease in the efficiency of the acitivity costs.  
#But since the Acitivity costs are daily costs, you need to multiply by the number of lactation days.
LC_his[[2]] = array(0, c(nAge[2],nSex))
LC_his[[2]][,2] = (AC_hist[[2]][,2,1]/((dh_hi[[2]]-0.1)*da_hi[[2]]) - 
                          AC_hist[[2]][,2,1]/((dh_hi[[2]])*da_hi[[2]]))*
                          sum(pLC_hst[[2]][2,])

#There are no lactation costs for killer whales or CSL
LC_his[[3]] = array(0, c(nAge[3],nSex))
LC_his[[4]] = array(0, c(nAge[4],nSex))

#Now combine the lactation and gestation costs into one set of daily costs.
P_hist = list()
for(h in 1:2)
{
  P_hist[[h]] = array(0, c(nAge[h],nSex,nDays))
  
  for(i in 1:nAge[h])
    for(t in 1:nDays)
    {
      P_hist[[h]][i,2,t] = hm_his[[h]][i,2] *
        pF_hs[[h]][2]*
        (PC_his[[h]][i,2]*pPC_hst[[h]][2,t]/sum(pPC_hst[[h]][2,])+
           LC_his[[h]][i,2]*pLC_hst[[h]][2,t]/sum(pLC_hst[[h]][2,]))
      
    }
}

#There are no growth costs for killer whales and CSL
#There are no production costs for killer whales
P_hist[[3]] = array(0, c(nAge[3],nSex,nDays))
P_hist[[4]] = array(0, c(nAge[4],nSex,nDays))

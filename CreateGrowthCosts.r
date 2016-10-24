
#Now calculate the growth costs
GC_his = list()
GC_his[[1]] = array(0, c(nAge[1],nSex))
#Harbor seal costs in joules per day
for(s in 1:nSex)
  GC_his[[1]][,s] = (1-hm_his[[1]][,s])*0.0165*312*86400

GC_his[[2]] = array(0, c(nAge[2],nSex))
for(s in 1:nSex)
{
  GC_his[[2]][,s] = c(wt_his[[2]][2:(nAge[2]),s]-
                             wt_his[[2]][1:(nAge[2]-1),s],0)*
    (0.07*39.330 + (1-0.07)*(1-0.7)*(17.99))*1/(0.92*0.2)*
    1000000/365
  
}

#There are no growth costs for killer whales and CSL
#There are no production costs for killer whales
P_hist[[3]] = array(0, c(nAge[3],nSex,nDays))
P_hist[[4]] = array(0, c(nAge[4],nSex,nDays))
GC_his[[3]] = array(0, c(nAge[3],nSex))
GC_his[[4]] = array(0, c(nAge[4],nSex))


da_hi = list()
dh_hi = list()
Eff_hi = list()

#Assimilation after excretion
da_hi[[1]] = array(rep(0.9,nAge[1]),
                   c(nAge[1]))

da_hi[[2]] = array(c(0.955,rep(0.85,nAge[2])),
                   c(nAge[2])) 
da_hi[[3]] = array(c(0.955,rep(0.85,nAge[3])),
                   c(nAge[3])) 


#Heat loss
dh_hi[[1]] = array(0.0775,
                   c(nAge[1])) 
dh_hi[[2]] = array(0.875,
                   c(nAge[2])) 
dh_hi[[3]] = array(0.875,
                   c(nAge[3])) 

#Efficiency models
Eff_hi[[1]] = array(da_hi[[1]] - dh_hi[[1]], c(nAge[1]))
Eff_hi[[2]] = array(da_hi[[2]] * dh_hi[[2]], c(nAge[2]))
#Efficiency of CSL and Killer whales is already in the Kleiber coefficients
Eff_hi[[3]] = array(da_hi[[2]] * dh_hi[[2]], c(nAge[3]))
Eff_hi[[4]] = array(1, c(nAge[4]))


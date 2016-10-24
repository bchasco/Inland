#Proportion mature at each age by sex
#HS is based on Pritchard and Calkins 1979
#SSL is based on Winship et al. 2002
#CSL is based on Winship steller sea lion estimates.  This doesn't really matter since only adult males are in Puget Sound
HS = as.matrix(read.table("HS_MatAtAge.dat", header=FALSE, skip=2))
SSL = as.matrix(read.table("SSL_MatAtAge.dat", header=FALSE, skip=2))
CSL = as.matrix(read.table("CSL_MatAtAge.dat", header=FALSE, skip=2))
hm_his = list()
hm_his[[1]] = array(HS,
               c(nAge[1],nSex)) 
hm_his[[2]] = array(SSL,
                c(nAge[2],nSex)) 
hm_his[[3]] = array(CSL,
                c(nAge[3],nSex)) 


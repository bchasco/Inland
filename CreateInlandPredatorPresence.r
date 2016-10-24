#Probaiblity that a predator is within the inland waters
#The harbor seal is assumed to present 100% of the time
#CSL and SSL presence is based on the 2014 NAVY surveys provided by steve jeffries.  
#There is a little jiggering that needs to happen because the haulout numbers can spike pretty dramatically.
#So I keep a running average of the haulout numbers across three observations.  See spreadsheet CSLandSSLInlandPresence.xlsx
psi = read.table("psi_ht.dat", header=TRUE)
psi_ht = list()
for(h in 1:nPredator)
{
  psi_ht[[h]] = array(as.matrix(psi[,1+h]))
}

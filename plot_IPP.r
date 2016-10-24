
IPP_output = array(NA,c(8,3,2))

for(i in 1:3)
  for(j in 1:8)
    IPP_output[j,i,1] = log(sum((t(cvs[i,j,9,])/baseNumbers)^2))

for(i in 1:3)
  for(j in 1:8)
    IPP_output[j,i,2] = log(sum((t(cvs[i,j,10,])/baseBiomass)^2))

par(mfcol=c(3,2))
for(i in 1:3)
  barplot(IPP_output[,i,1]/max(IPP_output[,i,1]), names.arg = sensitivityParNames, las=2)


for(i in 1:3)
  barplot(IPP_output[,i,2]/max(IPP_output[,i,2]), names.arg = sensitivityParNames, las=2)

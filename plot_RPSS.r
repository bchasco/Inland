
RPSS_output = array(NA,c(8,3,2))

for(i in 1:3)
  for(j in 1:8)
    RPSS_output[j,i,1] = print(sum((t(cvs[i,j,9,])-sensitivityNumbers)^2))

for(i in 1:3)
  for(j in 1:8)
    RPSS_output[j,i,2] = print(sum((t(cvs[i,j,10,])-sensitivityBiomass)^2))

par(mfrow=c(3,1))
for(i in 1:3)
  barplot(RPSS_output[,i,1]/max(RPSS_output[,i,1]), names.arg = sensitivityParNames)


par(mfrow=c(3,1))
for(i in 1:3)
  barplot(RPSS_output[,i,2]/max(RPSS_output[,i,2]), names.arg = sensitivityParNames)

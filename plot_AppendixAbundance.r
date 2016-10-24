png("plot_AppendixAbundance.png", width=480, height=580, pointsize=12)

N = matrix(unlist(N_hy),nYear,nPredator)

par(mfrow=c(2,2))

for(i in 1:nPredator)
{
  plot(N[,i], main=predatorNames[[i]], ylab="Abundance", xlab="Year", type="l")
}

dev.off()

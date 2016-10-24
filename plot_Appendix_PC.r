png("plot_ProductionCosts.png", width=480, height=580, pointsize=12)


par(mfrow=c(2,2))

for(i in 1:nPredator)
{
  plot(PC_hs[[i]][2,], main=predatorNames[[i]], ylab="Probability of lactating", ylim=c(0,1.05), xlab="Day of year", type="l")
}

dev.off()

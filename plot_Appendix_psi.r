png("plot_Appendix_psi.png", width=480, height=580, pointsize=12)

x = matrix(unlist(psi_ht),nDays,nPredator)

par(mfrow=c(2,2))

for(i in 1:nPredator)
{
  plot(x[,i], main=predatorNames[[i]], ylab="Presence", ylim=c(0,1.05), xlab="Day of year", type="l")
}

dev.off()

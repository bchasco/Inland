png("plot_AppendixPearson.png", width=480, height=480, pointsize=12)

N = matrix(unlist(N_hy),nYear,nPredator)

par(mfrow=c(1,1))

  plot(Pearson4percentDecline, main="Harbor seal", ylab="Abundance", xlab="Year", type="l")

dev.off()

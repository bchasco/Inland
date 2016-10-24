png("plot_smoltSurvival.png", width=780, height=780, pointsize=12)

par(mfrow=c(1,1), mai=c(2,2,2,2))

smolt = read.table("releaseData.dat", header=FALSE)

plot(smolt[,1],
     colSums(Numbers_spy[1:3,1,])/smolt[,2], 
     type="l",
      ylab="Mortality from harbor seals",
      lty=1,
      las=1,
     lwd=2,
     xlab="")

par(new=TRUE)
plot(smolt[,2]/1000000,
     axes=FALSE,
     type="l",
     lty=2,
     ylab="",
     xlab="")
axis(4,las=1)
mtext("Smolt releases (millions)", 4, line=2.5, srt=90)

dev.off()

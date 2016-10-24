tiff("plot_annualConsumption.tiff", width=480, height=580, pointsize=14)

FRAM_ch = array(0,c(nPredator,max(nAge),nYear))

for(y in 1:nYear)
{
    for(p in 1:4)
      for(a in 1:nChAge)
      {
                tmp = sum(CH_hisayt[[p]][,,a,y,])
                FRAM_ch[p,a,y] =  tmp     
      }
}

par(mfrow=c(2,2), mai=c(1,1.,0.4,0))
for(p in 1:4)
{
    matplot(t(FRAM_ch[p,,])/1000, type="l",
            ylab="",
            xlab="",
            axes=FALSE,
            ylim=c(0,max(1,FRAM_ch[p,,])/1000),
            lty=1,
            col=1:5,
            lwd=2)
    axis(1,at=1:nYear, labels=years, cex=1.3)
    text(3,max(t(FRAM_ch[p,,])/1000), paste0("( ", letters[p], " )"), cex=1.3)
    if(p==2) 
      legend(0,max(t(FRAM_ch[p,,])/1000),legend=paste("Ocean age", 0:4), lty=1, lwd=2, col=1:nChAge, bty="n", cex=1.3)
    axis(2)
    box()
}

par(mai=c(0,0,0,0), new=TRUE, fig=c(0,1,0,1))

plot(1, type="n", axes=FALSE, xaxs="i", yaxs="i", xlim=c(0,1), ylim=c(0,1))

text(0.05,0.5, "Number of Chinook consumed (thousands)", srt=90, cex=1.2, )

dev.off()

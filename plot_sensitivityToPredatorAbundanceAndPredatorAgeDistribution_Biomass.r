png("plot_sensitivityToPredatorAbundanceAndPredatoryAgeDistribution_Biomass.png", width=480, height=780, pointsize=12)

par(mfrow=c(3,3))

icnt = 1
for(pa in 1:3)
  for(ad in 1:3)
  {
    if(icnt>1) par(new=TRUE)
    par(fig=c(0.15+(ad-1)/3*0.8,0.15+(ad)/3*0.8,0.05+(pa-1)/3*0.9,0.05+(pa)/3*0.9), mai=c(0,0,0,0))
    barplot((sensivityBiomass[pa,ad,,])/1000, 
        #ylim=c(0,max(sensivityNumbers)/1000000),
        ylim=c(0,2300),
        space=0,
        las=1,
        axes=FALSE,
        border = myCols,
        col = myCols
    )
    if(pa==1) axis(1,at=seq(5,40,5),labels=yearNames[seq(5,40,5)],cex.axis=1.5)
    if(pa!=1) axis(1,at=seq(5,40,5),labels=FALSE, cex.axis=1.5)
    box()
    if(am==1) axis(2, las=1, at=seq(400,2400,400),labels=seq(400,2400,400),cex.axis=1.5)
    if(am!=1) axis(2, las=1, at=seq(400,2400,400),labels=FALSE)
    
    if(pa==2 & ad==2) text(23,1800,"Base case", cex=1.4)
    icnt = icnt + 1
    #text(10,2000,paste("pa", pa, "am", am), cex=1.4)
    
    if(pa==3 & ad==1) 
      legend(0,2000, 
             legend=c("harbor seal", "Steller sea lion", "California sea lion", "killer whale"), 
             pch=15, pt.cex=2.5, cex=1.5,
             col=myCols, bty="n")
  }

par(fig=c(0,1,0,1), mai=c(0,0,0,0), new=TRUE)
plot(1, type="n", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE)

text(0.97,0.2, "pinniped abundance 50%", cex=1.5, srt=90)
text(0.97,0.5, "pinniped abundance 100%", cex=1.5, srt=90)
text(0.97,0.81, "pinniped abundance 150%", cex=1.5, srt=90)

text(0.27,0.98, "Predator activity 50%", cex=1.5)
text(0.55,0.98, "Predator activity 100%", cex=1.5)
text(0.82,0.98, "Predator activity 150%", cex=1.5)

text(0.02,0.5, "Annual Chinook consumption (1000 tons)", cex=1.5, srt=90)

dev.off()

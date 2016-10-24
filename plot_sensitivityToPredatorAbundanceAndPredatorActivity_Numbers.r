png("plot_sensitivityToPredatorAbundanceAndPredatoryActivity_Numbers.png", width=480, height=780, pointsize=12)

par(mfrow=c(3,3))

icnt = 1
for(pa in 1:3)
  for(am in 1:3)
  {
    if(icnt>1) par(new=TRUE)
    par(fig=c(0.1+(am-1)/3*0.85,0.1+(am)/3*0.85,0.05+(pa-1)/3*0.9,0.05+(pa)/3*0.9), mai=c(0,0,0,0))
    barplot((sensivityNumbers[pa,am,,])/1000000, 
        #ylim=c(0,max(sensivityNumbers)/1000000),
        ylim=c(0,45),
        space=0,
        las=1,
        axes=FALSE,
        border = myCols,
        col = myCols
    )
    if(pa==1) axis(1,at=seq(5,40,5),labels=yearNames[seq(5,40,5)],cex.axis=1.5)
    if(pa!=1) axis(1,at=seq(5,40,5),labels=FALSE, cex.axis=1.5)
    box()
    if(am==1) axis(2, las=1, at=seq(10,40,10),labels=seq(10,40,10),cex.axis=1.5)
    if(am!=1) axis(2, las=1, at=seq(10,40,10),labels=FALSE)
    
    if(pa==2 & am==2) text(23,25,"Base case", cex=1.4)
    icnt = icnt + 1
    
    if(pa==3 & am==3) 
      legend(0,40, 
             legend=c("harbor seal", "Steller sea lion", "California sea lion", "killer whale"), 
             pch=15, pt.cex=2.5, cex=1.5,
             col=myCols, bty="n")
  }

par(fig=c(0,1,0,1), mai=c(0,0,0,0), new=TRUE)
plot(1, type="n", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE)

text(0.97,0.2, "pinniped abundance 50%", cex=1.5, srt=90)
text(0.97,0.5, "pinniped abundance 100%", cex=1.5, srt=90)
text(0.97,0.81, "pinniped abundance 150%", cex=1.5, srt=90)

text(0.25,0.98, "Predator activity 50%", cex=1.5)
text(0.5,0.98, "Predator activity 100%", cex=1.5)
text(0.8,0.98, "Predator activity 150%", cex=1.5)

text(0.02,0.5, "Annual Chinook consumption (millions of individuals)", cex=1.5, srt=90)

dev.off()

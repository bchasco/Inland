tiff("plot_pinnipedProjectImpacts_pptx.tiff", width=780, height=780, pointsize=14)
par(mfrow=c(3,2), mai=c(0.4,1.2,0.2,0.5), mgp=c(4,1,0))
rel = read.table("releaseData.dat", header=FALSE)

surv_a = c(0.05,0.43,0.7,0.84,1)
r_a = c(0,0.02,0.45,0.85,1)

smoltProd = c(rel[,2],rep(rel[nrow(rel),2],2))

ocean = matrix(0,nYear,5)
ret = array(0,c(3,nYear,5))
Adams = matrix(0,nYear,5)

myCols = 2:5

#harbor seals
ocean[,1] = t(colSums(Numbers_spy[,1,]))
for(y in 1:nYear)
{
  for(a in 2:5)
  {
      ocean[y,a] = ocean[y,a-1] * surv_a[a-1] * (1-r_a[a])
      ret[1,y,a] = ocean[y,a-1] * surv_a[a-1] * (r_a[a]) 
  }
}

fishArrays = array(0,c(2,length(1970:2015),5))
age = read.csv("agecomp_total.csv")

PScatches = read.table("PS_fisherycatches.dat", header=TRUE)
comm = PScatches[PScatches[,1]=="comm",]
for(y in comm[,2])
{
  fishArrays[1,y-1969,] = t(comm[comm[,2]==y,3]*age[age[,2]==y,3:7]/sum(age[age[,2]==y,3:7]))
}

rec = PScatches[PScatches[,1]=="rec",]
for(y in rec[,2])
{
  fishArrays[2,y-1969,] = t(rec[rec[,2]==y,3]*age[age[,2]==y,3:7]/sum(age[age[,2]==y,3:7]))
}

ymax = max(apply(ret,c(1,2), sum), apply(fishArrays,c(1,2),sum))/1000

barplot(t(ret[1,,2:5]/1000),
        axes=FALSE,
        ylab="",
        lty=2, 
        lwd=2, 
        las=1,
        xaxs="i",
        yaxs="i",
        space=0,
        ylim=c(0.,ymax),
        col=myCols,
        border=myCols)
text(7, ymax*0.9, "( a )", cex =2)
axis(1, at=1:nYear, labels=years, cex.axis=2)
axis(2, las=1, cex.axis=2)
box()

legend(1, max(rowSums(ret[1,,2:5]*6.9/1.5))/1000,
     legend=paste('Ocean age', 1:4), pch=15, col=myCols, bty="n")


#Stellers
ocean[,1] = t(colSums(Numbers_spy[,2,]))
for(y in 1:nYear)
{
  for(a in 2:5)
  {
    ocean[y,a] = ocean[y,a-1] * surv_a[a-1] * (1-r_a[a])
    ret[2,y,a] = ocean[y,a-1] * surv_a[a-1] * (r_a[a]) 
  }
}

barplot(t(ret[2,,2:5]/1000), 
        ylab="",
        lty=2, 
        lwd=2, 
        las=1,
        xaxs="i",
        yaxs="i",
        axes=FALSE,
        space=0,
        ylim=c(0,ymax),
        col=myCols,
        border=myCols)
text(7, ymax*0.9, "( b )", cex =2)
legend(15,ymax,legend=paste("Ocean age ",1:4), bty="n", pch=15, cex=2, pt.cex=3, col=2:5)
axis(1, at=1:nYear, labels=years, cex.axis=2)
axis(2, las=1, cex.axis=2)
box()

#California
ocean[,1] = t(colSums(Numbers_spy[,3,]))
for(y in 1:nYear)
{
  for(a in 2:5)
  {
    ocean[y,a] = ocean[y,a-1] * surv_a[a-1] * (1-r_a[a])
    ret[3,y,a] = ocean[y,a-1] * surv_a[a-1] * (r_a[a]) 
  }
}

barplot(t(ret[3,,2:5]/1000), 
        ylab="",
        lty=2, 
        lwd=2, 
        las=1,
        xaxs="i",
        yaxs="i",
        axes=FALSE,
        space=0,
        ylim=c(0,ymax),
        col=myCols,
        border=myCols)
text(7, ymax*0.9, "( c )", cex =2)
axis(1, at=1:nYear, labels=years, cex.axis=2)
axis(2, las=1, cex.axis=2)
box()

#Killer whale

barplot(FRAM_ch[4,2:5,]/1000, 
        ylab="",
        lty=2, 
        lwd=2, 
        las=1,
        xaxs="i",
        yaxs="i",
        axes=FALSE,
        space=0,
        ylim=c(0,ymax),
        col=myCols,
        border=myCols)
text(7, ymax*0.9, "( d )", cex =2)
axis(1, at=1:nYear, labels=years, cex.axis=2)
axis(2, las=1, cex.axis=2)
box()





barplot(t(fishArrays[1,,2:5])/1000, 
        ylab="",
        lty=2, 
        lwd=2, 
        las=1,
        xaxs="i",
        yaxs="i",
        axes=FALSE,
        space=0,
        ylim=c(0,ymax),
        col=myCols,
        border=myCols)
text(7, ymax*0.9, "( e )", cex =2)
axis(1, at=1:nYear, labels=years, cex.axis=2)
axis(2, las=1, cex.axis=2)
box()

barplot(t(fishArrays[2,,2:5])/1000, 
        ylab="",
        lty=2, 
        lwd=2, 
        las=1,
        xaxs="i",
        yaxs="i",
        axes=FALSE,
        space=0,
        ylim=c(0,ymax),
        col=myCols,
        border=myCols)
text(7, ymax*0.9, "( f )", cex =2)
axis(1, at=1:nYear, labels=years, cex.axis=2)
axis(2, las=1, cex.axis=2)
box()

par(new=TRUE, mai=c(0,0,0,0), fig=c(0,1,0,1))
plot(1, xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", type="n", axes=FALSE)
mtext("Chinook mortality (thousands)",2,-3, cex=1.5)
dev.off()

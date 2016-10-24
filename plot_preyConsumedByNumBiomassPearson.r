png("plot_preyConsumedByNumBiomassPearson.png", width=480, height=780, pointsize=12)

#Number consumed by age, predator
par(mfcol=c(3,2), mai=c(0.5,1.1,0.4,0.2))
#Numbers consumed by each predator
Numbers_spy = array(0,c(3,nPredator,nYear))

#These weights are from Ford et al. 1989, except for smolts
wtCH_a = c(20/1000,1.2,3.7,8.1,10.5)

myCols = c("lightblue","orange", "black", "darkgrey")
maxNum = 0
for(s in 1:3)
{
    for(y in 1:nYear)
    {  
      for(p in 1:nPredator)
      {
        if(p==1)
          Numbers_spy[s,p,y] = Numbers_spy[s,p,y] + sum(CH_hisayt[[p]][,,,y,FramSeasons[[s]]]) * Pearson4percentDecline[y]/N_hy[[1]][y]

        if(p!=1)  
          Numbers_spy[s,p,y] = Numbers_spy[s,p,y] + sum(CH_hisayt[[p]][,,,y,FramSeasons[[s]]])
      }
      if(sum(Numbers_spy[s,,y])>maxNum) maxNum = sum(Numbers_spy[s,,y])
    }
    
}
for(s in 1:3)
{
    barplot(Numbers_spy[s,,]/1000, 
    col=myCols, 
    border=myCols, 
    ylim=c(0,maxNum/1000*1.1),
    yaxs="i", xaxs="i",
    space=0,
    las=1,cex.axis=1.3)
  axis(1, at=1:nYear, labels=years, cex.axis=1.3)
  if(s==1) mtext("Numbers (000)", side=3, line=2, cex.lab=1.3)
  mtext(FramNames[s], side=2, line=5, cex.lab=1.3)
  box()
}

maxBio = 0
#Biomass consumed by each predator
Biomass_spy = array(0,c(3,nPredator,nYear))
for(s in 1:3)
{
    for(y in 1:nYear)
    {  
      for(p in 1:nPredator)
        for(a in 1:nChAge)
        {

          if(p==1)
          {
            if(a==1)
              Biomass_spy[s,p,y] = Biomass_spy[s,p,y] + Pearson4percentDecline[y]/N_hy[[1]][y]*sum(CH_hisayt[[p]][,,a,y,FramSeasons[[s]]])*0.0000049*mean(Len[unlist(FramSeasons[[s]]),2])^3.14
            if(a>1)
              Biomass_spy[s,p,y] = Biomass_spy[s,p,y] + Pearson4percentDecline[y]/N_hy[[1]][y]*sum(CH_hisayt[[p]][,,a,y,FramSeasons[[s]]])*wtCH_a[a]
          }
          if(p!=1)
          {
            if(a==1)
              Biomass_spy[s,p,y] = Biomass_spy[s,p,y] + sum(CH_hisayt[[p]][,,a,y,FramSeasons[[s]]])*0.0000049*mean(Len[unlist(FramSeasons[[s]]),2])^3.14
            if(a>1)
              Biomass_spy[s,p,y] = Biomass_spy[s,p,y] + sum(CH_hisayt[[p]][,,a,y,FramSeasons[[s]]])*wtCH_a[a]
          }
        }
      if(sum(Biomass_spy[s,,y])>maxBio) maxBio = sum(Biomass_spy[s,,y])
    }
}

for(s in 1:3)
{
    barplot(Biomass_spy[s,,]/1000, 
          col=myCols, 
          border=myCols, 
          ylim=c(0,maxBio/1000*1.1),
          yaxs="i", xaxs="i",
          space=0,
          las=1,cex.axis=1.3)
  axis(1, at=1:nYear, labels=years, cex.axis=1.3)
  if(s==1) 
    mtext("Metric tons", 
          side=3, line=2, cex.lab=1.3)
  if(s==1) 
    legend(0,maxBio/1000*1.1, 
           legend=c("harbor seal", "Steller sea lion", "California sea lion", "killer whale"), 
           pch=15, pt.cex=2.5, cex=1.5,
           col=myCols, bty="n")
  box()
}

dev.off()
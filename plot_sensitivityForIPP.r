#These are the labels for the barplots
myNames = c(
  expression(paste("Salmon condition ",alpha)),
  expression(paste("Salmon condition ",beta)),
  expression(paste("Smolt length ")),
  expression(paste("HS Abundance ")),
  expression(paste("HS Age-0 fraction ")),
  expression(paste("HS Chinook fraction ")),
  expression(paste("HS Kleiber ",alpha)),
  expression(paste("HS mass ")),
  expression(paste("CSL Abundance ")),
  expression(paste("CSL Age-0 fraction ")),
  expression(paste("CSL Chinook fraction ")),
  expression(paste("CSL Kleiber ",alpha)),
  expression(paste("CSL mass ")),
  expression(paste("SSL Abundance ")),
  expression(paste("SSL Age-0 fraction ")),
  expression(paste("SSL Chinook fraction ")),
  expression(paste("SSL Kleiber ",alpha)),
  expression(paste("SSL mass ")),
  expression(paste("KW Chinook fraction ")),
  expression(paste("KW Kleiber ",alpha)),
  expression(paste("KW mass "))
)
length(myNames)
icnt = 1
cvsout = IPPcvs

#You need to reorganize the parameter names to get them in a sensible order
parNamesOrg = names(table(cvsout[,1]))
parNamesOrg = c(parNamesOrg[1:2], parNamesOrg[16], parNamesOrg[8:12],parNamesOrg[3:7],parNamesOrg[17:21],parNamesOrg[13:15])

#This stores the summary statistics for the tests
summary_cvs = array(NA, c(3*21,4))

#Number in column 3, and biomass in column 4
for(i in c(0.02,0.1,0.2)){
  for(j in parNamesOrg) {
    summary_cvs[icnt,] = c(j,i,
                           sd(log(cvsout[cvsout[,1]==j & cvsout[,2]==i,6])),
                           sd(log(cvsout[cvsout[,1]==j & cvsout[,2]==i,7])))
    icnt = icnt +1 
  }
}

myCols = c("green",'green',rep("lightblue",5),rep("orange",5),rep("black",5),rep("grey",3))


png('plot_sensitivityForIPP.png', width=600, height=800, pointsize=16)

par(mfrow=c(3,2), mai=c(1.75,1,0.1,0))
icnt = 1
useBetaOneill=FALSE
for(i in c(0.02,0.1,0.2)){
  if(useBetaOneill==FALSE)
  {
    tmp = summary_cvs[summary_cvs[,2]==i & summary_cvs[,1]!="betaOneill",]
    #tmp = summary_cvs[summary_cvs[,2]==i,]
    barplot((as.numeric(t(tmp[,3]))), col=myCols, border=myCols, names.arg = myNames[parNamesOrg!="betaOneill"], las=2, ylim=as.numeric(c(0,max(summary_cvs[summary_cvs[,1]!="betaOneill",3]))))
    text(20,
         as.numeric(max(summary_cvs[summary_cvs[,1]!="betaOneill",3]))*0.75,
         paste("( ",letters[icnt],")"),
         cex=1.3)
    icnt = icnt + 1
    
    barplot((as.numeric(t(tmp[,4]))), 
            col=myCols, 
            border=myCols, 
            names.arg = myNames[parNamesOrg!="betaOneill"], 
            las=2, 
            ylim=as.numeric(c(0,max(summary_cvs[summary_cvs[,1]!="betaOneill",4]))))
    
    text(20,
         as.numeric(max(summary_cvs[summary_cvs[,1]!="betaOneill",4]))*.75,
         paste("( ",letters[icnt],")"),
         cex=1.3)
    icnt = icnt + 1
  }
  
  if(useBetaOneill==TRUE)
  {
    tmp = summary_cvs[summary_cvs[,2]==i,]
    barplot((as.numeric(t(tmp[,3]))), col=myCols, names.arg = myNames[], las=2, ylim=as.numeric(c(0,max(summary_cvs[,3]))))
    text(20,as.numeric(max(summary_cvs[,3]))*0.75,paste("( ",letters[icnt],")"))
    icnt = icnt + 1
    barplot((as.numeric(t(tmp[,4]))), col=myCols, names.arg = myNames[], las=2, ylim=as.numeric(c(0,max(summary_cvs[,4]))))
    text(20,as.numeric(max(summary_cvs[,4]))*.75,paste("( ",letters[icnt],")"))
    icnt = icnt + 1
  }
}  

dev.off()
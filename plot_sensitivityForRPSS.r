myNames = c(
  expression(paste("Salmon condition ",alpha)),
  expression(paste("Smolt length")),
  expression(paste("HS Abundance")),
  expression(paste("HS Age-0 fraction")),
  expression(paste("HS Chinook fraction")),
  expression(paste("HS Kleiber ",alpha)),
  expression(paste("HS mass")),
  expression(paste("CSL Abundance")),
  expression(paste("CSL Age-0 fraction")),
  expression(paste("CSL Chinook fraction ")),
  expression(paste("CSL Kleiber ",alpha)),
  expression(paste("CSL mass")),
  expression(paste("SSL Abundance")),
  expression(paste("SSL Age-0 fraction")),
  expression(paste("SSL Chinook fraction")),
  expression(paste("SSL Kleiber ",alpha)),
  expression(paste("SSL mass")),
  expression(paste("KW Chinook fraction")),
  expression(paste("KW Kleiber ",alpha)),
  expression(paste("KW mass"))
)

#I always like to make a copy of the output in case you accidently delete the output.
simDevs_out = simDevs

#Get the parameters in the correct order
parOrder = c(20,19,9,12,15,1,5,11,14,17,3,7,10,13,17,2,6,18,4,8)

#Output
RPSStest =  data.frame(array(NA, c(3*20,4)))
RPSStest[,1] = rep(sensitivityParNames[parOrder],3)
RPSStest[,2] = rep(sensivityCVRange,each = length(sensitivityParNames))

#Numbers
icnt = 1
for(s in 1:3)
{
  lm_All = anova(lm(log(simDevs_out[s,,21])~simDevs_out[s,,]))[1,2]
  for(i in parOrder)
  {
    #This is residual sums of squares after removing an input, sum_i (Y_i - Y_hat)^2
    #There's an easiear way to do using 
    #sum(lm(log(simDevs_out[s,,21])~simDevs_out[s,,parOrder[parOrder!=i]])$residuals^2)
    #but it looks more clunky
    RPSStest[icnt,3] = 
                round((lm_All - anova(lm(log(simDevs_out[s,,21])~simDevs_out[s,,parOrder[parOrder!=i]]))[1,2])/lm_All,5)
      icnt = icnt + 1
  }
  
}

#Biomass
icnt = 1
for(s in 1:3)
{
  lm_All = anova(lm(log(simDevs_out[s,,22])~1))[1,2]
  for(i in parOrder)
  {
    RPSStest[icnt,4] = 
      round((lm_All - anova(lm(log(simDevs_out[s,,22])~simDevs_out[s,,parOrder[parOrder!=i]]))[1,2])/lm_All,5)
    icnt = icnt + 1
  }
  
}


icnt = 1
myCols = c("green",'green',rep("lightblue",5),rep("orange",5),rep("black",5),rep("grey",3))
myCols_wBeta = c("green",'green','green', rep("lightblue",5),rep("orange",5),rep("black",5),rep("grey",3))

png('plot_sensitivityForRPSS.png', width=600, height=800, pointsize=16)
#Numbers in the first col, Biomass in the second
par(mfrow=c(3,2), mai=c(1.75,1,0.1,0))
icnt = 1
useBetaOneill=FALSE
for(i in c(0.02,0.1,0.2)){
  if(useBetaOneill==FALSE)
  {
    tmp = RPSStest[RPSStest[,2]==i & RPSStest[,1]!="betaOneill",]
    #tmp = RPSStest[RPSStest[,2]==i,]
    barplot((as.numeric(t(tmp[,3]/sum(tmp[,3])))),
            border=myCols,
            col=myCols, 
            axes=FALSE,
            names.arg = myNames, 
            las=2, 
            ylim=as.numeric(c(0,max(RPSStest[RPSStest[,1]!="betaOneill",3]))))
    axis(2,las=1, at=c(0,0.1,0.2,0.3,0.4))
    abline(h=0)
        text(20,
         as.numeric(max(RPSStest[RPSStest[,1]!="betaOneill",3]))*0.75,
         paste("( ",letters[icnt],")"),
         cex=1.3)

    icnt = icnt + 1
    
    barplot((as.numeric(t(tmp[,4]/sum(tmp[,4])))), 
            col=myCols, 
            border=myCols,
            names.arg = myNames, 
            axes=FALSE,
            las=2, 
            ylim=as.numeric(c(0,max(RPSStest[RPSStest[,1]!="betaOneill",4]))))
    
    axis(2,las=1, at=c(0,0.1,0.2,0.3,0.4))
    abline(h=0)
    text(20,
         as.numeric(max(RPSStest[RPSStest[,1]!="betaOneill",4]))*.75,
         paste("( ",letters[icnt],")"),
         cex=1.3)
    icnt = icnt + 1
  }
  
  if(useBetaOneill==TRUE)
  {
    tmp = RPSStest[RPSStest[,2]==i,]
    barplot((as.numeric(t(tmp[,3]))), 
            col=myCols_wBeta, 
            border=myCols_wBeta, 
            names.arg = myNames[], 
            las=2, 
            ylim=as.numeric(c(0,max(RPSStest[,3]))))
    text(20,as.numeric(max(RPSStest[,3]))*0.75,paste("( ",letters[icnt],")"))
    icnt = icnt + 1
    barplot((as.numeric(t(tmp[,4]))), 
            col=myCols, 
            border=myCols, 
            names.arg = myNames[], 
            las=2, 
            ylim=as.numeric(c(0,max(RPSStest[,4]))))
    text(20,as.numeric(max(RPSStest[,4]))*.75,paste("( ",letters[icnt],")"))
    icnt = icnt + 1
  }
}  

par(new=TRUE, mai=c(0,0,0,0), fig=c(0,1,0,1))
plot(1, type="n", axes=FALSE, xaxs="i", yaxs="i", ylim=c(0,1), xlim=c(0,1))
mtext("Relative partial sums-of-squares", 2, -2)
dev.off()

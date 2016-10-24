DailyEnergyDemand = matrix(NA,4,2)
par(mfrow=c(2,2))
for(i in 1:nPredator)
{
  DailyEnergyDemand[i,] = (P_hist[[i]][nAge[i],,1] + AC_hist[[i]][nAge[i],,1])/ Eff_hi[[i]][nAge[i]] /4.184/1000
}
row.names(DailyEnergyDemand) = predatorNames
DailyEnergyDemand = as.data.frame(round(DailyEnergyDemand,0))

names(DailyEnergyDemand) = sexNames
print(DailyEnergyDemand)

JuvDailyConsumption = rbind(CH_hisayt[[1]][12,,1,30,140]/N_hisyt[[1]][12,,30,140],
      CH_hisayt[[2]][15,,1,30,25]/N_hisyt[[2]][15,,30,25],
      CH_hisayt[[3]][9,,1,30,60]/N_hisyt[[3]][9,,30,60],
      CH_hisayt[[4]][25,,1,30,200]/N_hisyt[[4]][25,,30,200])

AdultDailyConsumption = rbind(rowSums(CH_hisayt[[1]][12,,2:5,30,140])/N_hisyt[[1]][12,,30,140],
                              rowSums(CH_hisayt[[2]][15,,2:5,30,25])/N_hisyt[[2]][15,,30,25],
                              rowSums(CH_hisayt[[3]][9,,2:5,30,60])/N_hisyt[[3]][9,,30,60],
                              rowSums(CH_hisayt[[4]][25,,2:5,30,200])/N_hisyt[[4]][25,,30,200])


print(JuvDailyConsumption)
print(AdultDailyConsumption)
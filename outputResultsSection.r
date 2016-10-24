#Results comparing the numbers and ratios of Chinook consumed

print(paste("Harbor seal ", years[1], sum(CH_hisayt[[1]][,,,1,])))
print(paste("Killer whale ", years[1], sum(CH_hisayt[[4]][,,,1,])))
print(paste("Harbor seal ", years[30], sum(CH_hisayt[[1]][,,,30,])))
print(paste("Killer whale ", years[30], sum(CH_hisayt[[4]][,,,30,])))
print(paste("Harbor seal ", years[46], sum(CH_hisayt[[1]][,,,46,])))
print(paste("Killer whale ", years[46], sum(CH_hisayt[[4]][,,,46,])))
print(paste("Ratio ", years[1], sum(CH_hisayt[[1]][,,,1,])/sum(CH_hisayt[[4]][,,,1,])))
print(paste("Ratio ", years[30], sum(CH_hisayt[[1]][,,,30,])/sum(CH_hisayt[[4]][,,,30,])))
print(paste("Ratio ", years[46], sum(CH_hisayt[[1]][,,,46,])/sum(CH_hisayt[[4]][,,,46,])))
print(paste("SSL ", years[46], sum(CH_hisayt[[2]][,,,46,])))
print(paste("CSL ", years[46], sum(CH_hisayt[[3]][,,,46,])))


#Weight of Chinook consumed
print(paste("KW ratio of biomass to pinnipeds", FramSeasons[2], years[1], sum(Biomass_spy[2,4,1])/sum(Biomass_spy[2,1:3,1])))
print(paste("KW ratio of biomass to pinnipeds", FramSeasons[2], years[30], sum(Biomass_spy[2,4,30])/sum(Biomass_spy[2,1:3,30])))
print(paste("KW ratio of biomass to pinnipeds", FramSeasons[2], years[46], sum(Biomass_spy[2,4,46])/sum(Biomass_spy[2,1:3,46])))


#Seasonal consumption
print(paste("KW spring consumption", FramSeasons[1:2], years[46], sum(Biomass_spy[1:2,4,46])))
print(paste("KW winter consumption", FramSeasons[3], years[46], sum(Biomass_spy[3,4,46])))
print(paste("sea lion spring consumption", FramSeasons[3], years[46], sum(Biomass_spy[3,2:3,46])))
print(paste("sea lion winter consumption", FramSeasons[2], years[46], sum(Biomass_spy[2,2:3,46])))
print(paste("harbor seal consumption", names(FramSeasons[1]), years[46], Biomass_spy[1,1,46]/length(FramSeasons[[1]])*30/1000))
print(paste("harbor seal consumption", names(FramSeasons[2]), years[46], Biomass_spy[2,1,46]/length(FramSeasons[[2]])*30/1000))
print(paste("harbor seal consumption", names(FramSeasons[3]), years[46], Biomass_spy[3,1,46]/length(FramSeasons[[3]])*30/1000))


#Pinniped effects on the future.
print(paste("harbor seal juvenile consumption", years[1], sum(CH_hisayt[[1]][,,1,1,])))
print(paste("harbor seal juvenile consumption", years[1], sum(CH_hisayt[[1]][,,2:5,1,])))
print(paste("harbor seal adult consumption", years[46], sum(CH_hisayt[[1]][,,1,30,])))
print(paste("harbor seal adult consumption", years[46], sum(CH_hisayt[[1]][,,2:5,30,])))

print(paste("sea lion juvenile consumption", years[1], sum(CH_hisayt[[2]][,,1,1,]) + sum(CH_hisayt[[3]][,,1,1,])))
print(paste("sea lion juvenile consumption", years[46], sum(CH_hisayt[[2]][,,1,46,]) + sum(CH_hisayt[[3]][,,1,46,])))
print(paste("sea lion adult consumption", years[1], sum(CH_hisayt[[2]][,,2:5,1,]) + sum(CH_hisayt[[3]][,,2:5,1,])))
print(paste("sea lion adult consumption", years[46], sum(CH_hisayt[[2]][,,2:5,46,]) + sum(CH_hisayt[[3]][,,2:5,46,])))

print(paste("Project harbor seal consumption of adults ", sum(ret[1,46,])))
print(paste("Project steller consumption of adults ", sum(ret[2,46,])))
print(paste("Project california seal consumption of adults ", sum(ret[3,46,])))

print(paste("Project all pinniped consumption of adults ", years[1], sum(ret[1:3,1,])))
print(paste("Project all pinniped consumption of adults ", years[30], sum(ret[1:3,30,])))
print(paste("Project all pinniped consumption of adults ", years[46], sum(ret[1:3,46,])))

print(paste("Project all pinniped consumption of adults ", years[46], sum(ret[1:3,46,])/sum(Numbers_spy[,4,46])))


#Smolt survival
print(paste("Smolt survival ", (colSums(Numbers_spy[1:3,1,])/smolt[,2])[c(1,34,46)]))


#Abstract
#Pinniped effects on the future.
print(paste("pinniped consumption", years[1], sum(CH_hisayt[[1]][,,,1,])+sum(CH_hisayt[[2]][,,,1,])+sum(CH_hisayt[[3]][,,,1,])))
print(paste("pinniped consumption", years[46], sum(CH_hisayt[[1]][,,,46,])+sum(CH_hisayt[[2]][,,,46,])+sum(CH_hisayt[[3]][,,,46,])))
print(paste("pinniped consumption", years[1], sum(CH_hisayt[[4]][,,,1,])))
print(paste("pinniped consumption", years[46], sum(CH_hisayt[[4]][,,,46,])))

print(paste("pinniped annual biomass", years[1], sum(Biomass_spy[,1,c(1)] + Biomass_spy[,2,c(1)] + Biomass_spy[,3,c(1)])))
print(paste("pinniped annual biomass", years[46], sum(Biomass_spy[,1,c(46)] + Biomass_spy[,2,c(46)] + Biomass_spy[,3,c(46)])))
print(paste("killer whale annual biomass", years[1], sum(Biomass_spy[,4,1])))
print(paste("killer whale annual biomass", years[46], sum(Biomass_spy[,4,46])))

print(paste("kw/pinniped spring/summer biomass", years[1], sum(Biomass_spy[1:2,4,1])/sum(Biomass_spy[1:2,1,c(1)] + Biomass_spy[1:2,2,c(1)] + Biomass_spy[1:2,3,c(1)])))
print(paste("kw/pinniped spring/summer ", years[1], sum(Biomass_spy[1:2,4,46])/sum(Biomass_spy[1:2,1,c(46)] + Biomass_spy[1:2,2,c(46)] + Biomass_spy[1:2,3,c(46)])))

print(paste("kw spring/summer biomass", years[1], sum(Biomass_spy[1:2,4,46])))
print(paste("kw fall winter ", years[1], sum(Biomass_spy[3,4,46])))

print(paste("pinniped spring/summer biomass", years[1], sum(Biomass_spy[1:2,2:3,46])))
print(paste("pinniped fall winter ", years[1], sum(Biomass_spy[3,2:3,46])))

print(paste("harbor seal biomass", years[46], sum(Biomass_spy[,1,46])))
print(paste("harbor seal biomass", years[1], sum(Biomass_spy[,1,1])))

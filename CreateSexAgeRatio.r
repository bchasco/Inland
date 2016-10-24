#The is different for every species.

#Stage-based fractions for the predator populations by year and sex.
#HS = read.table("StageRatios.dat", header=FALSE, skip=3, nrow=1)
#SSL = read.table("StageRatios.dat", header=FALSE, skip=7, nrow=1)
#CSL = read.table("StageRatios.dat", header=FALSE, skip=11, nrow=1)

#Stage ratios by predator h, stage k, year y
#hk_hky = list()
#hk_hky[[1]] = as.matrix(HS)
#hk_hky[[2]] = as.matrix(SSL)
#hk_hky[[3]] = as.matrix(CSL)
#hk_hky[[4]] = NA

#Sex ratios for the predator populations by year and stage.
#HS = read.table("SexRatios.dat", header=FALSE, skip=3, nrow=2)
#SSL = read.table("SexRatios.dat", header=FALSE, skip=8, nrow=2)
#CSL = read.table("SexRatios.dat", header=FALSE, skip=13, nrow=2)
#hs_hsky = list()
#hs_hsky[[1]] = array(as.matrix(HS/sum(HS)),
#                     c(length(sexNames), length(stageNames[[1]]),nYear))
#hs_hsky[[2]] = array(as.matrix(SSL/sum(SSL)),
#                     c(length(sexNames), length(stageNames[[2]]),nYear))
#hs_hsky[[3]] = array(as.matrix(CSL/sum(CSL)),
#                     c(length(sexNames), length(stageNames[[3]]),nYear))


#Create the arrays that map stage-based activities to age based acitivities using the mortality
hk_hiks = list()
for(h in 1:3)
{
  hk_hiks[[h]] = array(0,c(nAge[h],length(stageNames[[h]]),length(sexNames)))
  for(s in 1:nSex)
  {
    hk_hiks[[h]][1,1,] = 1
  }
  for(i in 2:(nAge[h]))
  {
    hk_hiks[[h]][i,2,] = 1 - hm_his[[h]][i,]
    hk_hiks[[h]][i,3,] = hm_his[[h]][i,]
  }
}

#Beginning with harbor seals 
#The following website has some information about sex and age.
#https://www.eopugetsound.org/sites/default/files/features/resources/harbor%20seal%20EoPS%20Species%20Profile%20Final%20June%2024,%202014.pdf
#This assume differential survival and 50/50 pup ratio
#Zier and Gaydos 2014, and Bigg 1969
#Create the arrays that map stage-based activities to age based acitivities using the mortality
HS_sexAgeRatio = read.table("SexAgeRatios_HS.dat")
HS_sexAgeRatio = HS_sexAgeRatio/sum(HS_sexAgeRatio)
hi_hisy = list()
hi_hisy[[1]] = array(0,c(nAge[1],nSex,nYear)) 
for(y in 1:nYear)
{
  hi_hisy[[1]][,,y] = as.matrix(HS_sexAgeRatio)
}

#Based on Winship et al. 2002
#Create the arrays that map stage-based activities to age based acitivities using the mortality
SSL_sexAgeRatio = read.table("SexAgeRatios_SSL.dat")
SSL_sexAgeRatio = SSL_sexAgeRatio/sum(SSL_sexAgeRatio)
hi_hisy[[2]] = array(0,c(nAge[2],nSex,nYear)) 
for(y in 1:nYear)
{
  hi_hisy[[2]][,,y] = as.matrix(SSL_sexAgeRatio)
}


#Based on the SSL survivals in Winship et al. 2002
#We assume that there are no females and there are no juvenile CSL
CSL_sexAgeRatio = read.table("SexAgeRatios_CSL.dat")
CSL_sexAgeRatio = CSL_sexAgeRatio/sum(CSL_sexAgeRatio)
hi_hisy[[3]] = array(0,c(nAge[3],nSex,nYear)) 
for(y in 1:nYear)
{
  hi_hisy[[3]][,,y] = as.matrix(CSL_sexAgeRatio)
}


#Killer whales are a little different.  We have age-specific data.  No transformations needed.
KW_Male_age_sex_year = read.table("KW_Male_sex_age_year.dat", header=FALSE)
KW_Fem_age_sex_year = read.table("KW_Fem_sex_age_year.dat", header=FALSE)
hi_hisy[[4]] = array(0,
    c(nAge[4],nSex,nYear))
hi_hisy[[4]][,1,] = t(KW_Male_age_sex_year)
hi_hisy[[4]][,2,] = t(KW_Fem_age_sex_year)

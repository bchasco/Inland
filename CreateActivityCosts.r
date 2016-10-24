#Calculate the daily activity costs by age
AC_hist = list()

for(h in 1:3) #predators
{
  AC_hist[[h]] = array(0,
                       c(nAge[h],nSex,nDays))
  for(b in 1:nSeason) #seasons
  {
    for(s in 1:nSex)  #sexes
    {
      for(i in 1:nAge[h])  #ages
      {
        for(j in 1:length(activityNames[[h]])) #activities
        {  
          AC_hist[[h]][i,s,] = AC_hist[[h]][i,s,] +
                colSums(t(pb_hbst[[h]][b,s,] %o% hk_hiks[[h]][i,,s]) * 
                (fa_hbkjs[[h]][b,,j,s] * 
                aa_hbkjs[[h]][b,,j,s] *
                wt_his[[h]][i,s]^bb_hbkjs[[h]][b,,j,s]))
          #}# end activity stage
        }#end activity
      }#end ages
    }#end sex
  }#end season
}#end predators      

for(h in 4:4)
{
  AC_hist[[h]] = array(0,
                  c(nAge[h],nSex,nDays)) 
  for(b in 1:nSeason) #seasons
  {
    for(s in 1:nSex)  #sexes
    {
      for(i in 1:nAge[4])  #ages
      {
        for(j in 1:length(activityNames[[h]])) #activities
        {  
          AC_hist[[h]][i,s,] = AC_hist[[h]][i,s,] +
            pb_hbst[[h]][b,s,] * #breed season
            fa_hbkjs[[h]][b,k,j,s] * #fraction of time
            aa_hbkjs[[h]][b,k,j,s] * #activity multiplier
            wt_his[[h]][i,s]^bb_hbkjs[[h]][b,k,j,s] 
        }#end activity
      }#end ages
    }#end sex
  }#end season
}


E_hisyt = list()
for(h in 1:4) #season
{
  E_hisyt[[h]] = array(NA,
                  c(nAge[h], nSex, nYear, nDays))
  for(y in 1:nYear)
  {
    for(i in 1:(nAge[h]))
    {
      E_hisyt[[h]][i,,y,] = N_hisyt[[h]][i,,y,]*
        (P_hist[[h]][i,,] + AC_hist[[h]][i,,])/
        Eff_hi[[h]][i]
      if(noHSorSSL_Lactaion==TRUE)
      {
        E_hisyt[[h]][i,,y,] = N_hisyt[[h]][i,,y,]*
          (AC_hist[[h]][i,,])/
          Eff_hi[[h]][i]
      }
    }# end age
  }# end year
}#end predator


if(sum(sel[sel[,1]==1,4:8])==4)
{
  print(paste("All of the Chinook selectivities sum to 1"))
  print(cbind(sel[sel[,1]==1,1:3],round(sel[sel[,1]==1,4:8],2)))
}
if(sum(sel[sel[,1]==1,4:8]==4))
  print(paste("One of the Chinook selectivities does not sum to 1"))

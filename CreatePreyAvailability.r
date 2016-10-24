#Read in the probability of Chinook being present in Puget Sound.
vartheta = read.table("vartheta.dat", header=TRUE)
vartheta = vartheta[,2:6]

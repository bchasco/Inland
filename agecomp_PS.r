
d = read.csv("SPS PSChinook_Nov06_2012.csv")

d = d[,c("Year",  "Spawners", "Common.Population.Name", "Age.1.Returns", "Age.2.Returns", "Age.3.Returns", "Age.4.Returns", "Age.5.Returns", "Age.6.Returns","Age.7.Returns")]
d = d[-which(d$Age.3.Returns < 0 | d$Age.4.Returns < 0 | d$Age.5.Returns < 0),]

names(d)[4:10] = paste0("Age",1:7)

# calculate average age comp by year, weighted by spawning pop size
library(dplyr)
summary = group_by(d, Year) %>% 
mutate(totSpawn = sum(Spawners)) %>%
summarize("age1" = sum(Age1 * Spawners)/totSpawn[1],
"age2" = sum(Age2 * Spawners)/totSpawn[1],
"age3" = sum(Age3 * Spawners)/totSpawn[1],
"age4" = sum(Age4 * Spawners)/totSpawn[1],
"age5" = sum(Age5 * Spawners)/totSpawn[1],
"age6" = sum(Age6 * Spawners)/totSpawn[1],
"age7" = sum(Age7 * Spawners)/totSpawn[1])

print(summary)

write.csv(summary,"ageComp_total.csv")
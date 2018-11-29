

rm(list=ls())

### setwd("~Desktop/Rpractice")

### getwd()

#### install.packages("Lahman")

require(Lahman)
require(dplyr)



bat <- Batting
bat1 <- Batting
bat2 <- Batting
names <- Master


bat <- mutate(bat, PA = AB + BB + IBB + HBP + SH + SF)

batMerge <- merge(bat, names, by.x = "playerID", by.y = "playerID")
batMerge <- select(batMerge, -birthYear, -birthMonth, -birthDay, -birthCountry, -birthState, -birthCity, -deathYear, -deathMonth, -deathDay, -deathCountry, -deathState, -deathCity, -finalGame, -deathDate, -birthDate, -retroID, -bbrefID)

batMerge <- filter(batMerge, yearID==2016 & PA >= 200)
bat1 <- filter(bat1, yearID==2016)


batMerge <- mutate(batMerge, HRPA = HR / PA)

### batMerge <- mutate(batMerge, projHR = HRPA * 600)

batMerge <- mutate(batMerge, BA = H / AB)

### batMerge <- mutate(batMerge, rbiPA = RBI / PA, rPA = R / PA)

### batMerge <- mutate(batMerge, projRBI = rbiPA * 600, projR = rPA * 600)

batMerge <- mutate(batMerge, X1B = H - X2B - X3B - HR)

batMerge <- mutate(batMerge, RC = ((H + BB - CS + HBP - GIDP) * (((X1B - X2B - X3B - HR) + (2*X2B) + (3*X3B) + (4*HR) + (.26 * (BB - IBB + HBP)) + (.52 * (SH + SF + SB)))))/ (AB + BB + HBP + SF + SH))

batMerge <- mutate(batMerge, OBP = (H + BB + HBP) / (AB + BB + HBP + SF))

batMerge <- mutate(batMerge, RC100 = (RC / PA) * 100)

batMerge <- mutate(batMerge, Kpct = SO / PA, BBpct = BB / PA)

### plot(batMerge$RBI, batmerge$BBpct)

### batMerge <- data[c("nameFirst", "nameLast", "nameGiven", "playerID", "yearID", "stint", "teamID", "lgID", "G", "AB","PA", "R", "H","BA","OBP", "X1B","X2B", "X3B", "HR","HRPA","RC","RC100", "RBI","BBpct","Kpct", "BB", "IBB","HBP", "SO", "SH", "SF", "GIDP", "weight", "height", "bats", "throws", "debut")]

batMerge[1,1]
batMerge[2,1] 
batMerge[2,1] <- "rossinini01"

teams <- 1:32

ex1 <- matrix(1:32,16)

playerIDs <- names[["playerID"]]


firstTen <- matrix(NA,10, length(bat2))

colnames(firstTen) <- colnames(bat2)

firstTen[1,1] <- bat2[1,1]



for (j in 1:10){
  for (i in 1:20){
    firstTen[i,j] <- bat2[i,j]
  }
}

firstTen <- head(bat2, 10)

length(bat2)







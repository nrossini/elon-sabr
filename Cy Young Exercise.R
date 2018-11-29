##### Your goal: Come up with two models. The first model should predict how many wins
##### a pitcher "deserves" based on his production. The second model should take into account 
##### his overall team's production
##### This is in response to DeGrom recently winning the CY Young with 

#### install.packages("Lahman")
require(Lahman)
require(dplyr)
rm(list = ls())

master <- Master
pnames <- select(master, playerID, nameFirst, nameLast)

pitch <- Pitching
pitch <- merge(pitch, pnames)

##### For Model 1

##### Use as model set
modernPitch <- filter(pitch, yearID>=2006 & yearID<=2015)
modernPitch <- filter(modernPitch, modernPitch$IPouts >= 150)
modernPitch <- filter(modernPitch, modernPitch$GS >= 5)

#### Make new columns
modernPitch <- mutate(modernPitch, K9=((SO*9)/(IPouts/3)))
modernPitch <- mutate(modernPitch, BB9=((BB*9)/(IPouts/3)))
modernPitch <- mutate(modernPitch, H9=(H*9)/(IPouts/3))
modernPitch <- mutate(modernPitch, WHIP = (BB + IBB + H) / (IPouts/3))
modernPitch <- mutate(modernPitch, FIPnc = ((13*HR)+(3*(BB+HBP))-(2*SO))/(IPouts/3))
modernPitch <- mutate(modernPitch, IP = (IPouts / 3))

#### Subset dataset by year, innings pitched, starts
pitch16 <- filter(pitch, yearID==2016)
pitch16 <- filter(pitch16, pitch16$IPouts >= 150)
pitch16 <- filter(pitch16, pitch16$GS >= 5)

#### Make new columns per nines
pitch16 <- mutate(pitch16, K9=((SO*9)/(IPouts/3)))
pitch16 <- mutate(pitch16, BB9=((BB*9)/(IPouts/3)))
pitch16 <- mutate(pitch16, H9=(H*9)/(IPouts/3))
pitch16 <- mutate(pitch16, WHIP = (BB + IBB + H) / (IPouts/3))
pitch16 <- mutate(pitch16, FIPnc = ((13*HR)+(3*(BB+HBP))-(2*SO))/(IPouts/3))
pitch16 <- mutate(pitch16, IP = (IPouts / 3))
pitch16 <- mutate(pitch16, Wdiff = W - expw)


## pitch16 <- mutate(pitch16, 

## FIP = ((13*HR)+(3*(BB+HBP))-(2*K))/IP 

lm1 <- lm(W ~ K9 + BB9 + H9 + WHIP + ERA + FIPnc, data = modernPitch)
summary(lm1)

lm2 <- lm(W ~ K9 + BB9 + H9 + WHIP + ERA + FIPnc + IP, data = modernPitch)
summary(lm2)

lm3 <- lm(W ~ BB9 + H9 + WHIP + ERA + IP, data = modernPitch)
summary(lm3)

pitch16$expw <- predict(lm3, newdata = pitch16)



#### For model 2

teams <- Teams
modernTeams <- filter(teams, yearID>=2006 & yearID<=2015)
teams16 <- filter(teams, yearID==2016)
####Select statement to determine what columns are of interest from Team table




##### CY Young Finalists 2016
cyyoung <- filter(pitch16, playerID== "porceri01" | playerID== "scherma01" | playerID== "lestejo01" | playerID== "hendrky01" | playerID== "verlaju01" | playerID== "klubeco01")

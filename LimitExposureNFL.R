#import data sets
myData <- read.csv('c:\\users\\hadcock\\documents\\nelson\\nfl dfs\\dfnweek10offenselateslate.csv')

#use this file to set exposure levels
dirMatrix <- read.csv('c:\\users\\hadcock\\documents\\nelson\\nfl dfs\\nfldirmatrix.csv')

#search for teams where I have offense vs defense
totalTeams <- length(fdTeams$DK)
defenses <- myData$Player.Name
defenseVector <- vector("numeric",totalTeams)

for (i in 1:totalTeams)
{
  defenseOpponent <- myData$Opp[match(fdTeams$DK[i],defenses)]
  offenseOpponents <- c(as.character(myData$Team[match(fdTeams$QB[i],defenses)])
                        ,as.character(myData$Team[match(fdTeams$RB1[i],defenses)])
                        ,as.character(myData$Team[match(fdTeams$RB2[i],defenses)])
                        ,as.character(myData$Team[match(fdTeams$TE[i],defenses)])
                        ,as.character(myData$Team[match(fdTeams$WR1[i],defenses)])
                        ,as.character(myData$Team[match(fdTeams$WR2[i],defenses)])
                        ,as.character(myData$Team[match(fdTeams$WR3[i],defenses)]))
  offenseVsDefense <- sum(offenseOpponents == defenseOpponent)
  defenseVector[i] <- ifelse(offenseVsDefense > 0, 1,0)
}
fdTeams$OffVsDef <- defenseVector

#get number of players
no_players <- length(myData$Player.Name)

#create vector data frame so can export to excel
myVectors <- data.frame(matrix(ncol=length(fdTeams$DK), nrow = 0))
playerNames <- myData$Player.Name

#loop through fdTeams and create a vector for each player
for (i in 1:no_players)
{
  x <- as.character(myData$Player.Name[i])
  y <- assign(x,c(as.numeric(fdTeams$DK == x | fdTeams$QB == x | fdTeams$RB1 == x | fdTeams$RB2 == x | fdTeams$TE == x | fdTeams$WR1 == x | fdTeams$WR2 == x | fdTeams$WR3 == x)))
  myVectors[nrow(myVectors)+1,] <-rbind(y)
}

Pick <- rep(1,length(myVectors[1,]))
myVectors <- rbind(Pick,defenseVector,myVectors,myVectors)

#solver to select teams from fdTeams
score <- as.numeric(fdTeams$Score)

num.teams <- length(score)

var.types <- rep("B", num.teams)

f <- score

A <- as.matrix(myVectors)

dir <- as.matrix(dirMatrix[,2])

b <- as.matrix(dirMatrix[,3])

sol <- Rglpk_solve_LP(obj = f
                      , mat = A
                      , dir = dir
                      , rhs = b
                      , types = var.types
                      , max=TRUE)

fdTeams$Solution <- sol$solution

print(sol$solution)

#create new data frame with only my picked teams
myfdTeams <- subset(fdTeams,Solution>0)

#create exposure table for my picks
myexposureTable <- data.frame("Name" = character(),"Pos" = character(), "myOwnership" = integer(), stringsAsFactors = FALSE)

name <- myData$Player.Name
position <- myData$Pos
totalPlayers <- length(name)

for (i in 1:totalPlayers)
{
  playerName <- name[i]
  playerPosition <- as.character(position[i])
  playerName <- c(as.character(playerName))
  playerExposure <- sum(myfdTeams == playerName)/nrow(myfdTeams)
  finalPlayer <- c(playerName,playerPosition,playerExposure)
  myexposureTable[nrow(myexposureTable)+1,] <-rbind(finalPlayer)
}



write.csv(myfdTeams, file = "FanduelTeamsR.csv")


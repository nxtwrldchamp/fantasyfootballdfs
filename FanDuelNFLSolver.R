#This code will input projections and favorite stacks, then build x amount of teams for
#each stack. The finished product will be a large group of lineups that we can then select
#our favorite teams from. 


#import projections
myData <- read.csv('c:\\users\\hadcock\\documents\\nelson\\nfl dfs\\dfnweek10offenselateslate.csv')

#import stacks I want to build teams around
lockExclude <- read.csv('c:\\users\\hadcock\\documents\\nelson\\nfl dfs\\locksexcludes.csv', stringsAsFactors = FALSE)

#install rglpk package and library
install.packages("Rglpk")
library(Rglpk)

#create vectors to use in lineups solver
name <- myData$Player.Name
pos <- myData$Pos
pts <- myData$Proj.FP
cost <- myData$Salary
team <- myData$Team

#finds stacks in csv file, set iterations of each stack to build then calculate total teams wanted
numberofStacks <- length(lockExclude$P1)
noEachStack <- 40
numberofTeams <- noEachStack*numberofStacks

#create data frame to capture teams
fdTeams <- data.frame(matrix(NA, nrow = numberofTeams, ncol = 10))
names(fdTeams) <- c("DK", "QB", "RB1", "RB2", "TE", "WR1", "WR2", "WR3", "Salary","Score")

#set counter to help input built teams to dataframe inside the loop
y <- 0

#loop through each stack, remove blank values from stack vectors
for (x in 1:numberofStacks)
{
  stacks <- c(lockExclude[x,])
  stacks <- c(as.character(stacks[!is.na(stacks)]))
  stacks <- stacks[stacks!=""]
  
  #set max score so you can loop through teams always getting the next best team
  maxPts  <- 1000
  
  #for each stack above, now loop through and create x number of teams for that stack
  for (i in 1:noEachStack)
  {
    num.players <- length(name)
    
    f <- pts
    
    var.types <- rep("B", num.players)
    
    A <- rbind(as.numeric(pos=="QB")
               , as.numeric(pos=="RB")
               , as.numeric(pos=="WR")
               , as.numeric(pos=="TE")
               , as.numeric(pos=="DK")
               , as.numeric(name %in% stacks)
               ,cost
               ,f)
    
    dir <- c("=="
             ,"=="
             ,"=="
             ,"=="
             ,"=="
             ,"=="
             ,"<="
             ,"<=")
    
    b <- c(1
           , 2
           , 3
           , 1
           , 1
           , length(stacks)
           , 60000
           , maxPts)
    
    sol <- Rglpk_solve_LP(obj = f
                          , mat = A
                          , dir = dir
                          , rhs = b
                          , types = var.types
                          , max=TRUE)
    
    #take solver solution and transform it into something I can add to our fdTeams dataframe
    myTeam <- data.frame(myData[sol$solution == 1,])
    score <- sol$optimum
    teamSalary <- sum(cost[sol$solution>0])
    myTeam <- t(myTeam[,1])
    finalLineup <- t(c(as.character(myTeam),teamSalary,score))
    fdTeams[i+y,] <- finalLineup
    maxPts <- score - .01
    print(i+y)
  }
  y <- y + noEachStack
}

#create player exposure data frame, showing how often players show up in lineups just created
playerExposure <- data.frame("Name" = character(), "Ownership" = integer(), stringsAsFactors = FALSE)
totalPlayers <- length(name)

for (i in 1:totalPlayers)
{
  playerName <- name[i]
  playerName <- c(as.character(playerName))
  playerExposureValue <- sum(fdTeams == playerName)/nrow(fdTeams)
  finalPlayer <- c(playerName,playerExposureValue)
  playerExposure[nrow(playerExposure)+1,] <-rbind(finalPlayer)
}

#search for teams where I have offense vs defense and mark them with a new column in fdTeams data frame

#set total teams, create defenses vector to look up whether offensive players are facing
#that defense. Create defenseVector to add to fdTeams signaling which teams have offensive
#players going against their defense. 

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

#this part of the code creates a vector for every player based on the fdTeams dataframe
#Will use this in a subsequent solver to use for adjusting exposure to certain players

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

#create a pick vector to use in subsequent solver, so that the solver knows how many teams to choose
Pick <- rep(1,length(myVectors[1,]))

#put all the vectors together to use in subsequent solver 
#pick vector used to determine how many teams to select
#defenseVector used to ensure no teams are selected where offense is up against defense
#myVectors x 2 so that I can set a Max exposure to match vs 1st, and Min exposure to match vs 2nd 
myVectors <- rbind(Pick,defenseVector,myVectors,myVectors)

#use this file to set exposure levels
#at the moment I manually create this file
#to match the above myVectors it should contain the following
#after headers, 1st row should be "picks", 2nd "offenseVsdefense", then list of players to set max exposure
#followed by list of players again to set min exposure
dirMatrix <- read.csv('c:\\users\\hadcock\\documents\\nelson\\nfl dfs\\nfldirmatrix.csv')


#solver to select x number of teams from fdTeams 
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



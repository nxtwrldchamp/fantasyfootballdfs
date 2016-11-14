myData <- read.csv('c:\\users\\hadcock\\documents\\nelson\\nfl dfs\\dfnweek10offenselateslate.csv')
lockExclude <- read.csv('c:\\users\\hadcock\\documents\\nelson\\nfl dfs\\locksexcludes.csv', stringsAsFactors = FALSE)


install.packages("Rglpk")
library(Rglpk)

#stock optimal linups solver
name <- myData$Player.Name
pos <- myData$Pos
pts <- myData$Proj.FP
cost <- myData$Salary
team <- myData$Team

#variation of each stack
numberofStacks <- length(lockExclude$P1)
noEachStack <- 40
numberofTeams <- noEachStack*numberofStacks

#create data frame to capture teams
fdTeams <- data.frame(matrix(NA, nrow = numberofTeams, ncol = 10))
names(fdTeams) <- c("DK", "QB", "RB1", "RB2", "TE", "WR1", "WR2", "WR3", "Salary","Score")

y <- 0

#get stacks
for (x in 1:numberofStacks)
{
  stacks <- c(lockExclude[x,])
  stacks <- c(as.character(stacks[!is.na(stacks)]))
  stacks <- stacks[stacks!=""]
  
  #set max score
  maxPts  <- 1000
  
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

#player exposure
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




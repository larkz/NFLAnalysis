# setwd('C:\\Users\\Larkin\\Google Drive\\MASc\\NFLAnalysis')
# dir <- '/Users/larkin/Google Drive/MASc/NFLAnalysis'
library(ggplot2)

dir <- '/Users/larkin/Google Drive/NFLAnalysis/RScript'

setwd(dir)

library(vars)
library(forecast)
library(hydroGOF)

# setwd(paste(dir, '/RScript', sep = ''))

source('getForecast.R')
source('run2014.R')

setwd(dir)

teamList <- read.csv('ListofTeams.csv')
box2014 <- read.csv('years_2014_games_games.csv')
box2013 <- read.csv('years_2013_games_games.csv')
box2012 <- read.csv('years_2012_games_games.csv')
box2011 <- read.csv('years_2011_games_games.csv')
box2010 <- read.csv('years_2010_games_games.csv')

cleanNFL <- function(rawbox, year){
  
  gg <- rawbox[!rawbox$Week == 'Week',]
  gg <- gg[!gg$Date == 'Playoffs',]
  
  rownames(gg) <- 1:nrow(gg)
  gg$Year <- c(year)
  
  return(gg)
  
}

# Get the starting year index
getStartIndex <- function(year, mat){
  for(i in 1:length(mat)){
    
    if(mat[i] == year){
      return(i)
    }
  }
  
}

y2014 <- cleanNFL(box2014, 2014)
y2013 <- cleanNFL(box2013, 2013)
y2012 <- cleanNFL(box2012, 2012)
y2011 <- cleanNFL(box2011, 2011)
y2010 <- cleanNFL(box2010, 2010)

tr_his <- rbind(y2010, y2011, y2012, y2013, y2014)

# Get Unified Date
# 1 to 1068 for files 2010 to 2013


tr_his$fullDate <- c(1000)

#1:dim(tr_his)[1]


for(i in 1:1068){
  
  tr_his[i,]$fullDate <- as.Date( paste(tr_his[i,]$Date, tr_his[i,]$Year, Sep = " " ), "%B %d %Y")
  
  #tr_his[i,]$fullDate <- as.Date(tr_his[i,]$fullDate, origin = "1970-01-01")
  
}
# A weird date formatting happened in the 2014 csv file had to customize
for(i in 1069:dim(tr_his)[1]){
  
  tr_his[i,]$fullDate <- as.Date( paste(tr_his[i,]$Date, tr_his[i,]$Year, Sep = "-" ), "%b-%d %Y")
  
  #tr_his[i,]$fullDate <- as.Date(tr_his[i,]$fullDate, origin = "1970-01-01")
  
}


#Get pointsfor 
getPointsFor <- function(teamStr, rowDat){
  
  if( rowDat$Winner.tie == teamStr){
    pointsFor = rowDat$PtsW 
    
  }
  if( rowDat$Loser.tie == teamStr){
    pointsFor = rowDat$PtsL
    
  }
   
}

tr_his$PtsFor <- c(as.integer(8000))
tr_his$PtsAgs <- c(as.integer(8000))

tr_his$YdsFor <- c(as.integer(8000))
tr_his$YdsAgs <- c(as.integer(8000))


#Sort by team

tr_his_bt <- list()

#Go through each name in the array
for(n in 1:dim(teamList)[1]){
  #print(toString(teamList[n,1]))
  
  cur_name =toString(teamList[n,1])
  
  cur_team_stat <- c()
  for(c in 1:dim(tr_his)[1]){
    #print(tr_his$Winner.tie[c])  
    #print(tr_his$Loser.tie[c])
    
    if(toString(tr_his$Winner.tie[c]) == cur_name ){
      cur_team_stat <- rbind(cur_team_stat, tr_his[c,] )
    }
    
    if(toString(tr_his$Loser.tie[c]) == cur_name ){
      cur_team_stat <- rbind(cur_team_stat, tr_his[c,] )
    }    
  }
  
  S =  paste( "tr_his_bt[['", cur_name ,"']] = cur_team_stat", sep = ''  )
  
  eval(parse(text=S)) 
  
  # toString(teamList[c,1]) =0 8)
  #toString(tr_his$Winner.tie[1]) == toString(teamList[25,1]) 
}


# Loop through all teams
for (t in ls(tr_his_bt)){
  
  grabArr <- c()
  for (i in 1:dim(tr_his_bt[[t]])[1]){
    
    if(tr_his_bt[[t]]$Winner.tie[i] == t){
      grabPts  <- tr_his_bt[[t]]$PtsW[i]
      grabYds  <- tr_his_bt[[t]]$YdsW[i]
    } else {
      grabPts  <- tr_his_bt[[t]]$PtsL[i]
      grabYds  <- tr_his_bt[[t]]$YdsL[i]
    }
    
    tr_his_bt[[t]]$PtsFor[i] <- as.integer(as.character(grabPts))
    tr_his_bt[[t]]$YdsFor[i] <- as.integer(as.character(grabYds))
    
  }
    
}

for (t in ls(tr_his_bt)){
  
  grabArr <- c()
  for (i in 1:dim(tr_his_bt[[t]])[1]){
    
    if(tr_his_bt[[t]]$Loser.tie[i] == t){
      grabPts  <- tr_his_bt[[t]]$PtsW[i]
      grabYds  <- tr_his_bt[[t]]$YdsW[i]
    } else {
      grabPts  <- tr_his_bt[[t]]$PtsL[i]
      grabYds  <- tr_his_bt[[t]]$YdsL[i]
    }
    
    tr_his_bt[[t]]$PtsAgs[i] <- as.integer(as.character(grabPts))
    tr_his_bt[[t]]$YdsAgs[i] <- as.integer(as.character(grabYds))
    
  }
  
}


# Look for correlation between two teams

get2Team <- function(team1str, team2str, nfl_dat){
  
  data_arr <- data.frame()
  
  
  for (c in 1:dim(nfl_dat[[team1str]])[1]){
    
    if (nfl_dat[[team1str]]$Winner.tie[c] == team1str && nfl_dat[[team1str]]$Loser.tie[c] == team2str ){
      data_arr <- rbind(data_arr, nfl_dat[[team1str]][c,] )
    } 
    
    if (nfl_dat[[team1str]]$Loser.tie[c] == team1str && nfl_dat[[team1str]]$Winner.tie[c] == team2str ){
      data_arr <- rbind(data_arr, nfl_dat[[team1str]][c,] )
    } 
    
  }
  
  return(data_arr)
}

# Back testing

sim_result <- as.data.frame(c())
detail_result <- list()

for(t in teamList$Team){
  
  res_arr <- run2014(t, tr_his_bt)
  sim_result <- rbind(sim_result, res_arr[[2]])
  detail_result[[t]] <- res_arr[[1]]
  print(t)
}

rownames(sim_result) <- teamList$Team


# Get histogram points for 

hist_pt_arr <- c()
for(v in tr_his_bt){
  
  hist_pt_arr <- c( v$PtsFor, hist_pt_arr)
  
}



# Get histogram yards for 

hist_yd_arr <- c()
for(v in tr_his_bt){
  
  hist_yd_arr <- c( v$YdsFor, hist_yd_arr)
  
}

cor(hist_yd_arr, hist_pt_arr)

# Histogram

qplot(hist_pt_arr,
      geom = "histogram",
      binwidth = 1,
      main = "Total Points scored per Game for NFL 2010-2013 Seasons", 
      xlab = "Points Scored per Game",
      fill=I("firebrick2"), 
      col=I("firebrick4"))



plt <- qplot(hist_yd_arr,
      geom = "histogram",
      binwidth = 12,
      main = "Total Yardage scored per Game for NFL 2010-2013 Seasons", 
      xlab = "Total Yards per Game",  
      fill=I("dodgerblue2"), 
      col=I("dodgerblue4"))

# Get mean and standard deviation
tab_arr <- c(mean(hist_pt_arr), mean(hist_yd_arr))
tab_arr <- rbind(tab_arr, c(sd(hist_pt_arr), sd(hist_yd_arr)))

tab_arr <- as.data.frame(tab_arr)
colnames(tab_arr) <- c("Points Scored", "Yards Per Game")
rownames(tab_arr) <- c("Mean", "Standard Deviation")


# Create a table plot
library(gridExtra)
tbl <- tableGrob(tab_arr)
grid.arrange(tbl,
             nrow=2,
             as.table=TRUE,
             heights=c(4,1))

hist(hist_pt_arr, breaks = 60)
hist(hist_yd_arr, breaks = 60)

# Simulation Results

sorted_sim_result <- sim_result[order(sim_result$lincomb),]

par(omi = c(0, 1, 0, 0))
barplot(sorted_sim_result$lincomb, legend.text=TRUE,
        horiz =  TRUE,
        density=NA,
        axes=TRUE, names.arg=rownames(sorted_sim_result), 
        cex.names=0.7, las=2,
        space = c(0,2),
        col= "cyan4",
        xlab = "Prediction Success %",
        main = "Spread Forecasting - PCSS Method")


# Historical Comparison
sorted_sim_result <- sim_result[order(sim_result$hist),]

par(omi = c(0, 1, 0, 0))
barplot(sorted_sim_result$hist, legend.text=TRUE,
        horiz =  TRUE,
        density=NA,
        axes=TRUE, names.arg=rownames(sorted_sim_result), 
        cex.names=0.7, las=2,
        space = c(0,2),
        col= "bisque3",
        xlab = "Prediction Success %",
        main = "Spread Forecasting - Historical Comparison Method")



# Get mean Point Spread

team_perf <- c()
rnames <- c()
for(tt in ls(tr_his_bt)){
  mm <- mean(tr_his_bt[[tt]]$PtsFor - tr_his_bt[[tt]]$PtsAgs)
  ss <- sd(tr_his_bt[[tt]]$PtsFor - tr_his_bt[[tt]]$PtsAgs)
  
  rnames <- rbind(rnames, tt)
  
  team_perf <- rbind(team_perf, c(mm,ss))
}

rownames(team_perf) <- rnames
colnames(team_perf) <- c("Mean Point Spread", "Variance")
team_perf <- as.data.frame(team_perf)

team_perf <- team_perf[order(team_perf[['Mean Point Spread']]),]

# Create a table
par(omi = c(0, 1, 0, 0))
barplot(team_perf[['Mean Point Spread']], legend.text=TRUE,
        horiz =  TRUE,
        density=NA,
        axes=TRUE, names.arg=rownames(team_perf), 
        cex.names=0.7, las=2,
        space = c(0,2),
        xlab = "Mean Point Spread",
        col= "firebrick2",
        main = "Ranking the NFL's Strongest Teams by Point Spread 2010-2013")















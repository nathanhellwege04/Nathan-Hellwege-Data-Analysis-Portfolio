# Nathan Hellwege
# Project file
# Section four takes a very long time to run and can be skipped (the resulting files are included in the data folder)
#
# Analysis of the value of different player archetypes.

rm(list=ls())

library(cluster)
library(glmnet)
library(data.table)
library(readr)
library(ggplot2)
library(zoo)
library(forecast)


# Data Import and Cleaning ####
# You'll see this several times. Each season had a separate flat file, but I combined them 
# and saved the combined dataset so I don't need to upload 5 flat files each time
# s20 <- read.csv("data/skaters2021.csv")
# s21 <- read.csv("data/skaters2122.csv")
# s22 <- read.csv("data/skaters2223.csv")
# s23 <- read.csv("data/skaters2324.csv")
# s24 <- read.csv("data/skaters2425.csv")
# 
# skaters <- rbind(s24, s23, s22, s21, s20)
# 
# write.csv(skaters, "skaters.csv", row.names = FALSE)
skaters <- read.csv("data/skaters.csv")

s5v5 <- skaters[which(skaters$situation == "5on5"),]

s5v5$G_60 <- (s5v5$I_F_goals * 3600) / s5v5$icetime
s5v5$PA_60 <- (s5v5$I_F_primaryAssists * 3600) / s5v5$icetime
s5v5$SA_60 <- (s5v5$I_F_secondaryAssists * 3600) / s5v5$icetime
s5v5$ShotAt_60 <- (s5v5$I_F_shotAttempts * 3600) / s5v5$icetime
s5v5$PIM_60 <- (s5v5$I_F_penalityMinutes * 3600) / s5v5$icetime
s5v5$H_60 <- (s5v5$I_F_hits * 3600) / s5v5$icetime
s5v5$Give_60 <- (s5v5$I_F_giveaways * 3600) / s5v5$icetime
s5v5$Take_60 <- (s5v5$I_F_takeaways * 3600) / s5v5$icetime
s5v5$Reb_60 <- (s5v5$I_F_rebounds * 3600) / s5v5$icetime

# Cluster analysis from before I really knew how I wanted to actually do it
# s5v5.minGP <- s5v5[which(s5v5$games_played >= 20),]
# skaters.red <- s5v5.minGP[, c("playerId", "G_60", "PA_60", "SA_60", "ShotAt_60", "PIM_60", "H_60", "Give_60", "Take_60", "Reb_60")]

# skaters.scale <- scale(skaters.red[,2:10])
# skaters.dist <- dist(skaters.scale, method = "euclidean")
# 
# aResult <- agnes(skaters.dist, diss = TRUE, method = "ward")
# aResult
# plot(aResult)
# 
# aClusters <- cutree(aResult, k = 4)
# s5v5.clus <- data.frame(skaters.red, aClusters)
# s5v5.full <- data.frame(s5v5.minGP, aClusters)
# 
# summary(subset(s5v5.clus, aClusters == 1))
# summary(subset(s5v5.clus, aClusters == 2))
# summary(subset(s5v5.clus, aClusters == 3))
# summary(subset(s5v5.clus, aClusters == 4))
# 
# table(s5v5.full[which(s5v5.full$aClusters == 1),]$position)
# table(s5v5.full[which(s5v5.full$aClusters == 2),]$position)
# table(s5v5.full[which(s5v5.full$aClusters == 3),]$position)
# table(s5v5.full[which(s5v5.full$aClusters == 4),]$position)
# s5v5.full[which(s5v5.full$aClusters == 3 & s5v5.full$position == "D"),]$name
# 
# length(unique(s5v5$name))
players <- data.frame(
  playerId = integer(0),
  seasonCount = integer(0),
  firstSeason = integer(0),
  lastSeason = integer(0),
  name = character(0),
  position = character(0),
  games_played = integer(0),
  icetime = numeric(0),
  shifts = numeric(0),
  goals = numeric(0),
  primaryAssists = numeric(0),
  secondaryAssists = numeric(0),
  shotAttempts = numeric(0),
  PIM = numeric(0),
  hits = numeric(0),
  giveaways = numeric(0),
  takeaways = numeric(0),
  rebounds = numeric(0)
)

setDT(s5v5)

players <- s5v5[, .(
  n = .N,
  first_season = min(season),
  last_season = max(season),
  name = first(name),
  position = first(position),
  games_played = sum(games_played),
  icetime = sum(icetime),
  shifts = sum(shifts),
  goals = sum(I_F_goals),
  primaryAssists = sum(I_F_primaryAssists),
  secondaryAssists = sum(I_F_secondaryAssists),
  shotAttempts = sum(I_F_shotAttempts),
  penaltyMinutes = sum(I_F_penalityMinutes),
  hits = sum(I_F_hits),
  giveaways = sum(I_F_giveaways),
  takeaways = sum(I_F_takeaways),
  rebounds = sum(I_F_rebounds)
), by = playerId]

players$G_60 <- (players$goals * 3600) / players$icetime
players$PA_60 <- (players$primaryAssists * 3600) / players$icetime
players$SA_60 <- (players$secondaryAssists * 3600) / players$icetime
players$ShotAt_60 <- (players$shotAttempts * 3600) / players$icetime
players$PIM_60 <- (players$penaltyMinutes * 3600) / players$icetime
players$H_60 <- (players$hits * 3600) / players$icetime
players$Give_60 <- (players$giveaways * 3600) / players$icetime
players$Take_60 <- (players$takeaways * 3600) / players$icetime
players$Reb_60 <- (players$rebounds * 3600) / players$icetime

p.min <- players[which(players$games_played >= 30),]

## Cluster Analysis ####
# Before I decided to run separate models for forwards and defensemen
# p.red <- p.min[, c("playerId", "G_60", "PA_60", "SA_60", "ShotAt_60", "PIM_60", "H_60", "Give_60", "Take_60", "Reb_60")]

# p.scale <- scale(p.red[,2:10])
# p.dist <- dist(p.scale, method = "euclidean")
# 
# players.agnes <- agnes(p.dist, diss = TRUE, method = "ward")
# players.agnes
# plot(players.agnes)
# 
# p.clus <- cutree(players.agnes, k = 3)
# p.min <- data.frame(p.min, p.clus)
# 
# table(p.min[which(p.min$p.clus == 1),]$position)
# table(p.min[which(p.min$p.clus == 2),]$position)
# table(p.min[which(p.min$p.clus == 3),]$position)
# p.min[which(p.min$p.clus == 1),]$name
# p.min[which(p.min$p.clus == 2),]$name
# p.min[which(p.min$p.clus == 3),]$name

# p.min[which(p.min$p.clus == 1 & p.min$position == "D"),]$name
# p.min[which(p.min$p.clus == 2 & p.min$position == "D"),]$name
# p.min[which(p.min$p.clus == 3),]$name

# Here's the actual cluster analysis I used for my presnetation
forwards <- p.min[position != "D"]
defense <- p.min[position == "D"]

f.red <- forwards[, c("playerId", "G_60", "PA_60", "SA_60", "ShotAt_60", "PIM_60", "H_60", "Give_60", "Take_60", "Reb_60")]
d.red <- defense[, c("playerId", "G_60", "PA_60", "SA_60", "ShotAt_60", "PIM_60", "H_60", "Give_60", "Take_60", "Reb_60")]

f.scale <- scale(f.red[,2:10])
d.scale <- scale(d.red[,2:10])
f.dist <- dist(f.scale, method = "euclidean")
d.dist <- dist(d.scale, method = "euclidean")

f.agnes <- agnes(f.dist, diss = TRUE, method = "ward")
plot(f.agnes, main="Dendrogram for Forwards", xlab = "")
d.agnes <- agnes(d.dist, diss = TRUE, method = "ward")
plot(d.agnes, main = "Dendrogram for Defensemen", xlab="")

clus <- cutree(f.agnes, k = 4)
forwards <- data.frame(forwards, clus)
clus <- cutree(d.agnes, k=4)
defense <- data.frame(defense, clus)

cats <- rbind(forwards, defense)
cats$clus <- ifelse(cats$position == 'D', cats$clus + 4, cats$clus)

# Get dataset to easily determine which category each playerId corresdponded to
lookup <- cats[, c("playerId", "clus")]

### Cluster interpretation ####
summary(forwards[which(forwards$clus == 1),19:27])
summary(forwards[which(forwards$clus == 2),19:27])
summary(forwards[which(forwards$clus == 3),19:27])
summary(forwards[which(forwards$clus == 4),19:27])

summary(defense[which(defense$clus == 1),19:27])
summary(defense[which(defense$clus == 2),19:27])
summary(defense[which(defense$clus == 3),19:27])
summary(defense[which(defense$clus == 4),19:27])

# I used my knowledge of these players to help name each cluster
forwards[which(forwards$clus == 1),]$name
forwards[which(forwards$clus == 2),]$name
forwards[which(forwards$clus == 3),]$name
forwards[which(forwards$clus == 4),]$name

defense[which(defense$clus == 1),]$name
defense[which(defense$clus == 2),]$name
defense[which(defense$clus == 3),]$name
defense[which(defense$clus == 4),]$name


#### Synthesize RAPM ready dataset (can be skipped)####
# This entire section was used to get a dataset that has every shift and the xG both for ana against during that shift.
# This once again takes a while to run (and a lot of RAM), so this section can be skipped as the resulting rapm_set.csv
# was saved and uploaded.

# shots24 <- read.csv("data/shots_2024.csv")
# shots23 <- read.csv("data/shots_2023.csv")
# shots22 <- read.csv("data/shots_2022.csv")
# shots21 <- read.csv("data/shots_2021.csv")
# shots20 <- read.csv("data/shots_2020.csv")

# shots24 <- shots24[, c("homeTeamCode", "awayTeamCode", "season", "isPlayoffGame", "game_id", "time", "period", "team", "goal", "xCordAdjusted", "yCordAdjusted", "shooterPlayerId", "shooterName", "goalieIdForShot", "goalieNameForShot", "xGoal")]
# shots23 <- shots23[, c("homeTeamCode", "awayTeamCode", "season", "isPlayoffGame", "game_id", "time", "period", "team", "goal", "xCordAdjusted", "yCordAdjusted", "shooterPlayerId", "shooterName", "goalieIdForShot", "goalieNameForShot", "xGoal")]
# shots22 <- shots22[, c("homeTeamCode", "awayTeamCode", "season", "isPlayoffGame", "game_id", "time", "period", "team", "goal", "xCordAdjusted", "yCordAdjusted", "shooterPlayerId", "shooterName", "goalieIdForShot", "goalieNameForShot", "xGoal")]
# shots21 <- shots21[, c("homeTeamCode", "awayTeamCode", "season", "isPlayoffGame", "game_id", "time", "period", "team", "goal", "xCordAdjusted", "yCordAdjusted", "shooterPlayerId", "shooterName", "goalieIdForShot", "goalieNameForShot", "xGoal")]
# shots20 <- shots20[, c("homeTeamCode", "awayTeamCode", "season", "isPlayoffGame", "game_id", "time", "period", "team", "goal", "xCordAdjusted", "yCordAdjusted", "shooterPlayerId", "shooterName", "goalieIdForShot", "goalieNameForShot", "xGoal")]
# 
# 
# shots <- rbind(shots24, shots23, shots22, shots21, shots20)

# write.csv(shots, file="shots.csv", row.names = FALSE)
lines <- read.csv("lines.csv")
shots <- read.csv("data/shots.csv")

shots <- shots[which(shots$goalieIdForShot != 0),]
max(shots$xGoal)
shots$gameId <- paste0(as.character(shots$season), "0", as.character(shots$game_id))
shots$gameId <- as.integer(shots$gameId)


setDT(lines)
setDT(shots)

# Get the xGoals during each shift
shots.home <- shots[team == "HOME"]
shots.away <- shots[team == "AWAY"]

lines[, xGH := shots.home[.SD, on = .(gameId, time >= start, time < end), sum(xGoal), by= .EACHI]$V1]
lines[, xGA := shots.away[.SD, on = .(gameId, time >= start, time < end), sum(xGoal), by= .EACHI]$V1]


# g24 <- read.csv("data/goalies24.csv")
# g23 <- read.csv("data/goalies23.csv")
# g22 <- read.csv("data/goalies22.csv")
# g21 <- read.csv("data/goalies21.csv")
# g20 <- read.csv("data/goalies20.csv")
#  
# goalies <- rbind(g24, g23, g22, g21, g20)
# write.csv(goalies, file="goalies.csv", row.names = FALSE)

# Remove goalies from the lines dataset
goalies <- read.csv("data/goalies.csv")

gList <- unique(goalies$playerId)

lines[] <- lapply(lines, function(col) {
  col[col %in% gList] <- NA
  col
})

# Get the number of skaters on the ice and filter to 5v5
lines$h_count <- rowSums(!is.na(lines[, 4:9]))
lines$a_count <- rowSums(!is.na(lines[, 10:15]))

lines5 <- lines[h_count == 5 & a_count == 5]
setDF(lines5)
lines5[c("xGH", "xGA")][is.na(lines5[c("xGH", "xGA")])] <- 0

# home_lines <- lines5[, xGF := xGH]
# home_lines[, home := 1]
# away_lines <- lines5[, xGF := xGA]
# away_lines[, home := 0]

# This next large chunk of code converts the playerids in lines to a count of players in each cluster on the ice for each team
home_lines <- lines5
away_lines <- lines5
home_lines$xGF <- home_lines$xGH
away_lines$xGF <- away_lines$xGA
home_lines$home <- 1
away_lines$home <- 0

setDT(lookup)
setDT(home_lines)
setDT(away_lines)

home_cols <- c("home_1","home_2","home_3","home_4","home_5","home_6")
away_cols <- c("away_1", "away_2", "away_3", "away_4", "away_5", "away_6")

# This code is effectively repeated four times. The first gets the cluster count for when the home team is on offense, the second
# is for when the away team is on defense, the third is for when the away team is on offense, and the fourth is for when the home
# team is on defense
lookup[, `:=`(
  playerId = as.character(playerId),
  clus = as.character(clus)
)]
home_lines[, (home_cols) := lapply(.SD, as.character), .SDcols = home_cols]
id_to_cat <- setNames(lookup$clus, lookup$playerId)
cat_mat <- matrix(
  id_to_cat[as.matrix(home_lines[, ..home_cols])],
  nrow = nrow(home_lines)
)
cats <- unique(lookup$clus)

# Count per category
home_lines[, paste0("o_cat_", cats) := lapply(cats, function(cat) {
  rowSums(cat_mat == cat, na.rm = TRUE)
})]
home_cat_cols <- paste0("o_cat_", 1:8)
home_lines[, o_cat_9 := 5 - rowSums(.SD), .SDcols = home_cat_cols]


lookup[, `:=`(
  playerId = as.character(playerId),
  clus = as.character(clus)
)]
home_lines[, (away_cols) := lapply(.SD, as.character), .SDcols = away_cols]
# Lookup vector
id_to_cat <- setNames(lookup$clus, lookup$playerId)
# Map IDs -> categories
cat_mat <- matrix(
  id_to_cat[as.matrix(home_lines[, ..away_cols])],
  nrow = nrow(home_lines)
)
# Categories
cats <- unique(lookup$clus)
# Count per category
home_lines[, paste0("d_cat_", cats) := lapply(cats, function(cat) {
  rowSums(cat_mat == cat, na.rm = TRUE)
})]
home_cat_cols <- paste0("d_cat_", 1:8)
home_lines[, d_cat_9 := 5 - rowSums(.SD), .SDcols = home_cat_cols]
write.csv(home_lines, "home_lines.csv", row.names = FALSE)



lookup[, `:=`(
  playerId = as.character(playerId),
  clus = as.character(clus)
)]
away_lines[, (away_cols) := lapply(.SD, as.character), .SDcols = away_cols]
id_to_cat <- setNames(lookup$clus, lookup$playerId)
cat_mat <- matrix(
  id_to_cat[as.matrix(away_lines[, ..away_cols])],
  nrow = nrow(away_lines)
)
cats <- unique(lookup$clus)
away_lines[, paste0("o_cat_", cats) := lapply(cats, function(cat) {
  rowSums(cat_mat == cat, na.rm = TRUE)
})]
away_cat_cols <- paste0("o_cat_", 1:8)
away_lines[, o_cat_9 := 5 - rowSums(.SD), .SDcols = away_cat_cols]


lookup[, `:=`(
  playerId = as.character(playerId),
  clus = as.character(clus)
)]
away_lines[, (home_cols) := lapply(.SD, as.character), .SDcols = home_cols]
id_to_cat <- setNames(lookup$clus, lookup$playerId)
cat_mat <- matrix(
  id_to_cat[as.matrix(away_lines[, ..home_cols])],
  nrow = nrow(away_lines)
)
cats <- unique(lookup$clus)
away_lines[, paste0("d_cat_", cats) := lapply(cats, function(cat) {
  rowSums(cat_mat == cat, na.rm = TRUE)
})]
away_cat_cols <- paste0("d_cat_", 1:8)
away_lines[, d_cat_9 := 5 - rowSums(.SD), .SDcols = away_cat_cols]

# Combine the datasets into one
setDF(home_lines)
setDF(away_lines)

away_lines$d_cat_1 <- home_lines$o_cat_1
away_lines$d_cat_2 <- home_lines$o_cat_2
away_lines$d_cat_3 <- home_lines$o_cat_3
away_lines$d_cat_4 <- home_lines$o_cat_4
away_lines$d_cat_5 <- home_lines$o_cat_5
away_lines$d_cat_6 <- home_lines$o_cat_6
away_lines$d_cat_7 <- home_lines$o_cat_7
away_lines$d_cat_8 <- home_lines$o_cat_8
away_lines$d_cat_9 <- home_lines$o_cat_9

home_lines$duration <- home_lines$end - home_lines$start
home_lines$xGF_60 <- (home_lines$xGF / home_lines$duration) * 3600
away_lines$duration <- away_lines$end - away_lines$start
away_lines$xGF_60 <- (away_lines$xGF / away_lines$duration) * 3600

rapm_cols <- c("xGF_60", "duration", "o_cat_1", "o_cat_2", "o_cat_3", "o_cat_4", "o_cat_5", "o_cat_6", "o_cat_7", "o_cat_8", "o_cat_9", "d_cat_1", "d_cat_2", "d_cat_3", "d_cat_4", "d_cat_5", "d_cat_6", "d_cat_7", "d_cat_8", "d_cat_9")
rapm_set <- rbind(home_lines[,rapm_cols], away_lines[,rapm_cols])

# Save it so I never have to run that code again
write.csv(rapm_set, "rapm_set.csv", row.names = FALSE)
##### Regularized Adjusted Plus-Minus ####
rapm_set <- read.csv("data/rapm_set.csv")

xGF_60 <- rapm_set$xGF_60
lengths <- rapm_set$duration
rapm_cats <- rapm_set[,3:20]
rapm_cats <- as.matrix(rapm_cats)
rapm_cats <- Matrix(rapm_cats, sparse=TRUE)

# nfolds is only 3 since anything higher crashed my computer
rapm_fit <- cv.glmnet(x = rapm_cats, y = xGF_60, weights = lengths, alpha = 0, nfolds = 3, standardize = FALSE, parallel = TRUE)

lambda <- rapm_fit$lambda.1se

coefs <- coef(rapm_fit, s = lambda)
rapm_df <- data.frame(
  variable = rownames(coefs),
  coefficient = as.numeric(coefs)
)
rapm_fit

###### Team salary analysis ####
caps <- read.csv("data/nhl_spending.csv")
caps$Active <- parse_number(caps$Active)
caps$Total.Cap <- parse_number(caps$Total.Cap)

# Get the salary cap for each year
caps$Salary.Cap <- ifelse(caps$Year == 2012, 64300000,
                          ifelse(caps$Year == 2013, 70200000,
                                 ifelse(caps$Year == 2014, 64300000,
                                        ifelse(caps$Year == 2015, 69000000,
                                               ifelse(caps$Year == 2016, 71400000,
                                                      ifelse(caps$Year == 2017, 73000000,
                                                             ifelse(caps$Year == 2018, 75000000,
                                                                    ifelse(caps$Year == 2019, 79500000,
                                                                           ifelse(caps$Year == 2020, 81500000,
                                                                                  ifelse(caps$Year == 2021, 81500000,
                                                                                         ifelse(caps$Year == 2022, 81500000,
                                                                                                ifelse(caps$Year == 2023, 82500000,
                                                                                                       ifelse(caps$Year == 2024, 83500000,
                                                                                                              ifelse(caps$Year == 2025, 88000000, 95500000))))))))))))))

# Calculate cap percnetages
caps$prct <- caps$Active / caps$Salary.Cap
caps$tot_prct <- caps$Total.Cap / caps$Salary.Cap
VGK <- caps[which(caps$Team == "VGK"),]
COL <- caps[which(caps$Team == "COL"),]
plot(VGK$Year, VGK$prct)
plot(COL$Year, COL$prct)

# This code looked at team performance over time. I did not find anything that accentuated my presentation
# team_stats <- read.csv("data/teams_2008_to_2024.csv")
# team_all <- team_stats[which(team_stats$situation == "all"),]
# team_all$season <- team_all$season + 1
# 
# table(caps$Team)
# table(team_all$team)
# for(i in 1:nrow(caps)){
#   if(caps$Team[i] == "PHX"){
#     caps$Team[i] <- "ARI"
#   } else if (caps$Team[i] == "WAS"){
#     caps$Team[i] <- "WSH"
#   }
# }
# for(i in 1:nrow(team_all)){
#   if(team_all$team[i] == "L.A"){
#     team_all$team[i] <- "LAK"
#   } else if (team_all$team[i] == "N.J"){
#     team_all$team[i] <- "NJD"
#   } else if (team_all$team[i] == "S.J"){
#     team_all$team[i] <- "SJS"
#   } else if (team_all$team[i] == "T.B"){
#     team_all$team[i] <- "TBL"
#   }
# }
# 
# caps$xGP <- 0
# for(i in 1:nrow(caps)){
#   tm <- caps$Team[i]
#   year <- caps$Year[i]
#   caps$xGP[i] <- team_all[which(team_all$team == tm & team_all$season == year),]$xGoalsPercentage[1]
# }
# # team_all[which(team_all$team == "STL" & team_all$season == 2023),]$xGoalsPercentage[1]
# 
# caps <- caps[which(!is.na(caps$xGP)),]
# # decomp <- stl(ts(caps$xGP, frequency=1), s.window = "periodic")
# 
# caps$over_80 <- ifelse(caps$prct >= 0.80, 1, 0)
# 
# tpy <- data.frame(
#   year = numeric(0),
#   pct = numeric(0)
# )
# for(year in unique(caps$Year)){
#   yr_avg <- mean(caps[which(caps$Year == year),]$tot_prct)
#   tpy <- rbind(tpy, data.frame(year = year, pct = yr_avg))
# }

# Make a time series plot of average team salaries
plot(tpy$year, tpy$pct)

ts_pct <- ts(tpy$pct, start = c(2012, 1), frequency = 1)
ggplot(tpy, aes(x = year, y = pct)) +
  geom_point() +  # Original data line
  geom_line(aes(y = rollmean(pct, k = 3, fill = NA, align = "right")), color = "red") + 
  labs(title = "On average, teams are spending an increasing percentage of the salary cap.") +
  theme(text = element_text(size=15)) +
  xlab("Year") + 
  ylab("Average Percentage of the Salary Cap Spent")

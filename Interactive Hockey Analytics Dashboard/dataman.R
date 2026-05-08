# All the data management needed for the app.
# The resulting dataframes can be found in the data folder

library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(FNN)
library(ggplot2)

rm(list=ls())

# Get shift data for gantt chart (did not actually use) ####
# This code was to build a gantt chart that showed when during a game the player was on the ice
shifts.full <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/nhlshifts25_26.csv")

shifts <- shifts.full[0,-1]

games <- unique(shifts.full$gameId)
for (i in 1:length(games)){
  gamestats <- shifts.full[which(shifts.full$gameId == games[i]),]
  g <- unique(gamestats[,-1])
  shifts <- rbind(shifts, g)
  if(i %% 10 == 0){
    print(paste0(i, "/", length(games)))
  }
}

shifts$durSec <- as.numeric(ms(shifts$duration))
shifts$startSec <- ((shifts$period - 1) * 1200) + as.numeric(ms(shifts$startTime))
shifts$endSec <- ((shifts$period - 1) * 1200) + as.numeric(ms(shifts$endTime))

shifts$gameType <- substr(as.character(shifts$gameId), 6, 6)
shifts <- shifts[which(shifts$gameType == "2"),]

pid <- 8478542
lastFive <- unique(shifts[which(shifts$playerId == pid),]$gameId)
lastFive <- head(sort(lastFive, decreasing = TRUE), n=5)

playerShifts <- shifts %>%
  filter(playerId == pid,
         gameId %in% lastFive)
playerShifts <- playerShifts %>%
  mutate(gameId = factor(gameId, levels = lastFive))

ggplot(playerShifts, aes(
  x = startSec,
  xend = endSec,
  y = gameId,
  yend = gameId
)) +
  geom_segment(size = 6, color = "steelblue") +
  labs(
    title = "Player Shift Timeline",
    x = "Game Time (seconds)",
    y = "Game ID"
  ) +
  theme_minimal()

## Get players on ice at each time ####
# This section synthesized a dataset of all the players on the ice at every time during the season
hometeams <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/homeaway25_26.csv")

setDT(shifts)
setDT(hometeams)

setkey(shifts, gameId)
setkey(hometeams, gameId)

shifts[hometeams, home_team := i.homeTeamAbbrev]
shifts[, is_home := teamAbbrev == home_team]

shifts[, `:=`(
  start = as.numeric(startSec),
  end   = as.numeric(endSec)
)]

shifts_small <- shifts[, .(gameId, start, end, playerId, teamAbbrev, is_home)]

shifts_small <- shifts_small[start < end]

setkey(shifts_small, gameId, start, end)

pts <- shifts_small[, .(time = sort(unique(c(start, end)))), by = gameId]

gc()

pts[, next_time := shift(time, type = "lead"), by = gameId]

intervals <- pts[!is.na(next_time) & time < next_time,
                 .(gameId, start = time, end = next_time)]

setkey(intervals, gameId, start, end)

gc()

active <- foverlaps(
  intervals,
  shifts_small,
  by.x = c("gameId", "start", "end"),
  by.y = c("gameId", "start", "end"),
  type = "within",
  nomatch = 0L
)

active[, `:=`(
  start = i.start,
  end   = i.end
)]

gc()

active[, slot := seq_len(.N), by = .(gameId, start, end, is_home)]

gc()

setorder(active, gameId, start, end, is_home, playerId)

active[, slot := seq_len(.N), by = .(gameId, start, end, is_home)]

active <- active[slot <= 6]

home <- active[is_home == TRUE]
away <- active[is_home == FALSE]

final_dataset <- unique(active[, .(gameId, start, end)])

for (i in 1:6) {
  final_dataset[, paste0("home_", i) := NA_integer_]
  final_dataset[, paste0("away_", i) := NA_integer_]
}

setkey(final_dataset, gameId, start, end)

for (i in 1:6) {
  tmp <- home[slot == i, .(gameId, start, end, playerId)]
  setnames(tmp, "playerId", paste0("home_", i))
  final_dataset[tmp, paste0("home_", i) := get(paste0("home_", i))]
}

for (i in 1:6) {
  tmp <- away[slot == i, .(gameId, start, end, playerId)]
  setnames(tmp, "playerId", paste0("away_", i))
  final_dataset[tmp, paste0("away_", i) := get(paste0("away_", i))]
}

setorder(final_dataset, gameId, start)

gc()

setkey(tmp, gameId, start, end)
for (i in 1:6) {
  tmp <- home[slot == i, .(gameId, start, end, playerId)]
  setkey(tmp, gameId, start, end)
  
  final_dataset[tmp, (paste0("home_", i)) := i.playerId]
}

for (i in 1:6) {
  tmp <- away[slot == i, .(gameId, start, end, playerId)]
  setkey(tmp, gameId, start, end)
  
  final_dataset[tmp, (paste0("away_", i)) := i.playerId]
}

setcolorder(final_dataset,
            c("gameId", "start", "end",
              paste0("home_", 1:6),
              paste0("away_", 1:6))
)
setorder(final_dataset, gameId, start)

write.csv(final_dataset, "lines25_26.csv", row.names = FALSE)


### Get stats for each shift ####
# This section sums up all the events that happened during each shift
pbp <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/nhl_2025_2026_pbp.csv")
lines <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/lines25_26.csv")

table(pbp$eventType)

events <- pbp[which(pbp$eventType == "blocked-shot" | pbp$eventType == "giveaway" | pbp$eventType == "goal" | pbp$eventType == "hit" | pbp$eventType == "missed-shot" | pbp$eventType == "shot-on-goal" | pbp$eventType == "takeaway"),]
table(events$eventType)

events$gameType <- as.numeric(substr(as.character(events$gameId), 6, 6))
events <- events[which(events$gameType == 2),]

team_ids <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/team_ids.csv")

# Get the team abbreviation for the home team, away team, and team doing the event
setDT(events)
setDT(team_ids)
setDT(hometeams)

setkey(events, teamId)
setkey(team_ids, teamId)
events[team_ids, event_team := i.team]

setkey(events, gameId)
setkey(hometeams, gameId)
events[hometeams, home_team := i.homeTeamAbbrev]
events[hometeams, away_team := i.awayTeamAbbrev]

setDF(events)
events$isHome <- ifelse(events$home_team == events$event_team, 1, 0)

# Create indicator variables for each event type
events$h.blck <- ifelse(events$eventType == "blocked-shot" & events$isHome == 1, 1, 0)
events$h.give <- ifelse(events$eventType == "giveaway" & events$isHome == 1, 1, 0)
events$h.goal <- ifelse(events$eventType == "goal" & events$isHome == 1, 1, 0)
events$h.hit <- ifelse(events$eventType == "hit" & events$isHome == 1, 1, 0)
events$h.miss <- ifelse(events$eventType == "missed-shot", 1, 0)
events$h.shot <- ifelse(events$eventType == "shot-on-goal" & events$isHome == 1 | events$eventType == "goal" & events$isHome == 1, 1, 0)
events$h.take <- ifelse(events$eventType == "takeaway" & events$isHome == 1, 1, 0)

events$a.blck <- ifelse(events$eventType == "blocked-shot" & events$isHome == 0, 1, 0)
events$a.give <- ifelse(events$eventType == "giveaway" & events$isHome == 0, 1, 0)
events$a.goal <- ifelse(events$eventType == "goal" & events$isHome == 0, 1, 0)
events$a.hit <- ifelse(events$eventType == "hit" & events$isHome == 0, 1, 0)
events$a.miss <- ifelse(events$eventType == "missed-shot", 0, 0)
events$a.shot <- ifelse(events$eventType == "shot-on-goal" & events$isHome == 0 | events$eventType == "goal" & events$isHome == 0, 1, 0)
events$a.take <- ifelse(events$eventType == "takeaway" & events$isHome == 0, 1, 0)

events$time <- period_to_seconds(ms(events$timeInPeriod))
events$time <- events$time + (1200 * events$period) - 1200
events$time <- as.integer(events$time)

setDT(events)
setDT(lines)
# shots.home <- shots[team == "HOME"]
# shots.away <- shots[team == "AWAY"]

events.tmp <- events[h.blck == 1]
lines[, h.blck := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(h.blck), by= .EACHI]$V1]
events.tmp <- events[h.give == 1]
lines[, h.give := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(h.give), by= .EACHI]$V1]
events.tmp <- events[h.goal == 1]
lines[, h.goal := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(h.goal), by= .EACHI]$V1]
events.tmp <- events[h.hit == 1]
lines[, h.hit := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(h.hit), by= .EACHI]$V1]
events.tmp <- events[h.miss == 1]
lines[, h.miss := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(h.miss), by= .EACHI]$V1]
events.tmp <- events[h.shot == 1]
lines[, h.shot := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(h.shot), by= .EACHI]$V1]
events.tmp <- events[h.take == 1]
lines[, h.take := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(h.take), by= .EACHI]$V1]

events.tmp <- events[a.blck == 1]
lines[, a.blck := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(a.blck), by= .EACHI]$V1]
events.tmp <- events[a.give == 1]
lines[, a.give := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(a.give), by= .EACHI]$V1]
events.tmp <- events[a.goal == 1]
lines[, a.goal := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(a.goal), by= .EACHI]$V1]
events.tmp <- events[a.hit == 1]
lines[, a.hit := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(a.hit), by= .EACHI]$V1]
events.tmp <- events[a.miss == 1]
lines[, a.miss := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(a.miss), by= .EACHI]$V1]
events.tmp <- events[a.shot == 1]
lines[, a.shot := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(a.shot), by= .EACHI]$V1]
events.tmp <- events[a.take == 1]
lines[, a.take := events.tmp[.SD, on = .(gameId, time >= start, time < end), sum(a.take), by= .EACHI]$V1]

setDF(lines)
lines$gameType <- as.numeric(substr(as.character(lines$gameId), 6, 6))

lines.rs <- lines[which(lines$gameType == 2),]

# write.csv(lines.rs, "lines_regular_season.csv", row.names = FALSE)
# lines[, xGH := shots.home[.SD, on = .(gameId, time >= start, time < end), sum(xGoal), by= .EACHI]$V1]
# lines[, xGA := shots.away[.SD, on = .(gameId, time >= start, time < end), sum(xGoal), by= .EACHI]$V1]

##### Get Line Stats ####
# In this section I format a couple datasets which allow me to enter in a list of player ids and receive the statistics for when those players played together

# Remove Goaltenders from the dataset
lines.rs <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/lines_regular_season.csv")
goalies <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/goalies.csv")

gList <- unique(goalies$playerId)

lines.rs[] <- lapply(lines.rs, function(col) {
  col[col %in% gList] <- NA
  col
})

setDT(lines.rs)
home_lines <- lines.rs[, .(
  team = "home",
  players = list(sort(c(home_1, home_2, home_3, home_4, home_5, home_6))),
  give_for = h.give,
  give_against = a.give,
  hit_for = h.hit,
  hit_against = a.hit,
  take_for = h.take,
  take_against = a.take,
  shot_for = h.shot,
  shot_against = a.shot,
  miss_for = h.miss,
  miss_against = a.miss,
  blck_for = h.blck,
  blck_against = a.blck,
  goal_for = h.goal,
  goal_against = a.goal
)]

away_lines <- lines.rs[, .(
  team = "away",
  players = list(sort(c(away_1, away_2, away_3, away_4, away_5, away_6))),
  give_for = a.give,
  give_against = h.give,
  hit_for = a.hit,
  hit_against = h.hit,
  take_for = a.take,
  take_against = h.take,
  shot_for = a.shot,
  shot_against = h.shot,
  miss_for = a.miss,
  miss_against = h.miss,
  blck_for = a.blck,
  blck_against = h.blck,
  goal_for = a.goal,
  goal_against = h.goal
)]

all_lines <- rbind(home_lines, away_lines)

library(data.table)
setDT(lines.rs)

# Replace NA stats with 0
stat_cols <- grep("^[ha]\\.", names(lines.rs), value = TRUE)
for (col in stat_cols) {
  set(lines.rs, which(is.na(lines.rs[[col]])), col, 0)
}

# Create player list columns
lines.rs[, home_players := lapply(.I, function(i) {
  na.omit(c(home_1[i], home_2[i], home_3[i],
            home_4[i], home_5[i], home_6[i]))
})]

lines.rs[, away_players := lapply(.I, function(i) {
  na.omit(c(away_1[i], away_2[i], away_3[i],
            away_4[i], away_5[i], away_6[i]))
})]


n <- nrow(lines.rs)

home_index <- data.table(
  player   = unlist(lines.rs$home_players),
  shift_id = rep(seq_len(n), times = lengths(lines.rs$home_players))
)

setkey(home_index, player)

away_index <- data.table(
  player   = unlist(lines.rs$away_players),
  shift_id = rep(seq_len(n), times = lengths(lines.rs$away_players))
)

setkey(away_index, player)

get_stats(c(8473507, 8475179), home_index, lines.rs)

lines.rs[, duration := end - start]

get_stats <- function(player_ids, index, dt) {
  
  matches <- index[J(player_ids), .N, by = shift_id][N == length(player_ids)]
  
  rows <- dt[matches$shift_id]
  
  stats <- rows[, lapply(.SD, sum),
                .SDcols = patterns("^h\\.|^a\\.")]
  
  stats[, toi := sum(rows$duration)]  # total ice time in seconds
  
  return(stats)
}

thing <- get_stats(c(8473507, 8475179), away_index, lines.rs)
saveRDS(lines.rs, "lines.rds")
saveRDS(home_index, "home_index.rds")
saveRDS(away_index, "away_index.rds")


# home_dt <- lines.rs[, .(
#   shift_id = .I,
#   team = "home",
#   players = home_players,
#   for = .SD[, patterns("^h\\.")],
#   against = .SD[, patterns("^a\\.")],
#   duration
# )]
# 
# away_dt <- dt[, .(
#   shift_id = .I,
#   team = "away",
#   players = away_players,
#   for = .SD[, patterns("^a\\.")],
#   against = .SD[, patterns("^h\\.")],
#   duration
# )]
# 
# team_dt <- rbind(home_dt, away_dt)

h_cols <- grep("^h\\.", names(lines.rs), value = TRUE)
a_cols <- grep("^a\\.", names(lines.rs), value = TRUE)

# Home perspective
home_dt <- copy(lines.rs)[, c("shift_id", "team", "duration") := .(.I, "home", duration)]
home_dt[, players := home_players]

# Rename stats: h.* → for_*, a.* → against_*
setnames(home_dt, h_cols, paste0("for_", sub("^h\\.", "", h_cols)))
setnames(home_dt, a_cols, paste0("against_", sub("^a\\.", "", a_cols)))
gc()

# Away perspective
away_dt <- copy(lines.rs)[, c("shift_id", "team", "duration") := .(.I, "away", duration)]
away_dt[, players := away_players]

# Rename stats: a.* → for_*, h.* → against_*
setnames(away_dt, a_cols, paste0("for_", sub("^a\\.", "", a_cols)))
setnames(away_dt, h_cols, paste0("against_", sub("^h\\.", "", h_cols)))

team_dt <- rbind(home_dt, away_dt, fill = TRUE)


# player_index <- data.table(
#   player = unlist(team_dt$players),
#   shift_id = rep(team_dt$shift_id, times = lengths(team_dt$players))
# )
player_index <- data.table(
  player = unlist(linestats$players),
  shift_id = rep(linestats$shift_id, times = lengths(linestats$players)),
  team = rep(linestats$team, times = lengths(linestats$players))
)
setkey(player_index, player)

get_stats <- function(player_ids) {
  
  # matches <- player_index[J(player_ids), .N, by = shift_id][N == length(player_ids)]
  # 
  # rows <- linestats[shift_id %in% matches$shift_id]
  matches <- player_index[
    J(player_ids),
    .N,
    by = .(shift_id, team)
  ][N == length(player_ids)]
  
  rows <- linestats[
    matches,
    on = .(shift_id, team)
  ]
  
  result <- rows[, lapply(.SD, sum),
                 .SDcols = patterns("^for_|^against_")]
  
  result[, toi := sum(rows$duration)]
  
  result
}

thing <- get_stats(c(8473507, 8475179))

saveRDS(player_index, "player_index.rds")
saveRDS(team_dt, "linestats.rds")
###### Format gl ####
# In this section I format the datasets which include the most commonly deployed linesand pairings for each team in each game
gl <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/lines_ids.csv")

lines <- gl[which(gl$position == "line"),]
pairs <- gl[which(gl$position == "pairing"),]

# Remove hyphens from names
lines$hyphenCount <- str_count(lines$name, "-")
unique(lines[which(lines$hyphenCount == 3),]$name)
lines$name <- gsub("Aston-Reese", "Aston Reese", lines$name)
lines$name <- gsub("Barr-Boulet", "Barr Boulet", lines$name)
lines$name <- gsub("Brandsegg-Nygrd", "Brandsegg Nygrd", lines$name)
lines$name <- gsub("Nugent-Hopkins", "Nugent Hopkins", lines$name)
lines$name <- gsub("Aube-Kubel", "Aube Kubel", lines$name)
lines$name <- gsub("Harvey-Pinard", "Harvey Pinard", lines$name)

pairs$hyphenCount <- str_count(pairs$name, "-")
unique(pairs[which(pairs$hyphenCount == 2),]$name)
pairs$name <- gsub("Sandin-Pellikka", "Sandin Pellikka", pairs$name)
pairs$name <- gsub("Bernard-Docker", "Bernard Docker", pairs$name)
pairs$name <- gsub("Gustafsson-Nyberg", "Gustafsson Nyberg", pairs$name)
pairs$name <- gsub("Ekman-Larsson", "Ekman Larsson", pairs$name)

lines$L1 <- ""
lines$L2 <- ""
lines$L3 <- ""
for(i in 1:nrow(lines)){
  lines$L1[i] <- strsplit(lines$name[i], split = "-")[[1]][1]
  lines$L2[i] <- strsplit(lines$name[i], split = "-")[[1]][2]
  lines$L3[i] <- strsplit(lines$name[i], split = "-")[[1]][3]
}

pairs$P1 <- ""
pairs$P2 <- ""
for(i in 1:nrow(pairs)){
  pairs$P1[i] <- strsplit(pairs$name[i], split = "-")[[1]][1]
  pairs$P2[i] <- strsplit(pairs$name[i], split = "-")[[1]][2]
}

# lines$ID1 <- as.integer(substr(lines$lineId, 1, 7))
# lines$ID2 <- as.integer(substr(lines$lineId, 8, 14))
# lines$ID3 <- as.integer(substr(lines$lineId, 15, 21))
# pairs$ID1 <- as.integer(substr(pairs$lineId, 8, 14))
# pairs$ID2 <- as.integer(substr(pairs$lineId, 15, 21))

lines <- lines[, c("lineId", "name", "gameId", "playerTeam", "opposingTeam", "home_or_away", "gameDate", "icetime", "L1", "L2", "L3", "ID1", "ID2", "ID3")]
pairs <- pairs[, c("lineId", "name", "gameId", "playerTeam", "opposingTeam", "home_or_away", "gameDate", "icetime", "P1", "P2", "ID1", "ID2")]

lines <- lines[order(lines$gameId, lines$playerTeam, -lines$icetime),]
pairs <- pairs[order(pairs$gameId, pairs$playerTeam, -pairs$icetime),]

Team <- "ANA"
gid <- max(lines[which(lines$playerTeam == Team),]$gameId)
lines[which(lines$playerTeam == Team & lines$gameId == gid),]

write.csv(lines, "line_time.csv", row.names = FALSE)
write.csv(pairs, "pair_time.csv", row.names = FALSE)

# please <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/2025.csv")
####### Get id/name dataset ####
try <- s %>% distinct(shooterName, .keep_all = TRUE)
pids <- try[,13:14]
colnames(pids) <- c("playerId", "name")
pids[nrow(pids) + 1,] <- list(8483678, "Elias Pettersson")
write.csv(pids, "playerids.csv", row.names = FALSE)

######## Manage shot data ####
shots <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/shots_2025.csv")
shots$A_skaters <- shots$shootingTeamDefencemenOnIce + shots$shootingTeamForwardsOnIce
shots$D_skaters <- shots$defendingTeamDefencemenOnIce + shots$defendingTeamForwardsOnIce
shots$situation <- ifelse(shots$A_skaters == shots$D_skaters, "evenStrength",
                          ifelse(shots$A_skaters > shots$D_skaters, "powerPlay", "penaltyKill"))
# shots$powerPlay <- ifelse(shots$A_skaters > shots$D_skaters, 1, 0)
# shots$penaltyKill <- ifelse(shots$A_skaters < shots$D_skaters, 1, 0)

shots <- shots[which(shots$isPlayoffGame == 0),]
shots$gameId <- paste0("20250", shots$game_id)
shots$gameId <- as.integer(shots$gameId)
s <- shots[,c("shotID", "gameId", "team", "homeTeamCode", "awayTeamCode", "xCordAdjusted", "yCordAdjusted", "xGoal", "shooterPlayerId", "shooterName", "situation")]
s$shootingTeam <- ifelse(s$team == "HOME", s$homeTeamCode, s$awayTeamCode)

write.csv(s, "shots_2025_man.csv", row.names = FALSE)

######### Manage skater data ####
skaters <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/skaters.csv")
skaters$pos <- ifelse(skaters$position == "D", "Defenseman", "Forward")
skaters$G_60 <- (skaters$I_F_goals * 3600) / skaters$icetime
skaters$PA_60 <- (skaters$I_F_primaryAssists * 3600) / skaters$icetime
skaters$SA_60 <- (skaters$I_F_secondaryAssists * 3600) / skaters$icetime
skaters$ShotAt_60 <- (skaters$I_F_shotAttempts * 3600) / skaters$icetime
skaters$PIM_60 <- (skaters$I_F_penalityMinutes * 3600) / skaters$icetime
skaters$H_60 <- (skaters$I_F_hits * 3600) / skaters$icetime
skaters$Give_60 <- (skaters$I_F_giveaways * 3600) / skaters$icetime
skaters$Take_60 <- (skaters$I_F_takeaways * 3600) / skaters$icetime
skaters$Reb_60 <- (skaters$I_F_rebounds * 3600) / skaters$icetime
skaters$Blck_60 <- (skaters$shotsBlockedByPlayer * 3600) / skaters$icetime

write.csv(skaters, "skaters.csv", row.names = FALSE)

# s.red <- na.omit(skaters[which(skaters$situation == "all"), c("playerId", "G_60", "PA_60", "SA_60", "ShotAt_60", "PIM_60", "H_60", "Give_60", "Take_60", "Reb_60", "Blck_60")])
# s.red[,-1] <- scale(s.red[,-1])
# guy <- s.red[which(s.red$playerId == pid), 2:11, drop = FALSE]
# s.knn <- s.red[which(s.red$playerId != pid),2:11]
# knn_result <- get.knnx(data = s.knn, query = guy, k=3)
# knn_result$nn.index
# s.red[625,]$playerId

########## Info for presentation ####
teamlines <- read.csv("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Data/teamlinestats.csv")
Flines <- teamlines[which(teamlines$position == "line" & teamlines$icetime >= 1200),]
Dpairs <- teamlines[which(teamlines$position == "pairing" & teamlines$icetime >= 1200),]
for (team in unique(teamlines$team)){
  print(paste(team, nrow(Flines[which(Flines$team == team),]), nrow(Dpairs[which(Dpairs$team == team),]), nrow(Dpairs[which(Dpairs$team == team),]) + nrow(Flines[which(Flines$team == team),])))
}

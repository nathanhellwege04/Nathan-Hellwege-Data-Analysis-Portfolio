# Nathan Hellwege
# Data management file
# This file takes a while to run, but the resulting flat files are included in the data folder

library(dplyr)
library(lubridate)
library(data.table)

rm(list=ls())

# Data Import and Basic Operations ####
shifts.full <- read.csv("nhlshifts.csv")
tmp <- shifts.full[which(shifts.full$id == 10319747),]
tmp[1,]

t1 <- c(1, 2, 3, 4, 5, NA)
t2 = c(1, 2, 3, 5, 4, NA)
if(setequal(t1, t2)){
  print("Yes")
}

length(unique(shifts.full$gameId))
length(unique(shifts.full$id))

n_distinct(shifts.full$gameId, shifts.full$playerId)

shifts <- shifts.full[0,-1]
# for (id in unique(shifts.full$id)) {
#   row <- shifts.full[which(shifts.full$id == id),]
#   shifts <- rbind(shifts, row[1,0])
# }
# 
# shifts <- unique(shifts.full[,-1])

# Remove duplicate shifts
games <- unique(shifts.full$gameId)
for (i in 1:length(games)){
  gamestats <- shifts.full[which(shifts.full$gameId == games[i]),]
  g <- unique(gamestats[,-1])
  shifts <- rbind(shifts, g)
  if(i %% 10 == 0){
    print(paste0(i, "/", length(games)))
  }
}

# Saving progress
# write.csv(shifts, file="shifts.csv", row.names = FALSE)

shifts$durSec <- as.numeric(ms(shifts$duration))
shifts$startSec <- ((shifts$period - 1) * 1200) + as.numeric(ms(shifts$startTime))
shifts$endSec <- ((shifts$period - 1) * 1200) + as.numeric(ms(shifts$endTime))

## Add home team indicator ####
hometeams <- read.csv("data/homeaway.csv")

# This code theoretically works, but would take hours to run
# home_list <- list()
# for(i in 1:length(games)){
#   id <- games[i]
#   home_team <- hometeams[which(hometeams$gameId == id),]
#   gamestats <- shifts[which(shifts$gameId == id),]
#   if (i %% 10 == 0){
#     print(paste0(i, "/6599"))
#   }
# }
# hometeams[which(hometeams$gameId == 2020020015),]$homeTeamAbbrev[1]

setDT(shifts)
setDT(hometeams)

setkey(shifts, gameId)
setkey(hometeams, gameId)

shifts[hometeams, home_team := i.homeTeamAbbrev]
shifts[, is_home := teamAbbrev == home_team]

### Synthesize new dataset that has every player on the ice at a given time ####
setDT(shifts)

shifts[, `:=`(
  start = as.numeric(startSec),
  end   = as.numeric(endSec)
)]

shifts_small <- shifts[, .(gameId, start, end, playerId, teamAbbrev, is_home)]
shifts_small <- shifts_small[start < end]

setkey(shifts_small, gameId, start, end)

# Get change points
pts <- shifts_small[, .(time = sort(unique(c(start, end)))), by = gameId]

# Running this code crashed my computer a couple times so I'm spamming gc() to help prevent it
gc()

pts[, next_time := shift(time, type = "lead"), by = gameId]
intervals <- pts[!is.na(next_time) & time < next_time,
                 .(gameId, start = time, end = next_time)]
setkey(intervals, gameId, start, end)
gc()

# Join player shifts if they overlapped
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

# Get the start and end times for shifts
active[, slot := seq_len(.N), by = .(gameId, start, end, is_home)]
gc()

setorder(active, gameId, start, end, is_home, playerId)
active[, slot := seq_len(.N), by = .(gameId, start, end, is_home)]
active <- active[slot <= 6]

# This is why I needed that home indicator earlier
home <- active[is_home == TRUE]
away <- active[is_home == FALSE]

final_dataset <- unique(active[, .(gameId, start, end)])

# Create columns for each player on the ice
for (i in 1:6) {
  final_dataset[, paste0("home_", i) := NA_integer_]
  final_dataset[, paste0("away_", i) := NA_integer_]
}

setkey(final_dataset, gameId, start, end)

# Pretty sure this code block also crashed my computer (you can't say I'm not pushing my computer to its limit)
# for (i in 1:6) {
#   tmp <- home[slot == i, .(gameId, start, end, playerId)]
#   setnames(tmp, "playerId", paste0("home_", i))
#   final_dataset[tmp, paste0("home_", i) := get(paste0("home_", i))]
# }
# 
# for (i in 1:6) {
#   tmp <- away[slot == i, .(gameId, start, end, playerId)]
#   setnames(tmp, "playerId", paste0("away_", i))
#   final_dataset[tmp, paste0("away_", i) := get(paste0("away_", i))]
# }

setorder(final_dataset, gameId, start)
gc()

# This code merely froze my computer for a couple minutes. Much better!
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

# Save the dataset so I never need to run this code again
write.csv(final_dataset, file="lines.csv", row.names = FALSE)

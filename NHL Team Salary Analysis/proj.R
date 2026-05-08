# Nathan Hellwege
# Effects of NHL Salary Allocation

# In this project, I use nhl salary data from the 2022-2025 seasons to investigate 
# the relationship between team success and how those teams spend their money

rm(list=ls())


# Load data files
players <- read.csv("Data/cap_data_22-25.csv")
head(players)
teams <- read.csv("Data/team_stats.csv")
head(teams)

# Rename some columns (R doesn't like special characters)
colnames(players)[colnames(players) == "Cap.Hit"] <- "Hit"
colnames(players)[colnames(players) == "Cap.."] <- "Perc"
colnames(teams)[colnames(teams) == "PTS."] <- "PPG"


# Combine LW, C, and RW to one group, F
players$P <- ifelse(players$Pos == 'G', 'G',
                    ifelse(players$Pos == 'D', 'D', 'F'))


# Playoff indicator 1 if team has an asterisk, then remove the asterisk
teams$Playoffs <- ifelse(substr(teams$Team, nchar(teams$Team), nchar(teams$Team)) == '*', 1, 0)
library(stringr)
teams$Team <- stringr::str_replace(teams$Team, '\\*', '')

# Add team abbreviations
library(dplyr)
teams <- teams %>%
  mutate(team_abbr = case_when(
    Team == "Anaheim Ducks" ~ "ANA",
    Team == "Arizona Coyotes" ~ "ARI",
    Team == "Boston Bruins" ~ "BOS",
    Team == "Buffalo Sabres" ~ "BUF",
    Team == "Calgary Flames" ~ "CGY",
    Team == "Carolina Hurricanes" ~ "CAR",
    Team == "Chicago Blackhawks" ~ "CHI",
    Team == "Colorado Avalanche" ~ "COL",
    Team == "Columbus Blue Jackets" ~ "CBJ",
    Team == "Dallas Stars" ~ "DAL",
    Team == "Detroit Red Wings" ~ "DET",
    Team == "Edmonton Oilers" ~ "EDM",
    Team == "Florida Panthers" ~ "FLA",
    Team == "Los Angeles Kings" ~ "LAK",
    Team == "Minnesota Wild" ~ "MIN",
    Team == "Montreal Canadiens" ~ "MTL",
    Team == "Nashville Predators" ~ "NSH",
    Team == "New Jersey Devils" ~ "NJD",
    Team == "New York Islanders" ~ "NYI",
    Team == "New York Rangers" ~ "NYR",
    Team == "Ottawa Senators" ~ "OTT",
    Team == "Philadelphia Flyers" ~ "PHI",
    Team == "Pittsburgh Penguins" ~ "PIT",
    Team == "San Jose Sharks" ~ "SJS",
    Team == "Seattle Kraken" ~ "SEA",
    Team == "St. Louis Blues" ~ "STL",
    Team == "Tampa Bay Lightning" ~ "TBL",
    Team == "Toronto Maple Leafs" ~ "TOR",
    Team == "Vancouver Canucks" ~ "VAN",
    Team == "Vegas Golden Knights" ~ "VEG",
    Team == "Washington Capitals" ~ "WSH",
    Team == "Winnipeg Jets" ~ "WPG",
    Team == "Utah Hockey Club" ~ "UTA"
  ))

# Create Prds which represents the furthest playoff round a team made, or 5 if the team won the cup
teams$PW[is.na(teams$PW)] <- 0
teams$Prds <- ifelse(teams$PW == 16, 5, 
                     ifelse(teams$PW >= 12, 4,
                            ifelse(teams$PW >= 8, 3,
                                   ifelse(teams$PW >= 4, 2,
                                          ifelse(teams$Playoffs == 1, 1, 0)))))


# Replace na cap hits with league minimum salary (which is different in 2024)
players[is.na(players$Hit),]
players$Hit <- ifelse(is.na(players$Hit), 
                      ifelse(players$Year == 2024, 775000, 750000),
                      players$Hit)
players[is.na(players$Hit),]

players[is.na(players$Perc),]
players$Perc <- ifelse(is.na(players$Perc), 0.9, players$Perc)
players[is.na(players$Perc),]


# Correct misentered values
table(players$Hit)
players[players$Hit < 750000,]
players[275, c("Hit", "Perc")] <- c(1250000, 1.4)
players[1382, c("Hit", "Perc")] <- c(1250000, 1.4)
players[2512, c("Hit", "Perc")] <- c(1250000, 1.4)
players[1976, c("Clause", "Hit", "Perc")] <- c('-', 789167, 0.9)
players[3278, c("Clause", "Hit", "Perc")] <- c('-', 750000, 0.9)
players[4464, c("Clause", "Hit", "Perc")] <- c('-', 750000, 0.9)
players[4482, c("Clause", "Hit", "Perc")] <- c('-', 750000, 0.9)
players[132, c("Age", "Pos", "GP", "Clause", "Hit", "Perc", "P")] <- c(20, 'D', 28, '-', 838333, 1.0, 'D')
players[1871, c("Clause", "Hit", "Perc")] <- c('-', 775000, 0.9)
players[2995, c("Clause", "Hit", "Perc")] <- c('-', 750000, 0.9)
players[1031, c("Clause", "Hit", "Perc")] <- c('NMC', 9750000, 11.1)
players[2082, c("Clause", "Hit", "Perc")] <- c('-', 8460250, 10.1)
players[2082, c("Clause", "Hit", "Perc")] <- c('-', 8460250, 10.3)
players[4356, c("Clause", "Hit", "Perc")] <- c('-', 8460250, 10.4)


players[as.numeric(players$Hit) < 750000,] 
players$Hit <- as.numeric(players$Hit)
players$GP <- as.numeric(players$GP)

# Synthesize team salary data
# teams$TGD <- sum(players[which(players$Team == teams$team_abbr & players$Year == teams$Year),]$Hit)
TEAMS = c('ANA', 'ARI', 'ATL', 'BOS', 'BUF', 'CAR', 'CBJ', 'CGY', 'CHI', 'COL', 'DAL', 'DET', 'EDM', 'FLA', 'LAK', 'MIN', 'MTL', 'NJD', 'NSH', 'NYI', 'NYR', 'OTT', 'PHI', 'PIT', 'SEA', 'SJS', 'STL', 'TBL', 'TOR', 'UTA', 'VAN', 'VEG', 'WPG', 'WSH')
teams$Tot <- 0
for (year in 2022:2025){
  for (team in TEAMS){
    team_sum <- sum(players[which(players$Team == team & players$Year == year),]$Hit)
    if (team_sum > 0){
      teams[teams$team_abbr == team & teams$Year == year, "Tot"] <- team_sum 
    }
  }
}

players$Hit.adj <- (players$Hit * players$GP) / 82

teams$Tot.adj <- 0
for (year in 2022:2025){
  for (team in TEAMS){
    team_sum <- sum(players[which(players$Team == team & players$Year == year),]$Hit.adj)
    if (team_sum > 0){
      teams[teams$team_abbr == team & teams$Year == year, "Tot.adj"] <- team_sum 
    }
  }
}

# Adjusted total for all forwards
teams$F.adj <- 0
for (year in 2022:2025){
  for (team in TEAMS){
    team_sum <- sum(players[which(players$Team == team & players$Year == year & players$P == 'F'),]$Hit.adj)
    if (team_sum > 0){
      teams[teams$team_abbr == team & teams$Year == year, "F.adj"] <- team_sum 
    }
  }
}

#Adjusted total for all goaltenders
teams$G.adj <- 0
for (year in 2022:2025){
  for (team in TEAMS){
    team_sum <- sum(players[which(players$Team == team & players$Year == year & players$P == 'G'),]$Hit.adj)
    if (team_sum > 0){
      teams[teams$team_abbr == team & teams$Year == year, "G.adj"] <- team_sum 
    }
  }
}

#Adjusted total for all defensemen
teams$D.adj <- 0
for (year in 2022:2025){
  for (team in TEAMS){
    team_sum <- sum(players[which(players$Team == team & players$Year == year & players$P == 'D'),]$Hit.adj)
    if (team_sum > 0){
      teams[teams$team_abbr == team & teams$Year == year, "D.adj"] <- team_sum 
    }
  }
}

teams$F.perc <- teams$F.adj / teams$Tot.adj
teams$D.perc <- teams$D.adj / teams$Tot.adj
teams$G.perc <- teams$G.adj / teams$Tot.adj

teams$F.perc + teams$D.perc + teams$G.perc

teams$F.pl <- teams$F.adj / 12
teams$D.pl <- teams$D.adj / 6
teams$G.pl <- teams$G.adj / 2


# Data exploration figures
pl.22 <- players[which(players$Year == 2022),]
pl.23 <- players[which(players$Year == 2023),]
pl.24 <- players[which(players$Year == 2024),]
pl.25 <- players[which(players$Year == 2025),]

intervals<-seq(0,14,by=1)
hit.cut<-cut(players$Hit / 1000000, intervals, left=TRUE, right=FALSE)
hit.freq<-table(hit.cut)
hit.freq
par(mfrow=c(1,1))
# View(hit.freq)
hist(players$Hit / 1000000, 
     breaks=intervals, 
     right=TRUE, 
     main="Most cap hits are less than $1,000,000", 
     ylim = c(0,2500),
     xlab="Player Cap Hit (millions of dollars)", 
     col="blue")
box(col = "black", lwd =1)

# Create a table of cap hit values
# install.packages("gt")
library(gt)
cut.22 <- cut(pl.22$Hit / 1000000, intervals, left=TRUE, right=FALSE)
cut.23 <- cut(pl.23$Hit / 1000000, intervals, left=TRUE, right=FALSE)
cut.24 <- cut(pl.24$Hit / 1000000, intervals, left=TRUE, right=FALSE)
cut.25 <- cut(pl.25$Hit / 1000000, intervals, left=TRUE, right=FALSE)
freq.22 <- table(cut.22)
freq.23 <- table(cut.23)
freq.24 <- table(cut.24)
freq.25 <- table(cut.25)


comb_table <- data.frame(rbind(freq.22, freq.23, freq.24, freq.25))
# View(comb_table)
# comb_tib <- tibble(names = names(comb_table), size = islands)
colnames(comb_table) <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12-13", "13-14")
rownames(comb_table) <- c("2021-22", "2022-23", "2023-24", "2024-25")
hit_table <- gt(comb_table, rownames_to_stub = TRUE) |>
  tab_header(title = "Cap hit count by year", subtitle = "By millions of dollars") |>
  tab_stubhead(label = "Year")
hit_table 

plot(teams$Year ~ teams$Tot)

par(mfrow=c(1,2))
boxplot(teams$Tot/1000000, main="Total Cap Hit", ylab = "Total (millions of dollars)", ylim = c(40, 135))
boxplot(teams$Tot.adj/1000000, main="Adjusted Total Cap Hit", ylab = "Total (millions of dollars)", ylim = c(40, 135))


# Model the relationship between money spent and season points
tot.lm <- lm(PTS ~ Tot.adj, data=teams)
summary(tot.lm)

par(mfrow=c(1,1))

plot(teams$PTS ~ teams$Tot.adj, main="There's a clear relationship between total money spent and points won", xlab="Salary total (adjusted for games played)", ylab="Points at the end of the season")
abline(tot.lm, col="Cyan4")

sum(teams$PW)/32


# Create simple logistic regression model
simple.log <- glm(Playoffs ~ Tot.adj, family = binomial(link = logit), data = teams)
summary(simple.log)

Pred <- predict(simple.log, type = "response")
Bin <- round(Pred)
100*mean(teams$Playoffs == Bin)
200*mean(teams$Playoffs == 1 & teams$Playoffs == Bin)


plot(teams$PTS ~ teams$F.perc)
plot(teams$PTS ~ teams$D.perc)
plot(teams$PTS ~ teams$G.perc)

percs.lm <- lm(PTS ~ F.perc + D.perc + G.perc, data=teams)
summary(percs.lm)


# Conditional Probability
mean(teams[which(teams$F.pl > teams$D.pl),]$Playoffs)
mean(teams[which(teams$F.pl > teams$G.pl),]$Playoffs)
mean(teams[which(teams$D.pl > teams$G.pl),]$Playoffs)

# Compare in-team percentages to the mean
F.mean <- mean(teams$F.perc)
D.mean <- mean(teams$D.perc)
G.mean <- mean(teams$G.perc)

# Hypothesis testing
m.F <- mean(teams[which(teams$F.perc > F.mean),]$Playoffs)
sd.F <- sd(teams[which(teams$F.perc > F.mean),]$Playoffs)
n.F <- length(teams[which(teams$F.perc > F.mean),]$Playoffs)
z.F <- (m.F - 0.5) / sqrt((0.5 * (1-0.5))/n.F)
p.F <- 2 * pnorm(abs(z.F), lower.tail=FALSE)
p.F

m.D <- mean(teams[which(teams$D.perc > D.mean),]$Playoffs)
sd.D <- sd(teams[which(teams$D.perc > D.mean),]$Playoffs)
n.D <- length(teams[which(teams$D.perc > D.mean),]$Playoffs)
z.D <- (m.D - 0.5) / sqrt((0.5 * (1-0.5))/n.D)
p.D <- 2 * pnorm(abs(z.D), lower.tail=FALSE)
p.D

m.G <- mean(teams[which(teams$G.perc > G.mean),]$Playoffs)
sd.G <- sd(teams[which(teams$G.perc > G.mean),]$Playoffs)
n.G <- length(teams[which(teams$G.perc > G.mean),]$Playoffs)
z.G <- (m.G - 0.5) / sqrt((0.5 * (1-0.5))/n.G)
p.G <- 2 * pnorm(abs(z.G), lower.tail=FALSE)
p.G

# Highest paid players
teams$max <- 0
for (year in 2022:2025){
  for (team in TEAMS){
    max_player <- max(players[which(players$Team == team & players$Year == year),]$Hit)
    teams[teams$team_abbr == team & teams$Year == year, "max"] <- max_player
  }
}

teams$L1 <- 0
teams$L2 <- 0
teams$L3 <- 0
teams$P1 <- 0
teams$P2 <- 0
teams$G1 <- 0
teams$G2 <- 0
for (year in 2022:2025){
  for (team in TEAMS){
    top.3F <- top_n(players[which(players$Team == team & players$Year == year & players$P == 'F'),], 3, Hit)
    top.6F <- top_n(players[which(players$Team == team & players$Year == year & players$P == 'F'),], 6, Hit)
    top.9F <- top_n(players[which(players$Team == team & players$Year == year & players$P == 'F'),], 9, Hit)
    top.2D <- top_n(players[which(players$Team == team & players$Year == year & players$P == 'D'),], 2, Hit)
    top.4D <- top_n(players[which(players$Team == team & players$Year == year & players$P == 'D'),], 4, Hit)
    top.2G <- top_n(players[which(players$Team == team & players$Year == year & players$P == 'G'),], 2, GP)
    top.3F.sum <- sum(top.3F$Hit)
    top.6F.sum <- sum(top.6F$Hit)
    top.9F.sum <- sum(top.9F$Hit)
    top.2D.sum <- sum(top.2D$Hit)
    top.4D.sum <- sum(top.4D$Hit)
    teams[teams$team_abbr == team & teams$Year == year, "L1"] <- top.3F.sum
    teams[teams$team_abbr == team & teams$Year == year, "L2"] <- top.6F.sum - top.3F.sum
    teams[teams$team_abbr == team & teams$Year == year, "L3"] <- top.9F.sum - top.6F.sum
    teams[teams$team_abbr == team & teams$Year == year, "P1"] <- top.2D.sum
    teams[teams$team_abbr == team & teams$Year == year, "P2"] <- top.4D.sum - top.2D.sum
    teams[teams$team_abbr == team & teams$Year == year, "G1"] <- top.2G[which.max(top.2G$GP), "Hit"]
    teams[teams$team_abbr == team & teams$Year == year, "G2"] <- top.2G[which.min(top.2G$GP), "Hit"]
  }
}


# Count of players making 10% of the cap
players$Perc <- as.numeric(players$Perc)
teams$over10 <- 0
for (year in 2022:2025){
  for (team in TEAMS){
    over10 <- nrow(players[which(players$Team == team & players$Year == year & players$Perc >= 10),])
    teams[teams$team == team & teams$Year == year, "over10"] <- over10
  }
}
#length(players[which(players$Team == "WSH" & players$Year == 2025 & players$perc >= 10),])
#nrow(players[which(players$Team == "WSH" & players$Year == 2025 & players$perc >= 10),])

table(teams$over10)

barplot(table(teams$over10), main="Count of teams with n\ncontracts over 10%", xlab = "Number of contracts over 10%", ylab = "Team count", ylim = c(0,40))
#over10_table <- gt(table(teams$over10))
#over10_table 


# Hypothesis testing using the over10 variable
teams$over10 <- as.numeric(teams$over10)
m.0over10 <- mean(teams[which(teams$over10 == 0),]$Playoffs)
sd.0over10 <- sd(teams[which(teams$over10 == 0),]$Playoffs)
n.0over10 <- length(teams[which(teams$over10 == 0),]$Playoffs)
z.0over10 <- (m.0over10 - 0.5) / sqrt((0.5 * (1-0.5))/n.0over10)
p.0over10 <- 2 * pnorm(abs(z.0over10), lower.tail=FALSE)
p.0over10

m.1over10 <- mean(teams[which(teams$over10 == 1),]$Playoffs)
sd.1over10 <- sd(teams[which(teams$over10 == 1),]$Playoffs)
n.1over10 <- length(teams[which(teams$over10 == 1),]$Playoffs)
z.1over10 <- (m.1over10 - 0.5) / sqrt((0.5 * (1-0.5))/n.1over10)
p.1over10 <- 2 * pnorm(abs(z.1over10), lower.tail=FALSE)
p.1over10

m.2over10 <- mean(teams[which(teams$over10 == 2),]$Playoffs)
sd.2over10 <- sd(teams[which(teams$over10 == 2),]$Playoffs)
n.2over10 <- length(teams[which(teams$over10 == 2),]$Playoffs)
z.2over10 <- (m.2over10 - 0.5) / sqrt((0.5 * (1-0.5))/n.2over10)
p.2over10 <- 2 * pnorm(abs(z.2over10), lower.tail=FALSE)
p.2over10

m.3over10 <- mean(teams[which(teams$over10 >= 3),]$Playoffs)
sd.3over10 <- sd(teams[which(teams$over10 >= 3),]$Playoffs)
n.3over10 <- length(teams[which(teams$over10 >= 3),]$Playoffs)
z.3over10 <- (m.3over10 - 0.5) / sqrt((0.5 * (1-0.5))/n.3over10)
p.3over10 <- 2 * pnorm(abs(z.3over10), lower.tail=FALSE)
p.3over10

mean(teams[which(teams$over10 == 0),]$Tot.adj)
mean(teams[which(teams$over10 == 1),]$Tot.adj)
mean(teams[which(teams$over10 == 2),]$Tot.adj)
mean(teams[which(teams$over10 >= 3),]$Tot.adj)


# Predictors for playoff success
cor(teams$PW, teams$G2)
g2.lm <- lm(PW ~ teams$G2, data = teams)
summary(g2.lm)

pteams <- teams[which(teams$Playoffs == 1),]
median(pteams$G2)
mean(pteams[which(pteams$G2 >= median(pteams$G2)),]$PW)
mean(pteams[which(pteams$G2 < median(pteams$G2)),]$PW)

hist(pteams$PW, breaks=17,freq=FALSE, ylim=c(0, 0.4))

par(mfrow=c(2,1))
hist(pteams[which(pteams$G2 >= median(pteams$G2)),]$PW, breaks=16, freq=FALSE, ylim=c(0,0.35), col="lightblue", 
     main="Histogram of playoff teams with a well paid backup", xlab="Playoff wins")
hist(pteams[which(pteams$G2 < median(pteams$G2)),]$PW, breaks=16, freq=FALSE, ylim=c(0,0.35), xlim = c(0, 16), col="lightblue", 
     main="Histogram of playoff teams without a well paid backup", xlab="Playoff wins")


mean(pteams[which(pteams$over10 == 0),]$PW)
mean(pteams[which(pteams$over10 == 1),]$PW)
mean(pteams[which(pteams$over10 == 2),]$PW)
mean(pteams[which(pteams$over10 >= 3),]$PW)
par(mfrow=c(2,2))
hist(pteams[which(pteams$over10 == 0),]$PW, breaks=16, freq=FALSE, ylim=c(0,0.5), xlim=c(0,16), col="lightgreen", 
     main="Teams with no players making over 10% of the cap", xlab="Playoff wins")
hist(pteams[which(pteams$over10 == 1),]$PW, breaks=16, freq=FALSE, ylim=c(0,0.5), xlim=c(0,16), col="lightgreen", 
     main="Teams with one player making over 10% of the cap", xlab="Playoff wins")
hist(pteams[which(pteams$over10 == 2),]$PW, breaks=16, freq=FALSE, ylim=c(0,0.5), xlim=c(0,16), col="lightgreen", 
     main="Teams with two players making over 10% of the cap", xlab="Playoff wins")
hist(pteams[which(pteams$over10 >= 3),]$PW, breaks=16, freq=FALSE, ylim=c(0,0.5), xlim=c(0,16), col="lightgreen", 
     main="Teams with three or more players making over 10% of the cap", xlab="Playoff wins")
par(mfrow=c(1,1))
# Logistic regression models
library(caret)
teams$Playoffs <- factor(teams$Playoffs)
summary(teams$Playoffs)
myControl<-trainControl(method = "cv", number = 10)
log1 <- train(Playoffs ~ Tot.adj, 
                data = teams,
                trControl = myControl,
                method = "glm",
                family = binomial (link = logit),
                metric = "Accuracy")
cm1 <- confusionMatrix(log1)
sen1 <- cm1$table[2,2] / (cm1$table[2,2] + cm1$table[1,2])
spec1 <- cm1$table[1,1] / (cm1$table[1,1] + cm1$table[2,1])
log1
sen1
spec1

log2 <- train(Playoffs ~ F.perc + Tot.adj, 
              data = teams,
              trControl = myControl,
              method = "glm",
              family = binomial (link = logit),
              metric = "Accuracy")
cm2 <- confusionMatrix(log2)
sen2 <- cm2$table[2,2] / (cm2$table[2,2] + cm2$table[1,2])
spec2 <- cm2$table[1,1] / (cm2$table[1,1] + cm2$table[2,1])
log2
sen2
spec2

log3 <- train(Playoffs ~ F.perc + D.perc + F.perc*D.perc + Tot.adj, 
              data = teams,
              trControl = myControl,
              method = "glm",
              family = binomial (link = logit),
              metric = "Accuracy")
cm3 <- confusionMatrix(log3)
sen3 <- cm3$table[2,2] / (cm3$table[2,2] + cm3$table[1,2])
spec3 <- cm3$table[1,1] / (cm3$table[1,1] + cm3$table[2,1])
log3
sen3
spec3

log4 <- train(Playoffs ~ L1, 
              data = teams,
              trControl = myControl,
              method = "glm",
              family = binomial (link = logit),
              metric = "Accuracy")
cm4 <- confusionMatrix(log4)
sen4 <- cm4$table[2,2] / (cm4$table[2,2] + cm4$table[1,2])
spec4 <- cm4$table[1,1] / (cm4$table[1,1] + cm4$table[2,1])
log4
sen4
spec4

log5 <- train(Playoffs ~ L1 + P1 + G1, 
              data = teams,
              trControl = myControl,
              method = "glm",
              family = binomial (link = logit),
              metric = "Accuracy")
cm5 <- confusionMatrix(log5)
sen5 <- cm5$table[2,2] / (cm5$table[2,2] + cm5$table[1,2])
spec5 <- cm5$table[1,1] / (cm5$table[1,1] + cm5$table[2,1])
log5
sen5
spec5

log6 <- train(Playoffs ~ L1 + L2 + L3 + P1 + P2 + G1 + G2, 
              data = teams,
              trControl = myControl,
              method = "glm",
              family = binomial (link = logit),
              metric = "Accuracy")
cm6 <- confusionMatrix(log6)
sen6 <- cm6$table[2,2] / (cm6$table[2,2] + cm6$table[1,2])
spec6 <- cm6$table[1,1] / (cm6$table[1,1] + cm6$table[2,1])
log6
sen6
spec6

log7 <- train(Playoffs ~ L1 + F.perc + D.perc + F.perc*D.perc, 
              data = teams,
              trControl = myControl,
              method = "glm",
              family = binomial (link = logit),
              metric = "Accuracy")
cm7 <- confusionMatrix(log7)
sen7 <- cm7$table[2,2] / (cm7$table[2,2] + cm7$table[1,2])
spec7 <- cm7$table[1,1] / (cm7$table[1,1] + cm7$table[2,1])
log7
sen7
spec7

summary(log1)
summary(log2)
summary(log3)
summary(log4)
summary(log5)
summary(log6)
#m1.log <- glm(Playoffs ~ Tot.adj, data = teams, family = binomial)
#m2.log <- glm(Playoffs ~ F.perc + Tot.adj, data = teams, family = binomial)
#m3.log <- glm(Playoffs ~ F.perc + D.perc + F.perc*D.perc + Tot.adj, data = teams, family = binomial)
#m4.log <- glm(Playoffs ~ L1, data = teams, family = binomial)
#m5.log <- glm(Playoffs ~ L1 + P1 + G1, data = teams, family = binomial)
#m6.log <- glm(Playoffs ~ L1 + L2 + P1 + P2 + G1 + G2, data = teams, family = binomial)
#prob.1 <- predict(m1.log, newdata = teams, type = "response")
#pred.1 <- ifelse(prob.1 > 0.5, 1, 0)
#prob.2 <- predict(m2.log, newdata = test_data, type = "response")
#pred.2 <- ifelse(prob.2 > 0.5, 1, 0)
#prob.3 <- predict(m3.log, newdata = test_data, type = "response")
#pred.3 <- ifelse(prob.3 > 0.5, 1, 0)
#prob.4 <- predict(m4.log, newdata = test_data, type = "response")
#pred.4 <- ifelse(prob.4 > 0.5, 1, 0)
#prob.5 <- predict(m5.log, newdata = test_data, type = "response")
#pred.5 <- ifelse(prob.5 > 0.5, 1, 0)
#prob.6 <- predict(m6.log, newdata = test_data, type = "response")
#pred.6 <- ifelse(prob.6 > 0.5, 1, 0)


# Linear regression models
top5.lm <- lm(PW ~ L1 + P1, data=teams)
summary(top5.lm)
top10.lm <- lm(PW ~ L1 + L2 + P1 + P2, data=teams)
summary(top10.lm)
full.lm <- lm(PW ~ L1 + L2 + L3 + P1 + P2, data=teams)
summary(full.lm)
topl.lm <- lm(PW ~ L1, data=teams)
summary(topl.lm)

# Playoff analysis
pteams <- teams[which(teams$Playoffs == 1),]
head(pteams)

tot_pl.lm <- lm(PW ~ Tot.adj, data=pteams)
summary(tot_pl.lm)

plot(pteams$PW ~ pteams$Tot.adj)


# Linear regression models
lm1 <- train(PW ~ Tot.adj, 
              data = pteams,
              trControl = myControl,
              method = "lm")

lm2 <- train(PW ~ F.perc + Tot.adj, 
              data = pteams,
              trControl = myControl,
              method = "lm")

lm3 <- train(PW ~ F.perc + D.perc + F.perc*D.perc + Tot.adj, 
              data = pteams,
              trControl = myControl,
              method = "lm")

lm4 <- train(PW ~ L1, 
              data = pteams,
              trControl = myControl,
              method = "lm")

lm5 <- train(PW ~ L1 + P1 + G1, 
              data = pteams,
              trControl = myControl,
              method = "lm")

lm6 <- train(PW ~ L1 + L2 + L3 + P1 + P2 + G1 + G2, 
              data = pteams,
              trControl = myControl,
              method = "lm")

summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)


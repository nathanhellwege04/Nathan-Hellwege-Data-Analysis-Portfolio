# Nathan Hellwege
# Analysis of pre-draft player statistics and which players make the league
# 03/06/2026


# Housekeeping ####
rm(list=ls())
library(stringr)
library(stringi)
library(dplyr)
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(e1071)
library(ggplot2)
library(reshape2)


## Data Import and Management ####
draft_data <- read.csv("Data/updated_draft.csv")
p <- read.csv("Data/players.csv")
head(draft_data)
head(p)

d <- draft_data[which(draft_data$Pos != 'G'),]
d$nhl <- ifelse(is.na(d$GP), 0, 1)

# Format player names
p$Player <- gsub(" \\([^)]*\\)$", "", p$Player)
p$playerNoDia <- stri_trans_general(str = p$Player, id = "Latin-ASCII")


d$prior_season <- paste0(as.character(d$Year - 1), '-', as.character(d$Year))
#p[which(p$Player == d$Player[3]),][1,]
#is.na(p[which(p$Player == d$Player[3] & p$Year == d$prior_season[3]),][1,]$X.[1])

prior <- data.frame(
  X. = integer(0),
  Player = character(0),
  Team = character(0),
  GP = character(0),
  G = character(0),
  A = character(0),
  TP = character(0),
  PPG = character(0),
  PIM = character(0),
  X... = character(0),
  League = character(0),
  Year = character(0)
)

d$in_set <- 0
# for (i in 1:nrow(d)){
#   player_stats <- p[which(p$Player == d$Player[i] & p$Year == d$prior_season[i]),][1,]
#   if(nrow(player_stats) > 0){
#     prior <- rbind(prior, player_stats)
#     d$in_set[i] <- 1
#   } else{
#     year.back <- paste0(as.character(d$Year - 2), '-', as.character(d$Year - 1))
#     player_stats <- p[which(p$Player == d$Player[i] & p$Year == year.back),][1,]
#     if (nrow(player_stats) > 0){
#       prior <- rbind(prior, player_stats)
#       d$in_set[i] <- 1
#     } else {
#       prior[nrow(prior) + 1, ] <- NA
#     }
#   }
# }

for (i in 1:nrow(d)){
  player_stats <- p[which(p$playerNoDia == d$Player[i] & p$Year == d$prior_season[i]),][1,]
  if(!is.na(player_stats$X.[1])){
    d$in_set[i] <- 1
  }
  prior <- rbind(prior, player_stats)
}
sum(is.na(prior$X.))

#d$Am_Lg <- str_extract_all(d$Drafted.From, "\\[(.*?)\\]")
#d$Am_Lg <- sub(".*[\\[(]([^\\])]+)[\\])].*", "\\1", d$Drafted.From)
d <- d %>%
  mutate(
    Am_Lg = str_extract(Drafted.From, "(?<=\\[|\\().+?(?=\\]|\\))")
  )


table(d[which(d$in_set == 0),]$Am_Lg)

# tmp <- d[which(d$in_set == 0),]
# tmp <- tmp[which(tmp$Am_Lg == 'Finland'),]

#### The following lines of code were used to manually correct mismatches in how a player's name was listed in the two datasets
#### No, this was not fun to do
# for (i in 1:nrow(d)){
#   if (d$in_set[i] == 0){
#     prmt <- paste0("Row ", i, ", ", d$Year[i], ", ", d$Round[i], ".", d$Num.[i], ", ", d$Am_Lg[i], ", ", d$Player[i], ": ")
#     name <- readline(prompt = prmt)
#     if (name == 'q'){
#       break
#     } else if (name != 's'){
#       d$Player[i] <- name
#     }
#   }
# }
# 
# write.csv(d, file = "updated_draft.csv", row.names = FALSE)

prior$GP <- as.numeric(prior$GP)
prior$G <- as.numeric(prior$G)
prior$A <- as.numeric(prior$A)
prior$TP <- as.numeric(prior$TP)
prior$PPG <- as.numeric(prior$PPG)
prior$PIM <- as.numeric(prior$PIM)
prior$X... <- as.numeric(prior$X...)

table(prior$League)
mean(prior[which(prior$League == "USHL"),]$TP)
mean(prior[which(prior$League == "OHL"),]$TP) # Note the big difference


Lg.avg <- c(
  sum(prior[which(prior$League == "USHL"),]$TP) / sum(prior[which(prior$League == "USHL"),]$GP),
  sum(prior[which(prior$League == "OHL"),]$TP) / sum(prior[which(prior$League == "OHL"),]$GP),
  sum(prior[which(prior$League == "WHL"),]$TP) / sum(prior[which(prior$League == "WHL"),]$GP),
  sum(prior[which(prior$League == "QMJHL"),]$TP) / sum(prior[which(prior$League == "QMJHL"),]$GP),
  sum(prior[which(prior$League == "SHL"),]$TP) / sum(prior[which(prior$League == "SHL"),]$GP),
  sum(prior[which(prior$League == "j20-superelit"),]$TP) / sum(prior[which(prior$League == "j20-superelit"),]$GP)
)

# Manually fixing errors caused by multiple players having the same name
rownames(prior) <- NULL
tmp <- prior[which(prior$League == "NHL"),]
print(tmp)
prior[82,] <- list(233, "Erik Gustafsson", "Djugardens IF J20", 21, 3, 11, 14, 0.67, 14, 3, "j20-superelit", "2011-2012", "Erik Gustafsson")
prior[88,] <- list(60, "Erik Jinesjö Karlsson", "Frölunda HC J20", 47, 14, 19, 33, 0.70, 70, -4, "j20-superelit", "2011-2012", "Erik Karlsson")
prior[793,] <- list(405, "Josh Anderson", "Prince George Cougars", 39, 1, 5, 6, 0.15, 86, -4, "WHL", "2015-2016", "Josh Anderson")
prior[1044,] <- list(29, "Sebastian Aho", "Skellefteå AIK", 50, 10, 20, 30, 0.6, 10, -6, "SHL", "2016-2017", "Sebastian Aho")
prior[1206,] <- list(79, "Ryan O'Reilly", "Madison Capitols", 45, 21, 13, 34, 0.76, 8, -2, "USHL", "2017-2018", "Ryan O'Reilly")

tmp <- prior[which(prior$League == "ECHL"),]
print(tmp)
prior[646,] <- list(55, "Alexandre Carrier", "Gatineau Olympiques", 68, 12, 43, 55, 0.81, 64, 17, "QMJHL", "2014-2015", "Alexandre Carrier")

barplot(Lg.avg, 
        names.arg = c("USHL", "OHL", "WHL", "QMJHL", "SHL", "J20-Superelit"), 
        ylim = c(0, 1), 
        ylab = "Average Points per Game", 
        xlab = "League",
        main = "Players from different leagues can have wildly different scoring numbers."
        )

# Keep only leagues with at least 10 players
table(prior$League)
prev <- prior[which(prior$League == "AJHL" | 
                              prior$League == "BCHL" | 
                                      prior$League == "CZECH" | 
                                              prior$League == "j20-superelit" |
                                                      prior$League == "KHL" |
                                                              prior$League == "LIIGA" |
                                                                      prior$League == "mhl" |
                                                                              prior$League == "NCAA" |
                                                                                      prior$League == "NLA" |
                                                                                              prior$League == "OHL" |
                                                                                                      prior$League == "OJHL" |
                                                                                                              prior$League == "QMJHL" |
                                                                                                                      prior$League == "SHL" |
                                                                                                                              prior$League == "u20-sm-liiga" |
                                                                                                                                      prior$League == "USHL" |
                                                                                                                                              prior$League == "USHS-PREP" |
                                                                                                                                                      prior$League == "WHL"),]
table(prev$League)
# prev <- prior.pruned[which(!is.na(prior.pruned$X.)),]
rownames(prev) <- NULL

prev$nhl <- 0
prev$Pos <- ""
# prev$nhl[3] <- d[d$Player == prev$Player[3],]$nhl
for (i in 1:nrow(prev)){
  prev$nhl[i] <- d[d$Player == prev$playerNoDia[i],]$nhl
  prev$Pos[i] <- d[d$Player == prev$playerNoDia[i],]$Pos
}
prev <- rename(prev, PlusMinus = X...)

### Classification Tree ####
prev$nhl <- as.factor(prev$nhl)

set.seed(1)
myIndex <- createDataPartition(prev$nhl, p=0.7, list=FALSE)
trainSet <- prev[myIndex,]
validationSet <- prev[-myIndex,]

set.seed(1)
full_tree <- rpart(nhl ~ GP + G + A + TP + PPG + PIM + PlusMinus + League + Pos, 
                   data = trainSet, 
                   method = "class", 
                   cp = 0, 
                   minsplit = 2, 
                   minbucket = 1)
prp(full_tree, 
    type = 1, 
    extra = 1, 
    under = TRUE)

printcp(full_tree)

# Tree 11 appears to have the lowest total xerror, but tree 4 is also solid
tree.11 <- prune(full_tree, cp = 0.00413224)
prp(tree.11, 
    type = 1, 
    extra = 1, 
    under = TRUE)

# tree.9 <- prune(full_tree, cp = 0.00578513)
# prp(tree.9, 
#     type = 1, 
#     extra = 1, 
#     under = TRUE)

tree.4 <- prune(full_tree, cp = 0.01157026)
prp(tree.4, 
    type = 1, 
    extra = 1, 
    under = TRUE)

length(which(prev$nhl=="1")) / nrow(prev) 
# Just less than half of all draftees make the NHL
# Close enough to 50% to leave it

# Evaluate both models
pred.11 <- predict(tree.11, validationSet, type = "class")
confusionMatrix(pred.11, validationSet$nhl, positive = "1")

pred.4 <- predict(tree.4, validationSet, type = "class")
confusionMatrix(pred.4, validationSet$nhl, positive = "1")

# Tree 11
predicted_prob <- predict(tree.11, validationSet, type= 'prob')
head(predicted_prob)

validationSet$nhl <- as.numeric(as.character(validationSet$nhl))
gains_table <- gains(validationSet$nhl, predicted_prob[,2])
gains_table

plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$nhl)) ~ c(0, gains_table$cume.obs), 
     xlab = '# of cases', 
     ylab = "Cumulative", 
     type = "l")
lines(c(0, sum(validationSet$nhl))~c(0, dim(validationSet)[1]), 
      col="red", 
      lty=2)

barplot(gains_table$mean.resp/mean(validationSet$nhl), 
        names.arg=gains_table$depth, 
        xlab="Percentile", 
        ylab="Lift", 
        ylim=c(0, 3.0), 
        main="Decile-Wise Lift Chart")

roc_object <- roc(validationSet$nhl, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object)

# The following tree was made obsolete after additional data management had been completed. 

# Tree 9
# predicted_prob <- predict(tree.9, validationSet, type= 'prob')
# head(predicted_prob)
# 
# validationSet$nhl <- as.numeric(as.character(validationSet$nhl))
# gains_table <- gains(validationSet$nhl, predicted_prob[,2])
# gains_table
# 
# plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$nhl)) ~ c(0, gains_table$cume.obs), 
#      xlab = '# of cases', 
#      ylab = "Cumulative", 
#      type = "l")
# lines(c(0, sum(validationSet$nhl))~c(0, dim(validationSet)[1]), 
#       col="red", 
#       lty=2)
# 
# barplot(gains_table$mean.resp/mean(validationSet$nhl), 
#         names.arg=gains_table$depth, 
#         xlab="Percentile", 
#         ylab="Lift", 
#         ylim=c(0, 3.0), 
#         main="Decile-Wise Lift Chart")
# 
# roc_object <- roc(validationSet$nhl, predicted_prob[,2])
# plot.roc(roc_object)
# auc(roc_object)


# Tree 4
predicted_prob <- predict(tree.4, validationSet, type= 'prob')
head(predicted_prob)

validationSet$nhl <- as.numeric(as.character(validationSet$nhl))
gains_table <- gains(validationSet$nhl, predicted_prob[,2])
gains_table

plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$nhl)) ~ c(0, gains_table$cume.obs), 
     xlab = '# of cases', 
     ylab = "Cumulative", 
     type = "l")
lines(c(0, sum(validationSet$nhl))~c(0, dim(validationSet)[1]), 
      col="red", 
      lty=2)

barplot(gains_table$mean.resp/mean(validationSet$nhl), 
        names.arg=gains_table$depth, 
        xlab="Percentile", 
        ylab="Lift", 
        ylim=c(0, 2.0), 
        main="Decile-Wise Lift Chart")
abline(h=1.0, col="red", lty=2)

roc_object <- roc(validationSet$nhl, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object)

# Tree 4 appears better, but it's not earth shattering

# Analysis of league scoring
for(lg in unique(prev$League)){
  print(paste(lg, sum(prev[which(prev$League == lg),]$TP) / sum(prev[which(prev$League == lg),]$GP)))
}


#### Individualized Naive Bayes Models ####

# Get league specific datasets
ushl <- prev[which(prev$League == "USHL"),]
ohl <- prev[which(prev$League == "OHL"),]
whl <- prev[which(prev$League == "WHL"),]
qmjhl <- prev[which(prev$League == "QMJHL"),]
shl <- prev[which(prev$League == "SHL"),]
j20 <- prev[which(prev$League == "j20-superelit"),]
liiga <- prev[which(prev$League == "LIIGA"),]
khl <- prev[which(prev$League == "KHL"),]

# Seperate datasets into needed x and y components
ushl <- ushl[,c(4, 5, 6, 7, 8, 9, 10, 14)]
ushl <- na.omit(ushl)
y.ushl <- as.factor(ushl$nhl)
x.ushl <- ushl[,-8]

# Correlation matrix plot
cor.ushl <- round(cor(x.ushl),2)
cor.ushl[upper.tri(cor.ushl)] <- NA
cor.ushl.melt <- melt(cor.ushl, na.rm = TRUE)

ggplot(data = cor.ushl.melt, aes(x=Var2, y=Var1, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed() +
  ggtitle("Severe multicollinearity in the USHL dataset")
# corrplot(cor.ushl, type = "lower", order = "hclust",
#          tl.col = "black", tl.srt = 45)


ohl <- ohl[,c(4, 5, 6, 7, 8, 9, 10, 14)]
ohl <- na.omit(ohl)
y.ohl <- as.factor(ohl$nhl)
x.ohl <- ohl[,-8]

whl <- whl[,c(4, 5, 6, 7, 8, 9, 10, 14)]
whl <- na.omit(whl)
y.whl <- as.factor(whl$nhl)
x.whl <- whl[,-8]

qmjhl <- qmjhl[,c(4, 5, 6, 7, 8, 9, 10, 14)]
qmjhl <- na.omit(qmjhl)
y.qmjhl <- as.factor(qmjhl$nhl)
x.qmjhl <- qmjhl[,-8]

shl <- shl[,c(4, 5, 6, 7, 8, 9, 10, 14)]
shl <- na.omit(shl)
y.shl <- as.factor(shl$nhl)
x.shl <- shl[,-8]

j20 <- j20[,c(4, 5, 6, 7, 8, 9, 10, 14)]
j20 <- na.omit(j20)
y.j20 <- as.factor(j20$nhl)
x.j20 <- j20[,-8]

liiga <- liiga[,c(4, 5, 6, 7, 8, 9, 10, 14)]
liiga <- na.omit(liiga)
y.liiga <- as.factor(liiga$nhl)
x.liiga <- liiga[,-8]

khl <- khl[,c(4, 5, 6, 7, 8, 9, 10, 14)]
khl <- na.omit(khl)
y.khl <- as.factor(khl$nhl)
x.khl <- khl[,-8]

# Setup k-fold cross validation
myControl <- trainControl(
  method = "cv",
  number = 20,
  savePredictions = "final"
)

# Train models using PCA into NB
nb.ushl <- train(
  x = x.ushl,
  y = y.ushl,
  method = "nb",
  preProcess = c("center", "scale", "pca"),
  thresh = 0.99,
  trControl = myControl
)
nb.ushl
confusionMatrix(nb.ushl$pred$pred, nb.ushl$pred$obs)

nb.ohl <- train(
  x = x.ohl,
  y = y.ohl,
  method = "nb",
  preProcess = c("center", "scale", "pca"),
  thresh = 0.99,
  trControl = myControl
)
nb.ohl
confusionMatrix(nb.ohl$pred$pred, nb.ohl$pred$obs)

nb.whl <- train(
  x = x.whl,
  y = y.whl,
  method = "nb",
  preProcess = c("center", "scale", "pca"),
  thresh = 0.99,
  trControl = myControl
)
nb.whl
confusionMatrix(nb.whl$pred$pred, nb.whl$pred$obs)

nb.qmjhl <- train(
  x = x.qmjhl,
  y = y.qmjhl,
  method = "nb",
  preProcess = c("center", "scale", "pca"),
  thresh = 0.99,
  trControl = myControl
)
nb.qmjhl
confusionMatrix(nb.qmjhl$pred$pred, nb.qmjhl$pred$obs)

nb.shl <- train(
  x = x.shl,
  y = y.shl,
  method = "nb",
  preProcess = c("center", "scale", "pca"),
  thresh = 0.99,
  trControl = myControl
)
nb.shl
confusionMatrix(nb.shl$pred$pred, nb.shl$pred$obs)

nb.j20 <- train(
  x = x.j20,
  y = y.j20,
  method = "nb",
  preProcess = c("center", "scale", "pca"),
  thresh = 0.99,
  trControl = myControl
)
nb.j20
confusionMatrix(nb.j20$pred$pred, nb.j20$pred$obs)

nb.liiga <- train(
  x = x.liiga,
  y = y.liiga,
  method = "nb",
  preProcess = c("center", "scale", "pca"),
  thresh = 0.99,
  trControl = myControl
)
nb.liiga
confusionMatrix(nb.liiga$pred$pred, nb.liiga$pred$obs)

nb.khl <- train(
  x = x.khl,
  y = y.khl,
  method = "nb",
  preProcess = c("center", "scale", "pca"),
  thresh = 0.99,
  trControl = myControl
)
nb.khl
confusionMatrix(nb.khl$pred$pred, nb.khl$pred$obs)


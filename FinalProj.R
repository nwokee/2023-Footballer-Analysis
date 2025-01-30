library(ggplot2)
library(reshape2)
library(corrplot)
library(dplyr)
PlayerDF <- read.csv("2022-23 PData.csv")

#------------------------------------------------------------------------------#
#-------------------------- PART 0: DATA ADJUSTMENTS --------------------------#

# adjust tackles attempted to tackle success rate
PlayerDF$TacklesAttempted <- round((PlayerDF$TacklesWon / PlayerDF$TacklesAttempted) * 100,2)
colnames(PlayerDF)[colnames(PlayerDF) == "TacklesAttempted"] <- "TackleSuccRate"
PlayerDF$TackleSuccRate[is.nan(PlayerDF$TackleSuccRate)] <- 0

# turn Assists into a counting stat
PlayerDF$Assists <- round(PlayerDF$Assists*PlayerDF$Total90s)

#------------------------------------------------------------------------------#
#-------------------------- PART 1: LEAGUE AVERAGES ---------------------------#
Prem <- PlayerDF[which(PlayerDF$League == "Premier League"), ]
Liga <- PlayerDF[which(PlayerDF$League == "La Liga"), ]
Buli <- PlayerDF[which(PlayerDF$League == "Bundesliga"), ]
Lig1 <- PlayerDF[which(PlayerDF$League == "Ligue 1"), ]
SerA <- PlayerDF[which(PlayerDF$League == "Serie A"), ]

LeaguesComp <- data.frame(c("Premier League", "La Liga", 
                            "Bundesliga", "Ligue 1", "Serie A"))
colnames(LeaguesComp) <- "League"

## compare amount of players who made the minutes restriction
LeaguesComp$NumPlayers <- c(length(Prem$Player), length(Liga$Player), 
                            length(Buli$Player), length(Lig1$Player), 
                            length(SerA$Player))

## Average Age
LeaguesComp$AVGAge <- c(mean(Prem$Age), mean(Liga$Age), mean(Buli$Age), 
                        mean(Lig1$Age), mean(SerA$Age))

## compare average minutes and stdev of minutes
LeaguesComp$AVGMinutes <- c(mean(Prem$Minutes), mean(Liga$Minutes), 
                            mean(Buli$Minutes), mean(Lig1$Minutes), 
                            mean(SerA$Minutes))
LeaguesComp$STDVMinutes <- c(sd(Prem$Minutes), sd(Liga$Minutes), 
                            sd(Buli$Minutes), sd(Lig1$Minutes), 
                            sd(SerA$Minutes))

## compare total goals
LeaguesComp$Goals <- c(sum(Prem$Goals), sum(Liga$Goals), 
                       sum(Buli$Goals), sum(Lig1$Goals), 
                       sum(SerA$Goals))

## compare total solo goals (goals that weren't assisted)
LeaguesComp$SoloGoals <- c(sum(Prem$Goals) - sum(Prem$Assists),
                           sum(Liga$Goals) - sum(Liga$Assists),
                           sum(Buli$Goals) - sum(Buli$Assists),
                           sum(Lig1$Goals) - sum(Lig1$Assists),
                           sum(SerA$Goals) - sum(SerA$Assists))

## compare average fouls
LeaguesComp$AVGFouls <- c(mean(Prem$Fouls), mean(Liga$Fouls), 
                            mean(Buli$Fouls), mean(Lig1$Fouls), 
                            mean(SerA$Fouls))

#------------------------------------------------------------------------------#
#----------------------- PART 2: TOP GOAL CONTRIBUTORS ------------------------#

# add column for Goal Contributions (Goals + Assists)
PlayerDF$TotalGA<- PlayerDF$Goals + PlayerDF$Assists


## Top 15 Players by Goal Contributions
Top15GA <- PlayerDF[order(-PlayerDF$TotalGA), ][1:15, ]
Top15GALong <- melt(Top15GA, 
                         id.vars = "Player", 
                         measure.vars = c("Goals", "Assists"), 
                         variable.name = "ContributionType", 
                         value.name = "Value")

ggplot(Top15GALong, aes(x = reorder(Player, -Value), y = Value, 
                        fill = ContributionType)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 15 Players by Total Goal Contributions",
    x = "Player",
    y = "Total Contributions"
  ) +
  scale_fill_manual(values = c("Goals" = "blue", "Assists" = "red")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#------------------------------------------------------------------------------#
#----------------------- PART 3: PASSING DISTRIBUTIONS ------------------------#

# Create a histogram of Completed Passes per 90
ggplot(PlayerDF, aes(x = PassesCompleted)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Completed Passes per 90",
    x = "Passes p90",
    y = "Frequency"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------#
#------------------ PART 4: GOAL COMP BETWEEN TWO CHAMPIONS -------------------#

# pie chart comparison of goal distribution between Manchester City and Bayern
colors <- c("red", "blue", "green", "orange", "purple", "pink", 
            "cyan", "yellow", "brown", "gray", "turquoise", "gold")

# Man City Pie Chart
ManCityPlayers <- PlayerDF[which(PlayerDF$Team == "Manchester City" 
                                 & PlayerDF$Goals > 0), ]
ManCityPlayers$Player <- factor(ManCityPlayers$Player, 
                                    levels = 
                                  ManCityPlayers$Player[order(-ManCityPlayers$Goals)])

ManCityPlayers <- ManCityPlayers[order(ManCityPlayers$Goals), ]
ManCityPlayers$Percentage <- ManCityPlayers$Goals / sum(ManCityPlayers$Goals) * 100
ManCityPlayers$CumulativePercentage <- cumsum(ManCityPlayers$Percentage)
ManCityPlayers$Midpoint <- ManCityPlayers$CumulativePercentage - ManCityPlayers$Percentage / 2

ggplot(ManCityPlayers, aes(x = "", y = Percentage, fill = Player)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(
    aes(
      label = paste0(round(Percentage, 1), "%"),
      x = 1.25,
      y = Midpoint
    ),
    size = 4.5,
    color = "black"
  ) +
  labs(
    title = "Goal Distribution Among Manchester City FC",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = colors)


# Bayern Munich Pie Chart
BayernPlayers <- PlayerDF[which(PlayerDF$Team == "Bayern Munich" 
                                 & PlayerDF$Goals > 0), ]
BayernPlayers$Player <- factor(BayernPlayers$Player, 
                                levels = 
                                 BayernPlayers$Player[order(-BayernPlayers$Goals)])

BayernPlayers <- BayernPlayers[order(BayernPlayers$Goals), ]
BayernPlayers$Percentage <- BayernPlayers$Goals / sum(BayernPlayers$Goals) * 100
BayernPlayers$CumulativePercentage <- cumsum(BayernPlayers$Percentage)
BayernPlayers$Midpoint <- BayernPlayers$CumulativePercentage - BayernPlayers$Percentage / 2

ggplot(BayernPlayers, aes(x = "", y = Percentage, fill = Player)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(
    aes(
      label = paste0(round(Percentage, 1), "%"),
      x = 1.25,
      y = Midpoint
    ),
    size = 3.5,
    color = "black"
  ) +
  labs(
    title = "Goal Distribution Among FC Bayern Munchen",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = colors)


#------------------------------------------------------------------------------#
#------------------------- PART 5: TOUCH DISTRIBUTION -------------------------#

## filtering players into different df's based on position (allowing for dupes)
forwards <- PlayerDF[which(grepl("FW", PlayerDF$Position)), ]
midfielders <- PlayerDF[which(grepl("MF", PlayerDF$Position)), ]
defenders <- PlayerDF[which(grepl("DF", PlayerDF$Position)), ]
goalies <- PlayerDF[which(grepl("GK", PlayerDF$Position)), ]

# rebind the data
forwards$PositionCategory <- "Attackers"
midfielders$PositionCategory <- "Midfielders"
defenders$PositionCategory <- "Defenders"
goalies$PositionCategory <- "Goalies"
all_players <- rbind(forwards, midfielders, defenders, goalies)
all_players$PositionCategory <- factor(all_players$PositionCategory, 
                                       levels = c("Attackers", "Midfielders", 
                                                  "Defenders", "Goalies"))
# Create the stacked boxplot
ggplot(all_players, aes(x = PositionCategory, y = Touches, 
                        fill = PositionCategory)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Touches by Player Position",
    x = "Position",
    y = "Touches"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  

#------------------------------------------------------------------------------#
#------------------- PART 6: THE MOST DANGEROUS DRIBBLERS ---------------------#

## scatterplot for takeons and and success rate (min 2.5 takeons per 90)
takeonDF <- PlayerDF[which(PlayerDF$TakeOnsAttempted > 2.5), ]

# force players into 1 of 3 position classes
for (i in 1:length(takeonDF$Player)) {
  temp <- substring(takeonDF$Position[i],1,2)
  
  if(identical(temp,'FW')) {takeonDF$Class[i] <- 'Attacker'}
  else if(identical(temp,'MF')) {takeonDF$Class[i] <- 'Midfielder'}
  else if(identical(temp,'DF')) {takeonDF$Class[i] <- 'Defender'}
}

ggplot(takeonDF, aes(TakeOnsAttempted, TakeOnSuccessRate, colour = Class)) +
  geom_point(size = 6) +
  xlab("Number of Take Ons Attempted") + # set axis
  ylab("Take On Success Rate") + # set axis
  labs(color = "League") + # set legend
  ggtitle("Take On Success Across Europe (Min 2.5 per 90)") + # set title
  scale_fill_manual(values = colors) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# multiply success rate by completions to the most dangerous dribblers in europe
takeonDF$takeoncoeff <- takeonDF$SuccessfulTakeOns * takeonDF$TakeOnSuccessRate
takeonDF$takeoncoeff <- takeonDF$takeoncoeff / 100
# Get the top 10 players by takeoncoeff
top_dribblers <- takeonDF[order(-takeonDF$takeoncoeff), ][1:10, ]

# Create the bar plot with colors
ggplot(top_dribblers, aes(x = reorder(Player, -takeoncoeff), y = takeoncoeff, 
                          fill = takeoncoeff)) +
  geom_bar(stat = "identity") +  
  xlab("Player Name") + 
  ylab("Take-On Coefficient") + 
  ggtitle("Top 10 Most Dangerous Dribblers in Europe") +
  theme_minimal() +
  scale_fill_gradient(low = "#FF9999", high = "#FF0000") +
  geom_text(aes(label = round(takeoncoeff, 2)), vjust = -0.5, 
            color = "black", size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  axis.text.y = element_blank(),
  legend.position = "none") 


#------------------------------------------------------------------------------#
#--------------------- PART 7: ERLING HAALAND PREDICTION ----------------------#

gameweeks <- 1:21
haalandGA <- c(2,3,4,7,10,11,12,17,18,18,20,20,21,21,23,24,24,24,25,28,28)
haaland <- data.frame(gameweeks,haalandGA)

ggplot(haaland, aes(x = gameweeks, y = haalandGA)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_text(aes(label = haaland$haalandGA), vjust = -1.5, 
            color = "black", size = 3) +
  labs(title = "Erling Haaland's Goal Contribution Total Over The Season", 
       x = "Gameweek", 
       y = "Goals and Assists (G/A)") +
  theme_minimal()

haalandmodel <- lm(haalandGA ~ gameweeks,haaland)
summary(haalandmodel)

## predict haaland's end of season tally and add it to df for plotting
eosPredict <- predict(haalandmodel, newdata = data.frame(gameweeks = 38))
haaland[22, ] <- c(38, eosPredict)
eosPredict <- round(eosPredict)
eosPredict


ggplot(haaland, aes(x = gameweeks, y = haalandGA)) +
  geom_point(color = "blue") +  # Points for the original data
  geom_smooth(method = "lm", color = "red") +  # Linear regression line
  labs(title = "Erling Haaland's Goal Contribution Total Over The Season", 
       x = "Gameweek", 
       y = "Goals and Assists (G/A)") +
  xlim(1, 38) +  # Extend x-axis to 38
  ylim(0, 60) +
  theme_minimal()


# haaland finished on 44 GA, plotting the rest of his season
gameweeks <- 1:38
haalandGA <- c(2,3,4,7,10,11,12,17,18,18,20,20,21,21,23,24,24,24,25,28,28,
               29,30,30,31,32,33,33,35,37,40,41,42,42,43,43,44,44)
haaland <- data.frame(gameweeks,haalandGA)

ggplot(haaland, aes(x = gameweeks, y = haalandGA)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_abline(slope = haalandmodel$coefficients[2], 
              intercept = haalandmodel$coefficients[1], 
              color = "red", linetype = "dashed", size = 1) +
  labs(title = "Erling Haaland's Goal Contribution Total Over The Season", 
       x = "Gameweek", 
       y = "Goals and Assists (G/A)") +
  ylim(1, 60) +
  theme_minimal()


#------------------------------------------------------------------------------#
#------------------- PART 8: TEAMS OF THE SEASON BY LEAGUE --------------------#

#first make a correlation matrix showing which stats relate to others
cor_matrix <- cor(PlayerDF[, 10:74])
corrplot(cor_matrix, method = "circle", tl.cex = 0.55)

# filter out all players from the premier league
# using the FW, MF, DF, GK separations from earlier
forwards <- PlayerDF[which(grepl("FW", PlayerDF$Position)), ]
midfielders <- PlayerDF[which(grepl("MF", PlayerDF$Position)), ]
defenders <- PlayerDF[which(grepl("DF", PlayerDF$Position)), ]
goalies <- PlayerDF[which(grepl("GK", PlayerDF$Position)), ]

# from those 4 dfs, do just the Premier League
MFPrem <- midfielders[which(midfielders$League == "Premier League"), ]
DFPrem <- defenders[which(defenders$League == "Premier League"), ]
GKPrem <- goalies[which(goalies$League == "Premier League"), ]

# calculate the mean of all stats and adjust df to show
# every player's deviation from the mean

######### Forward Scale #########
## 1.00 -- Goals, Goals Per Shot
## 0.85 -- Shots, Shots On Target
## 0.70 -- Assists
## 0.60 -- Shot Assists
## 0.55 -- Touches In Penalty Area, Goal Creating Actions
## 0.50 -- Shot Creating Actions
## 0.45 -- Offsides(penalize), Successful Take Ons, Take On Success Rate
## 0.40 -- Passes Into The Penalty Area, Touches In Attacking Third
## 0.35 -- Penalty Box Entries
## 0.30 -- Average Shot Distance
## 0.20 -- Fouls Drawn, Fouls Committed(penalize)
## Multiply final score by total 90s to give credit to players who play more


## Premier League Forwards
FWPrem <- forwards[which(forwards$League == "Premier League"), ]
FWPremDevs <- FWPrem

for (i in 1:length(FWPrem$Player)) {
  for (j in 11:74) {
    FWPremDevs[i,j] <- (FWPrem[i,j] - mean(FWPrem[,j])) /  sd(FWPrem[,j])
  }
}

FWPremRanks <- c(FWPremDevs$Goals + FWPremDevs$GoalsPerShot +
                   FWPremDevs$Shots * 0.85 + FWPremDevs$ShotsOnTarget * 0.85 +
                   FWPremDevs$Assists * 0.7 + FWPremDevs$ShotAssists * 0.6 +
                   FWPremDevs$TouchesInPenaltyArea * 0.55 +  
                   FWPremDevs$GoalCreatingActions * 0.55 +
                   FWPremDevs$ShotCreatingActions * 0.5 -
                   FWPremDevs$Offsides * 0.45 + 
                   FWPremDevs$SuccessfulTakeOns * 0.45 + 
                   FWPremDevs$TakeOnSuccessRate * 0.45 + 
                   FWPremDevs$PassesIntoThePenaltyArea * 0.4 +
                   FWPremDevs$TouchesInAttackingThird * 0.4 +
                   FWPremDevs$PenaltyBoxEntries * 0.35 +
                   FWPremDevs$AverageShotDistance * 0.3 +
                   FWPremDevs$FoulsDrawn * 0.2 - FWPremDevs$Fouls * 0.2)
FWPremRanks <- FWPremRanks * FWPremDevs$Total90s

FWPremRanks <- data.frame(FWPremDevs[,1:6],FWPremRanks)


## La Liga Forwards
FWLiga <- forwards[which(forwards$League == "La Liga"), ]
FWLigaDevs <- FWLiga

for (i in 1:length(FWLiga$Player)) {
  for (j in 11:74) {
    FWLigaDevs[i,j] <- (FWLiga[i,j] - mean(FWLiga[,j])) /  sd(FWLiga[,j])
  }
}

FWLigaRanks <- c(FWLigaDevs$Goals + FWLigaDevs$GoalsPerShot +
                   FWLigaDevs$Shots * 0.85 + FWLigaDevs$ShotsOnTarget * 0.85 +
                   FWLigaDevs$Assists * 0.7 + FWLigaDevs$ShotAssists * 0.6 +
                   FWLigaDevs$TouchesInPenaltyArea * 0.55 +  
                   FWLigaDevs$GoalCreatingActions * 0.55 +
                   FWLigaDevs$ShotCreatingActions * 0.5 -
                   FWLigaDevs$Offsides * 0.45 + 
                   FWLigaDevs$SuccessfulTakeOns * 0.45 + 
                   FWLigaDevs$TakeOnSuccessRate * 0.45 + 
                   FWLigaDevs$PassesIntoThePenaltyArea * 0.4 +
                   FWLigaDevs$TouchesInAttackingThird * 0.4 +
                   FWLigaDevs$PenaltyBoxEntries * 0.35 +
                   FWLigaDevs$AverageShotDistance * 0.3 +
                   FWLigaDevs$FoulsDrawn * 0.2 - FWLigaDevs$Fouls * 0.2)
FWLigaRanks <- FWLigaRanks * FWLigaDevs$Total90s

FWLigaRanks <- data.frame(FWLigaDevs[,1:6],FWLigaRanks)


# Bundesliga Forwards
FWBund <- forwards[which(forwards$League == "Bundesliga"), ]
FWBundDevs <- FWBund

for (i in 1:length(FWBund$Player)) {
  for (j in 11:74) {
    FWBundDevs[i,j] <- (FWBund[i,j] - mean(FWBund[,j])) /  sd(FWBund[,j])
  }
}

FWBundRanks <- c(FWBundDevs$Goals + FWBundDevs$GoalsPerShot +
                   FWBundDevs$Shots * 0.85 + FWBundDevs$ShotsOnTarget * 0.85 +
                   FWBundDevs$Assists * 0.7 + FWBundDevs$ShotAssists * 0.6 +
                   FWBundDevs$TouchesInPenaltyArea * 0.55 +  
                   FWBundDevs$GoalCreatingActions * 0.55 +
                   FWBundDevs$ShotCreatingActions * 0.5 -
                   FWBundDevs$Offsides * 0.45 + 
                   FWBundDevs$SuccessfulTakeOns * 0.45 + 
                   FWBundDevs$TakeOnSuccessRate * 0.45 + 
                   FWBundDevs$PassesIntoThePenaltyArea * 0.4 +
                   FWBundDevs$TouchesInAttackingThird * 0.4 +
                   FWBundDevs$PenaltyBoxEntries * 0.35 +
                   FWBundDevs$AverageShotDistance * 0.3 +
                   FWBundDevs$FoulsDrawn * 0.2 - FWBundDevs$Fouls * 0.2)
FWBundRanks <- FWBundRanks * FWBundDevs$Total90s

FWBundRanks <- data.frame(FWBundDevs[,1:6],FWBundRanks)


# Ligue 1 Forwards
FWLig1 <- forwards[which(forwards$League == "Ligue 1"), ]
FWLig1Devs <- FWLig1

for (i in 1:length(FWLig1$Player)) {
  for (j in 11:74) {
    FWLig1Devs[i,j] <- (FWLig1[i,j] - mean(FWLig1[,j])) /  sd(FWLig1[,j])
  }
}

FWLig1Ranks <- c(FWLig1Devs$Goals + FWLig1Devs$GoalsPerShot +
                   FWLig1Devs$Shots * 0.85 + FWLig1Devs$ShotsOnTarget * 0.85 +
                   FWLig1Devs$Assists * 0.7 + FWLig1Devs$ShotAssists * 0.6 +
                   FWLig1Devs$TouchesInPenaltyArea * 0.55 +  
                   FWLig1Devs$GoalCreatingActions * 0.55 +
                   FWLig1Devs$ShotCreatingActions * 0.5 -
                   FWLig1Devs$Offsides * 0.45 + 
                   FWLig1Devs$SuccessfulTakeOns * 0.45 + 
                   FWLig1Devs$TakeOnSuccessRate * 0.45 + 
                   FWLig1Devs$PassesIntoThePenaltyArea * 0.4 +
                   FWLig1Devs$TouchesInAttackingThird * 0.4 +
                   FWLig1Devs$PenaltyBoxEntries * 0.35 +
                   FWLig1Devs$AverageShotDistance * 0.3 +
                   FWLig1Devs$FoulsDrawn * 0.2 - FWLig1Devs$Fouls * 0.2)
FWLig1Ranks <- FWLig1Ranks * FWLig1Devs$Total90s

FWLig1Ranks <- data.frame(FWLig1Devs[,1:6],FWLig1Ranks)

# Serie A Forwards
FWSerA <- forwards[which(forwards$League == "Serie A"), ]
FWSerADevs <- FWSerA

for (i in 1:length(FWSerA$Player)) {
  for (j in 11:74) {
    FWSerADevs[i,j] <- (FWSerA[i,j] - mean(FWSerA[,j])) /  sd(FWSerA[,j])
  }
}

FWSerARanks <- c(FWSerADevs$Goals + FWSerADevs$GoalsPerShot +
                   FWSerADevs$Shots * 0.85 + FWSerADevs$ShotsOnTarget * 0.85 +
                   FWSerADevs$Assists * 0.7 + FWSerADevs$ShotAssists * 0.6 +
                   FWSerADevs$TouchesInPenaltyArea * 0.55 +  
                   FWSerADevs$GoalCreatingActions * 0.55 +
                   FWSerADevs$ShotCreatingActions * 0.5 -
                   FWSerADevs$Offsides * 0.45 + 
                   FWSerADevs$SuccessfulTakeOns * 0.45 + 
                   FWSerADevs$TakeOnSuccessRate * 0.45 + 
                   FWSerADevs$PassesIntoThePenaltyArea * 0.4 +
                   FWSerADevs$TouchesInAttackingThird * 0.4 +
                   FWSerADevs$PenaltyBoxEntries * 0.35 +
                   FWSerADevs$AverageShotDistance * 0.3 +
                   FWSerADevs$FoulsDrawn * 0.2 - FWSerADevs$Fouls * 0.2)
FWSerARanks <- FWSerARanks * FWSerADevs$Total90s

FWSerARanks <- data.frame(FWSerADevs[,1:6],FWSerARanks)





######### Midfielder Scale #########
## 1.00 -- Assists, Passes Completed
## 0.85 -- Shot Assists, Pass Completion PCt
## 0.70 -- Total prog Pass Distance, Carries, Passes Leading to shots 
## 0.60 -- Goals, Goals per Shot, crosses
## 0.55 -- Recoveries, progressive Passes
## 0.50 -- Final Third Passes, Passes into Penalty AREA 
## 0.45 -- Through Balls, Switches, shot creating Actions, Carry Dist
## 0.40 -- Touches, Aerial Duels won, Aerial duel win Rate
## 0.35 -- Fouls Drawn, Fouls Committed (penalize)
## 0.30 -- Take ons Completed, Tackles, Amt Dispossessed (penalize)
## 0.20 -- Interceptions, Passes Blocked
## Multiply final score by total 90s to give credit to players who play more


## Premier League Midfielders
MFPrem <- midfielders[which(midfielders$League == "Bundesliga"), ]
MFPremDevs <- MFPrem

for (i in 1:length(MFPrem$Player)) {
  for (j in 11:74) {
    MFPremDevs[i,j] <- (MFPrem[i,j] - mean(MFPrem[,j])) /  sd(MFPrem[,j])
  }
}

MFPremRanks <- c(MFPremDevs$Assists + MFPremDevs$PassesCompleted +
                   MFPremDevs$ShotAssists * 0.85 + 
                   MFPremDevs$PassCompletionPct * 0.85 +
                   MFPremDevs$TotalProgressivePassDist * 0.7 + 
                   MFPremDevs$Carries * 0.7 +
                   MFPremDevs$PassesThatLedToShots * 0.7 +
                   MFPremDevs$Goals * 0.6 + MFPremDevs$GoalsPerShot * 0.6 +
                   MFPremDevs$Crosses * 0.6 + MFPremDevs$Recoveries * 0.55 +  
                   MFPremDevs$ProgressivePasses * 0.55 + 
                   MFPremDevs$PassesIntoThePenaltyArea * 0.55 +
                   MFPremDevs$FinalThirdPasses * 0.5 +
                   MFPremDevs$ThroughBalls * 0.45 + 
                   MFPremDevs$Switches * 0.45 +
                   MFPremDevs$ShotCreatingActions * 0.45 +
                   MFPremDevs$Touches * 0.4 + MFPremDevs$AerialDuelsWon * 0.4 +
                   MFPremDevs$AerialDuelWinRate * 0.4 +
                   MFPremDevs$FoulsDrawn * 0.35 - MFPremDevs$Fouls * 0.35 +
                   MFPremDevs$CrossesIntoThePenaltyArea * 0.3 + 
                   MFPremDevs$SuccessfulTakeOns * 0.3 +
                   MFPremDevs$TakeOnSuccessRate * 0.3 +
                   MFPremDevs$TacklesWon * 0.3 -
                   MFPremDevs$AmtDispossessed * 0.3 +
                   MFPremDevs$Interceptions * 0.2 +
                   MFPremDevs$PassesBlocked * 0.2)

MFPremRanks <- MFPremRanks * MFPremDevs$Total90s

MFPremRanks <- data.frame(MFPremDevs[,1:6],MFPremRanks)

## La Liga Midfielders
MFLiga <- midfielders[which(midfielders$League == "La Liga"), ]
MFLigaDevs <- MFLiga

for (i in 1:length(MFLiga$Player)) {
  for (j in 11:74) {
    MFLigaDevs[i,j] <- (MFLiga[i,j] - mean(MFLiga[,j])) /  sd(MFLiga[,j])
  }
}

MFLigaRanks <- c(MFLigaDevs$Assists + MFLigaDevs$PassesCompleted +
                   MFLigaDevs$ShotAssists * 0.85 + 
                   MFLigaDevs$PassCompletionPct * 0.85 +
                   MFLigaDevs$TotalProgressivePassDist * 0.7 + 
                   MFLigaDevs$Carries * 0.7 +
                   MFLigaDevs$PassesThatLedToShots * 0.7 +
                   MFLigaDevs$Goals * 0.6 + MFLigaDevs$GoalsPerShot * 0.6 +
                   MFLigaDevs$Crosses * 0.6 + MFLigaDevs$Recoveries * 0.55 +  
                   MFLigaDevs$ProgressivePasses * 0.55 + 
                   MFLigaDevs$PassesIntoThePenaltyArea * 0.55 +
                   MFLigaDevs$FinalThirdPasses * 0.5 +
                   MFLigaDevs$ThroughBalls * 0.45 + 
                   MFLigaDevs$Switches * 0.45 +
                   MFLigaDevs$ShotCreatingActions * 0.45 +
                   MFLigaDevs$Touches * 0.4 + MFLigaDevs$AerialDuelsWon * 0.4 +
                   MFLigaDevs$AerialDuelWinRate * 0.4 +
                   MFLigaDevs$FoulsDrawn * 0.35 - MFLigaDevs$Fouls * 0.35 +
                   MFLigaDevs$CrossesIntoThePenaltyArea * 0.3 + 
                   MFLigaDevs$SuccessfulTakeOns * 0.3 +
                   MFLigaDevs$TakeOnSuccessRate * 0.3 +
                   MFLigaDevs$TacklesWon * 0.3 -
                   MFLigaDevs$AmtDispossessed * 0.3 +
                   MFLigaDevs$Interceptions * 0.2 +
                   MFLigaDevs$PassesBlocked * 0.2)

MFLigaRanks <- MFLigaRanks * MFLigaDevs$Total90s

MFLigaRanks <- data.frame(MFLigaDevs[,1:6],MFLigaRanks)

## Bundesliga Midfielders
MFBund <- midfielders[which(midfielders$League == "Bundesliga"), ]
MFBundDevs <- MFBund

for (i in 1:length(MFBund$Player)) {
  for (j in 11:74) {
    MFBundDevs[i,j] <- (MFBund[i,j] - mean(MFBund[,j])) /  sd(MFBund[,j])
  }
}

MFBundRanks <- c(MFBundDevs$Assists + MFBundDevs$PassesCompleted +
                   MFBundDevs$ShotAssists * 0.85 + 
                   MFBundDevs$PassCompletionPct * 0.85 +
                   MFBundDevs$TotalProgressivePassDist * 0.7 + 
                   MFBundDevs$Carries * 0.7 +
                   MFBundDevs$PassesThatLedToShots * 0.7 +
                   MFBundDevs$Goals * 0.6 + MFBundDevs$GoalsPerShot * 0.6 +
                   MFBundDevs$Crosses * 0.6 + MFBundDevs$Recoveries * 0.55 +  
                   MFBundDevs$ProgressivePasses * 0.55 + 
                   MFBundDevs$PassesIntoThePenaltyArea * 0.55 +
                   MFBundDevs$FinalThirdPasses * 0.5 +
                   MFBundDevs$ThroughBalls * 0.45 + 
                   MFBundDevs$Switches * 0.45 +
                   MFBundDevs$ShotCreatingActions * 0.45 +
                   MFBundDevs$Touches * 0.4 + MFBundDevs$AerialDuelsWon * 0.4 +
                   MFBundDevs$AerialDuelWinRate * 0.4 +
                   MFBundDevs$FoulsDrawn * 0.35 - MFBundDevs$Fouls * 0.35 +
                   MFBundDevs$CrossesIntoThePenaltyArea * 0.3 + 
                   MFBundDevs$SuccessfulTakeOns * 0.3 +
                   MFBundDevs$TakeOnSuccessRate * 0.3 +
                   MFBundDevs$TacklesWon * 0.3 -
                   MFBundDevs$AmtDispossessed * 0.3 +
                   MFBundDevs$Interceptions * 0.2 +
                   MFBundDevs$PassesBlocked * 0.2)

MFBundRanks <- MFBundRanks * MFBundDevs$Total90s

MFBundRanks <- data.frame(MFBundDevs[,1:6],MFBundRanks)


## Ligue 1 Midfielders
MFLig1 <- midfielders[which(midfielders$League == "Ligue 1"), ]
MFLig1Devs <- MFLig1

for (i in 1:length(MFLig1$Player)) {
  for (j in 11:74) {
    MFLig1Devs[i,j] <- (MFLig1[i,j] - mean(MFLig1[,j])) /  sd(MFLig1[,j])
  }
}

MFLig1Ranks <- c(MFLig1Devs$Assists + MFLig1Devs$PassesCompleted +
                   MFLig1Devs$ShotAssists * 0.85 + 
                   MFLig1Devs$PassCompletionPct * 0.85 +
                   MFLig1Devs$TotalProgressivePassDist * 0.7 + 
                   MFLig1Devs$Carries * 0.7 +
                   MFLig1Devs$PassesThatLedToShots * 0.7 +
                   MFLig1Devs$Goals * 0.6 + MFLig1Devs$GoalsPerShot * 0.6 +
                   MFLig1Devs$Crosses * 0.6 + MFLig1Devs$Recoveries * 0.55 +  
                   MFLig1Devs$ProgressivePasses * 0.55 + 
                   MFLig1Devs$PassesIntoThePenaltyArea * 0.55 +
                   MFLig1Devs$FinalThirdPasses * 0.5 +
                   MFLig1Devs$ThroughBalls * 0.45 + 
                   MFLig1Devs$Switches * 0.45 +
                   MFLig1Devs$ShotCreatingActions * 0.45 +
                   MFLig1Devs$Touches * 0.4 + MFLig1Devs$AerialDuelsWon * 0.4 +
                   MFLig1Devs$AerialDuelWinRate * 0.4 +
                   MFLig1Devs$FoulsDrawn * 0.35 - MFLig1Devs$Fouls * 0.35 +
                   MFLig1Devs$CrossesIntoThePenaltyArea * 0.3 + 
                   MFLig1Devs$SuccessfulTakeOns * 0.3 +
                   MFLig1Devs$TakeOnSuccessRate * 0.3 +
                   MFLig1Devs$TacklesWon * 0.3 -
                   MFLig1Devs$AmtDispossessed * 0.3 +
                   MFLig1Devs$Interceptions * 0.2 +
                   MFLig1Devs$PassesBlocked * 0.2)

MFLig1Ranks <- MFLig1Ranks * MFLig1Devs$Total90s

MFLig1Ranks <- data.frame(MFLig1Devs[,1:6],MFLig1Ranks)

## Serie A Midfielders
MFSerA <- midfielders[which(midfielders$League == "Serie A"), ]
MFSerADevs <- MFSerA

for (i in 1:length(MFSerA$Player)) {
  for (j in 11:74) {
    MFSerADevs[i,j] <- (MFSerA[i,j] - mean(MFSerA[,j])) /  sd(MFSerA[,j])
  }
}

MFSerARanks <- c(MFSerADevs$Assists + MFSerADevs$PassesCompleted +
                   MFSerADevs$ShotAssists * 0.85 + 
                   MFSerADevs$PassCompletionPct * 0.85 +
                   MFSerADevs$TotalProgressivePassDist * 0.7 + 
                   MFSerADevs$Carries * 0.7 +
                   MFSerADevs$PassesThatLedToShots * 0.7 +
                   MFSerADevs$Goals * 0.6 + MFSerADevs$GoalsPerShot * 0.6 +
                   MFSerADevs$Crosses * 0.6 + MFSerADevs$Recoveries * 0.55 +  
                   MFSerADevs$ProgressivePasses * 0.55 + 
                   MFSerADevs$PassesIntoThePenaltyArea * 0.55 +
                   MFSerADevs$FinalThirdPasses * 0.5 +
                   MFSerADevs$ThroughBalls * 0.45 + 
                   MFSerADevs$Switches * 0.45 +
                   MFSerADevs$ShotCreatingActions * 0.45 +
                   MFSerADevs$Touches * 0.4 + MFSerADevs$AerialDuelsWon * 0.4 +
                   MFSerADevs$AerialDuelWinRate * 0.4 +
                   MFSerADevs$FoulsDrawn * 0.35 - MFSerADevs$Fouls * 0.35 +
                   MFSerADevs$CrossesIntoThePenaltyArea * 0.3 + 
                   MFSerADevs$SuccessfulTakeOns * 0.3 +
                   MFSerADevs$TakeOnSuccessRate * 0.3 +
                   MFSerADevs$TacklesWon * 0.3 -
                   MFSerADevs$AmtDispossessed * 0.3 +
                   MFSerADevs$Interceptions * 0.2 +
                   MFSerADevs$PassesBlocked * 0.2)

MFSerARanks <- MFSerARanks * MFSerADevs$Total90s

MFSerARanks <- data.frame(MFSerADevs[,1:6],MFSerARanks)


######### Defender Scale #########
## 1.00 -- Tackle Success Rate, Tackles Won
## 0.85 -- Interceptions, Clearances, Errors(penalize)
## 0.70 -- Shots Blocked, Pass Completion Pct
## 0.60 -- Recoveries, Dribbled Past (Penalize)
## 0.55 -- Fouls Committed (penalize), Passes Blocked
## 0.50 -- Touches, Long Passes Completed, Long Pass Pct
## 0.45 -- Aerial Duels Won, Aerial Duel Win Rate
## 0.40 -- Progressive Carries, Prog Carry Dist
## 0.35 -- Passes Completed, Progressive Passes
## 0.30 -- Final Third Entries
## 0.20 -- Goals, Assists, Shot Assists
## Multiply final score by total 90s to give credit to players who play more


## Premier League Defenders
DFPrem <- defenders[which(defenders$League == "Premier League"), ]
DFPremDevs <- DFPrem

for (i in 1:length(DFPrem$Player)) {
  for (j in 11:74) {
    DFPremDevs[i,j] <- (DFPrem[i,j] - mean(DFPrem[,j])) /  sd(DFPrem[,j])
  }
}

DFPremRanks <- c(DFPremDevs$TackleSuccRate + DFPremDevs$TacklesWon +
                   DFPremDevs$Interceptions * 0.85 + 
                   DFPremDevs$Clearances * 0.85 -
                   DFPremDevs$Errors * 0.85 +
                   DFPremDevs$ShotsBlocked * 0.7 +
                   DFPremDevs$PassCompletionPct * 0.7 +
                   DFPremDevs$Recoveries * 0.6 -
                   DFPremDevs$DribbledPast * 0.6 -
                   DFPremDevs$Fouls * 0.55 +
                   DFPremDevs$PassesBlocked * 0.55 +
                   DFPremDevs$Touches * 0.5 + 
                   DFPremDevs$LongPassesCompleted * 0.5 +
                   DFPremDevs$LongPassCompletionPct * 0.5 +
                   DFPremDevs$AerialDuelsWon * 0.45 +
                   DFPremDevs$AerialDuelWinRate * 0.45 +
                   DFPremDevs$ProgressiveCarries * 0.4 +
                   DFPremDevs$ProgressiveCarryDistance * 0.4 +
                   DFPremDevs$PassesCompleted * 0.35 +
                   DFPremDevs$ProgressivePasses * 0.35 +
                   DFPremDevs$FinalThirdEntries * 0.3 +
                   DFPremDevs$Goals * 0.2 + DFPremDevs$Assists * 0.2 +
                   DFPremDevs$ShotAssists * 0.2)
                   
DFPremRanks <- DFPremRanks * DFPremDevs$Total90s

DFPremRanks <- data.frame(DFPremDevs[,1:6],DFPremRanks)

## La Liga Defenders
DFLiga <- defenders[which(defenders$League == "La Liga"), ]
DFLigaDevs <- DFLiga

for (i in 1:length(DFLiga$Player)) {
  for (j in 11:74) {
    DFLigaDevs[i,j] <- (DFLiga[i,j] - mean(DFLiga[,j])) /  sd(DFLiga[,j])
  }
}

DFLigaRanks <- c(DFLigaDevs$TackleSuccRate + DFLigaDevs$TacklesWon +
                   DFLigaDevs$Interceptions * 0.85 + 
                   DFLigaDevs$Clearances * 0.85 -
                   DFLigaDevs$Errors * 0.85 +
                   DFLigaDevs$ShotsBlocked * 0.7 +
                   DFLigaDevs$PassCompletionPct * 0.7 +
                   DFLigaDevs$Recoveries * 0.6 -
                   DFLigaDevs$DribbledPast * 0.6 -
                   DFLigaDevs$Fouls * 0.55 +
                   DFLigaDevs$PassesBlocked * 0.55 +
                   DFLigaDevs$Touches * 0.5 + 
                   DFLigaDevs$LongPassesCompleted * 0.5 +
                   DFLigaDevs$LongPassCompletionPct * 0.5 +
                   DFLigaDevs$AerialDuelsWon * 0.45 +
                   DFLigaDevs$AerialDuelWinRate * 0.45 +
                   DFLigaDevs$ProgressiveCarries * 0.4 +
                   DFLigaDevs$ProgressiveCarryDistance * 0.4 +
                   DFLigaDevs$PassesCompleted * 0.35 +
                   DFLigaDevs$ProgressivePasses * 0.35 +
                   DFLigaDevs$FinalThirdEntries * 0.3 +
                   DFLigaDevs$Goals * 0.2 + DFLigaDevs$Assists * 0.2 +
                   DFLigaDevs$ShotAssists * 0.2)

DFLigaRanks <- DFLigaRanks * DFLigaDevs$Total90s

DFLigaRanks <- data.frame(DFLigaDevs[,1:6],DFLigaRanks)

## Bundesliga Defenders
DFBund <- defenders[which(defenders$League == "Bundesliga"), ]
DFBundDevs <- DFBund

for (i in 1:length(DFBund$Player)) {
  for (j in 11:74) {
    DFBundDevs[i,j] <- (DFBund[i,j] - mean(DFBund[,j])) /  sd(DFBund[,j])
  }
}

DFBundRanks <- c(DFBundDevs$TackleSuccRate + DFBundDevs$TacklesWon +
                   DFBundDevs$Interceptions * 0.85 + 
                   DFBundDevs$Clearances * 0.85 -
                   DFBundDevs$Errors * 0.85 +
                   DFBundDevs$ShotsBlocked * 0.7 +
                   DFBundDevs$PassCompletionPct * 0.7 +
                   DFBundDevs$Recoveries * 0.6 -
                   DFBundDevs$DribbledPast * 0.6 -
                   DFBundDevs$Fouls * 0.55 +
                   DFBundDevs$PassesBlocked * 0.55 +
                   DFBundDevs$Touches * 0.5 + 
                   DFBundDevs$LongPassesCompleted * 0.5 +
                   DFBundDevs$LongPassCompletionPct * 0.5 +
                   DFBundDevs$AerialDuelsWon * 0.45 +
                   DFBundDevs$AerialDuelWinRate * 0.45 +
                   DFBundDevs$ProgressiveCarries * 0.4 +
                   DFBundDevs$ProgressiveCarryDistance * 0.4 +
                   DFBundDevs$PassesCompleted * 0.35 +
                   DFBundDevs$ProgressivePasses * 0.35 +
                   DFBundDevs$FinalThirdEntries * 0.3 +
                   DFBundDevs$Goals * 0.2 + DFBundDevs$Assists * 0.2 +
                   DFBundDevs$ShotAssists * 0.2)

DFBundRanks <- DFBundRanks * DFBundDevs$Total90s

DFBundRanks <- data.frame(DFBundDevs[,1:6],DFBundRanks)

## Ligue 1 Defenders
DFLig1 <- defenders[which(defenders$League == "Ligue 1"), ]
DFLig1Devs <- DFLig1

for (i in 1:length(DFLig1$Player)) {
  for (j in 11:74) {
    DFLig1Devs[i,j] <- (DFLig1[i,j] - mean(DFLig1[,j])) /  sd(DFLig1[,j])
  }
}

DFLig1Ranks <- c(DFLig1Devs$TackleSuccRate + DFLig1Devs$TacklesWon +
                   DFLig1Devs$Interceptions * 0.85 + 
                   DFLig1Devs$Clearances * 0.85 -
                   DFLig1Devs$Errors * 0.85 +
                   DFLig1Devs$ShotsBlocked * 0.7 +
                   DFLig1Devs$PassCompletionPct * 0.7 +
                   DFLig1Devs$Recoveries * 0.6 -
                   DFLig1Devs$DribbledPast * 0.6 -
                   DFLig1Devs$Fouls * 0.55 +
                   DFLig1Devs$PassesBlocked * 0.55 +
                   DFLig1Devs$Touches * 0.5 + 
                   DFLig1Devs$LongPassesCompleted * 0.5 +
                   DFLig1Devs$LongPassCompletionPct * 0.5 +
                   DFLig1Devs$AerialDuelsWon * 0.45 +
                   DFLig1Devs$AerialDuelWinRate * 0.45 +
                   DFLig1Devs$ProgressiveCarries * 0.4 +
                   DFLig1Devs$ProgressiveCarryDistance * 0.4 +
                   DFLig1Devs$PassesCompleted * 0.35 +
                   DFLig1Devs$ProgressivePasses * 0.35 +
                   DFLig1Devs$FinalThirdEntries * 0.3 +
                   DFLig1Devs$Goals * 0.2 + DFLig1Devs$Assists * 0.2 +
                   DFLig1Devs$ShotAssists * 0.2)

DFLig1Ranks <- DFLig1Ranks * DFLig1Devs$Total90s

DFLig1Ranks <- data.frame(DFLig1Devs[,1:6],DFLig1Ranks)

## Serie A Defenders

DFSerA <- defenders[which(defenders$League == "Serie A"), ]
DFSerADevs <- DFSerA

for (i in 1:length(DFSerA$Player)) {
  for (j in 11:74) {
    DFSerADevs[i,j] <- (DFSerA[i,j] - mean(DFSerA[,j])) /  sd(DFSerA[,j])
  }
}

DFSerARanks <- c(DFSerADevs$TackleSuccRate + DFSerADevs$TacklesWon +
                   DFSerADevs$Interceptions * 0.85 + 
                   DFSerADevs$Clearances * 0.85 -
                   DFSerADevs$Errors * 0.85 +
                   DFSerADevs$ShotsBlocked * 0.7 +
                   DFSerADevs$PassCompletionPct * 0.7 +
                   DFSerADevs$Recoveries * 0.6 -
                   DFSerADevs$DribbledPast * 0.6 -
                   DFSerADevs$Fouls * 0.55 +
                   DFSerADevs$PassesBlocked * 0.55 +
                   DFSerADevs$Touches * 0.5 + 
                   DFSerADevs$LongPassesCompleted * 0.5 +
                   DFSerADevs$LongPassCompletionPct * 0.5 +
                   DFSerADevs$AerialDuelsWon * 0.45 +
                   DFSerADevs$AerialDuelWinRate * 0.45 +
                   DFSerADevs$ProgressiveCarries * 0.4 +
                   DFSerADevs$ProgressiveCarryDistance * 0.4 +
                   DFSerADevs$PassesCompleted * 0.35 +
                   DFSerADevs$ProgressivePasses * 0.35 +
                   DFSerADevs$FinalThirdEntries * 0.3 +
                   DFSerADevs$Goals * 0.2 + DFSerADevs$Assists * 0.2 +
                   DFSerADevs$ShotAssists * 0.2)

DFSerARanks <- DFSerARanks * DFSerADevs$Total90s

DFSerARanks <- data.frame(DFSerADevs[,1:6],DFSerARanks)

######### Goalkeepeers Scale #########
## 1.00 -- Errors (penalize)
## 0.85 -- Clearances, Recoveries
## 0.70 -- Aerial Duels Won, Aerial Duel Win Rate
## 0.60 -- Fouls (penalize)
## 0.50 -- Pass Completion Pct, Touches
## 0.40 -- Passes Completed, Long Pass Completion Pct
## Multiply final score by total 90s to give credit to players who play more
## No save/goal prevention data so not a lot I can use


## Premier League Goalkeepers
GKPrem <- goalies[which(goalies$League == "Premier League"), ]
GKPremDevs <- GKPrem

for (i in 1:length(GKPrem$Player)) {
  for (j in 11:74) {
    GKPremDevs[i,j] <- (GKPrem[i,j] - mean(GKPrem[,j])) /  sd(GKPrem[,j])
  }
}

GKPremRanks <- c(0 - GKPremDevs$Errors + GKPremDevs$Clearances * 0.85 +
                   GKPremDevs$Recoveries * 0.85 +
                   GKPremDevs$AerialDuelsWon * 0.7 +
                   GKPremDevs$AerialDuelWinRate * 0.7 -
                   GKPremDevs$Fouls * 0.6 +
                   GKPremDevs$PassCompletionPct * 0.5 +
                   GKPremDevs$Touches * 0.5 +
                   GKPremDevs$PassesCompleted * 0.4 +
                   GKPremDevs$LongPassCompletionPct * 0.4)

GKPremRanks <- GKPremRanks * GKPremDevs$Total90s

GKPremRanks <- data.frame(GKPremDevs[,1:6],GKPremRanks)

## La Liga Goalkeepers
GKLiga <- goalies[which(goalies$League == "La Liga"), ]
GKLigaDevs <- GKLiga

for (i in 1:length(GKLiga$Player)) {
  for (j in 11:74) {
    GKLigaDevs[i,j] <- (GKLiga[i,j] - mean(GKLiga[,j])) /  sd(GKLiga[,j])
  }
}

GKLigaRanks <- c(0 - GKLigaDevs$Errors + GKLigaDevs$Clearances * 0.85 +
                   GKLigaDevs$Recoveries * 0.85 +
                   GKLigaDevs$AerialDuelsWon * 0.7 +
                   GKLigaDevs$AerialDuelWinRate * 0.7 -
                   GKLigaDevs$Fouls * 0.6 +
                   GKLigaDevs$PassCompletionPct * 0.5 +
                   GKLigaDevs$Touches * 0.5 +
                   GKLigaDevs$PassesCompleted * 0.4 +
                   GKLigaDevs$LongPassCompletionPct * 0.4)

GKLigaRanks <- GKLigaRanks * GKLigaDevs$Total90s

GKLigaRanks <- data.frame(GKLigaDevs[,1:6],GKLigaRanks)

## Bundesliga Goalkeepers
GKBund <- goalies[which(goalies$League == "Bundesliga"), ]
GKBundDevs <- GKBund

for (i in 1:length(GKBund$Player)) {
  for (j in 11:74) {
    GKBundDevs[i,j] <- (GKBund[i,j] - mean(GKBund[,j])) /  sd(GKBund[,j])
  }
}

GKBundRanks <- c(0 - GKBundDevs$Errors + GKBundDevs$Clearances * 0.85 +
                   GKBundDevs$Recoveries * 0.85 +
                   GKBundDevs$AerialDuelsWon * 0.7 +
                   GKBundDevs$AerialDuelWinRate * 0.7 -
                   GKBundDevs$Fouls * 0.6 +
                   GKBundDevs$PassCompletionPct * 0.5 +
                   GKBundDevs$Touches * 0.5 +
                   GKBundDevs$PassesCompleted * 0.4 +
                   GKBundDevs$LongPassCompletionPct * 0.4)

GKBundRanks <- GKBundRanks * GKBundDevs$Total90s

GKBundRanks <- data.frame(GKBundDevs[,1:6],GKBundRanks)

## Ligue 1 Goalkeepers
GKLig1 <- goalies[which(goalies$League == "Ligue 1"), ]
GKLig1Devs <- GKLig1

for (i in 1:length(GKLig1$Player)) {
  for (j in 11:74) {
    GKLig1Devs[i,j] <- (GKLig1[i,j] - mean(GKLig1[,j])) /  sd(GKLig1[,j])
  }
}

GKLig1Ranks <- c(0 - GKLig1Devs$Errors + GKLig1Devs$Clearances * 0.85 +
                   GKLig1Devs$Recoveries * 0.85 +
                   GKLig1Devs$AerialDuelsWon * 0.7 +
                   GKLig1Devs$AerialDuelWinRate * 0.7 -
                   GKLig1Devs$Fouls * 0.6 +
                   GKLig1Devs$PassCompletionPct * 0.5 +
                   GKLig1Devs$Touches * 0.5 +
                   GKLig1Devs$PassesCompleted * 0.4 +
                   GKLig1Devs$LongPassCompletionPct * 0.4)

GKLig1Ranks <- GKLig1Ranks * GKLig1Devs$Total90s

GKLig1Ranks <- data.frame(GKLig1Devs[,1:6],GKLig1Ranks)

## Serie A Goalkeepers
GKSerA <- goalies[which(goalies$League == "Serie A"), ]
GKSerADevs <- GKSerA

for (i in 1:length(GKSerA$Player)) {
  for (j in 11:74) {
    GKSerADevs[i,j] <- (GKSerA[i,j] - mean(GKSerA[,j])) /  sd(GKSerA[,j])
  }
}

GKSerARanks <- c(0 - GKSerADevs$Errors + GKSerADevs$Clearances * 0.85 +
                   GKSerADevs$Recoveries * 0.85 +
                   GKSerADevs$AerialDuelsWon * 0.7 +
                   GKSerADevs$AerialDuelWinRate * 0.7 -
                   GKSerADevs$Fouls * 0.6 +
                   GKSerADevs$PassCompletionPct * 0.5 +
                   GKSerADevs$Touches * 0.5 +
                   GKSerADevs$PassesCompleted * 0.4 +
                   GKSerADevs$LongPassCompletionPct * 0.4)

GKSerARanks <- GKSerARanks * GKSerADevs$Total90s

GKSerARanks <- data.frame(GKSerADevs[,1:6],GKSerARanks)


#------------------------------------------------------------------------------#
#--------------------- PART 9: OVERALL TEAM OF THE SEASON ---------------------#
## same formula as above, but with all players
## NEW: Adjust all players based on their leagues euro coefficient (reward harder leagues)
## take the top 5 from each category and put them together, adjusting for dupes


## FORWARDS
FWOVR <- forwards
FWOVRDevs <- FWOVR

for (i in 1:length(FWOVR$Player)) {
  for (j in 11:74) {
    FWOVRDevs[i,j] <- (FWOVR[i,j] - mean(FWOVR[,j])) /  sd(FWOVR[,j])
  }
}

FWOVRRanks <- c(FWOVRDevs$Goals + FWOVRDevs$GoalsPerShot +
                  FWOVRDevs$Shots * 0.85 + FWOVRDevs$ShotsOnTarget * 0.85 +
                  FWOVRDevs$Assists * 0.7 + FWOVRDevs$ShotAssists * 0.6 +
                  FWOVRDevs$TouchesInPenaltyArea * 0.55 +  
                  FWOVRDevs$GoalCreatingActions * 0.55 +
                  FWOVRDevs$ShotCreatingActions * 0.5 -
                  FWOVRDevs$Offsides * 0.45 + 
                  FWOVRDevs$SuccessfulTakeOns * 0.45 + 
                  FWOVRDevs$TakeOnSuccessRate * 0.45 + 
                  FWOVRDevs$PassesIntoThePenaltyArea * 0.4 +
                  FWOVRDevs$TouchesInAttackingThird * 0.4 +
                  FWOVRDevs$PenaltyBoxEntries * 0.35 +
                  FWOVRDevs$AverageShotDistance * 0.3 +
                  FWOVRDevs$FoulsDrawn * 0.2 - FWOVRDevs$Fouls * 0.2)
FWOVRRanks <- FWOVRRanks * FWOVRDevs$Total90s

FWOVRRanks <- data.frame(FWOVRDevs[,1:6],FWOVRRanks)

FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "Premier League"] <- FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "Premier League"] * 1.04785
FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "La Liga"] <- FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "La Liga"] * 0.96499
FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "Bundesliga"] <- FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "Bundesliga"] * 0.91241
FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "Serie A"] <- FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "Serie A"] * 0.90963
FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "Ligue 1"] <- FWOVRRanks$FWOVRRanks[FWOVRRanks$League == "Ligue 1"] * 0.80582



## MIDFIELDERS
MFOVR <- midfielders
MFOVRDevs <- MFOVR

for (i in 1:length(MFOVR$Player)) {
  for (j in 11:74) {
    MFOVRDevs[i,j] <- (MFOVR[i,j] - mean(MFOVR[,j])) /  sd(MFOVR[,j])
  }
}

MFOVRRanks <- c(MFOVRDevs$Assists + MFOVRDevs$PassesCompleted +
                  MFOVRDevs$ShotAssists * 0.85 + 
                  MFOVRDevs$PassCompletionPct * 0.85 +
                  MFOVRDevs$TotalProgressivePassDist * 0.7 + 
                  MFOVRDevs$Carries * 0.7 +
                  MFOVRDevs$PassesThatLedToShots * 0.7 +
                  MFOVRDevs$Goals * 0.6 + MFOVRDevs$GoalsPerShot * 0.6 +
                  MFOVRDevs$Crosses * 0.6 + MFOVRDevs$Recoveries * 0.55 +  
                  MFOVRDevs$ProgressivePasses * 0.55 + 
                  MFOVRDevs$PassesIntoThePenaltyArea * 0.55 +
                  MFOVRDevs$FinalThirdPasses * 0.5 +
                  MFOVRDevs$ThroughBalls * 0.45 + 
                  MFOVRDevs$Switches * 0.45 +
                  MFOVRDevs$ShotCreatingActions * 0.45 +
                  MFOVRDevs$Touches * 0.4 + MFOVRDevs$AerialDuelsWon * 0.4 +
                  MFOVRDevs$AerialDuelWinRate * 0.4 +
                  MFOVRDevs$FoulsDrawn * 0.35 - MFOVRDevs$Fouls * 0.35 +
                  MFOVRDevs$CrossesIntoThePenaltyArea * 0.3 + 
                  MFOVRDevs$SuccessfulTakeOns * 0.3 +
                  MFOVRDevs$TakeOnSuccessRate * 0.3 +
                  MFOVRDevs$TacklesWon * 0.3 -
                  MFOVRDevs$AmtDispossessed * 0.3 +
                  MFOVRDevs$Interceptions * 0.2 +
                  MFOVRDevs$PassesBlocked * 0.2)

MFOVRRanks <- MFOVRRanks * MFOVRDevs$Total90s

MFOVRRanks <- data.frame(MFOVRDevs[,1:6],MFOVRRanks)

MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "Premier League"] <- MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "Premier League"] * 1.04785
MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "La Liga"] <- MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "La Liga"] * 0.96499
MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "Bundesliga"] <- MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "Bundesliga"] * 0.91241
MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "Serie A"] <- MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "Serie A"] * 0.90963
MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "Ligue 1"] <- MFOVRRanks$MFOVRRanks[MFOVRRanks$League == "Ligue 1"] * 0.80582

## DEFENDERS
DFOVR <- defenders
DFOVRDevs <- DFOVR

for (i in 1:length(DFOVR$Player)) {
  for (j in 11:74) {
    DFOVRDevs[i,j] <- (DFOVR[i,j] - mean(DFOVR[,j])) /  sd(DFOVR[,j])
  }
}

DFOVRRanks <- c(DFOVRDevs$TackleSuccRate + DFOVRDevs$TacklesWon +
                  DFOVRDevs$Interceptions * 0.85 + 
                  DFOVRDevs$Clearances * 0.85 -
                  DFOVRDevs$Errors * 0.85 +
                  DFOVRDevs$ShotsBlocked * 0.7 +
                  DFOVRDevs$PassCompletionPct * 0.7 +
                  DFOVRDevs$Recoveries * 0.6 -
                  DFOVRDevs$DribbledPast * 0.6 -
                  DFOVRDevs$Fouls * 0.55 +
                  DFOVRDevs$PassesBlocked * 0.55 +
                  DFOVRDevs$Touches * 0.5 + 
                  DFOVRDevs$LongPassesCompleted * 0.5 +
                  DFOVRDevs$LongPassCompletionPct * 0.5 +
                  DFOVRDevs$AerialDuelsWon * 0.45 +
                  DFOVRDevs$AerialDuelWinRate * 0.45 +
                  DFOVRDevs$ProgressiveCarries * 0.4 +
                  DFOVRDevs$ProgressiveCarryDistance * 0.4 +
                  DFOVRDevs$PassesCompleted * 0.35 +
                  DFOVRDevs$ProgressivePasses * 0.35 +
                  DFOVRDevs$FinalThirdEntries * 0.3 +
                  DFOVRDevs$Goals * 0.2 + DFOVRDevs$Assists * 0.2 +
                  DFOVRDevs$ShotAssists * 0.2)

DFOVRRanks <- DFOVRRanks * DFOVRDevs$Total90s

DFOVRRanks <- data.frame(DFOVRDevs[,1:6],DFOVRRanks)

DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "Premier League"] <- DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "Premier League"] * 1.04785
DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "La Liga"] <- DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "La Liga"] * 0.96499
DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "Bundesliga"] <- DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "Bundesliga"] * 0.91241
DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "Serie A"] <- DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "Serie A"] * 0.90963
DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "Ligue 1"] <- DFOVRRanks$DFOVRRanks[DFOVRRanks$League == "Ligue 1"] * 0.80582


## GOALKEEPERS
GKOVR <- goalies
GKOVRDevs <- GKOVR

for (i in 1:length(GKOVR$Player)) {
  for (j in 11:74) {
    GKOVRDevs[i,j] <- (GKOVR[i,j] - mean(GKOVR[,j])) /  sd(GKOVR[,j])
  }
}

GKOVRRanks <- c(0 - GKOVRDevs$Errors + GKOVRDevs$Clearances * 0.85 +
                  GKOVRDevs$Recoveries * 0.85 +
                  GKOVRDevs$AerialDuelsWon * 0.7 +
                  GKOVRDevs$AerialDuelWinRate * 0.7 -
                  GKOVRDevs$Fouls * 0.6 +
                  GKOVRDevs$PassCompletionPct * 0.5 +
                  GKOVRDevs$Touches * 0.5 +
                  GKOVRDevs$PassesCompleted * 0.4 +
                  GKOVRDevs$LongPassCompletionPct * 0.4)

GKOVRRanks <- GKOVRRanks * GKOVRDevs$Total90s

GKOVRRanks <- data.frame(GKOVRDevs[,1:6],GKOVRRanks)

GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "Premier League"] <- GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "Premier League"] * 1.04785
GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "La Liga"] <- GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "La Liga"] * 0.96499
GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "Bundesliga"] <- GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "Bundesliga"] * 0.91241
GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "Serie A"] <- GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "Serie A"] * 0.90963
GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "Ligue 1"] <- GKOVRRanks$GKOVRRanks[GKOVRRanks$League == "Ligue 1"] * 0.80582
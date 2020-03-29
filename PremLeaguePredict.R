library(tidyverse)
library(data.table)
library(stringr)
library(scales)
library(formattable)


season_data1314 <- read.csv("season_data_three_four.csv")[2:11]
season_data1415 <- read.csv("season_data_four_five.csv")[2:11]
season_data1516 <- read.csv("season_data_five_six.csv")[1:10]
season_data1617 <- read.csv("season_data_six_seven.csv")[1:10]
season_data1718 <- read.csv("season_data_seven_eight.csv")[2:11]
season_data1819 <- read.csv("season_data_eight_nine.csv")[2:11]

filenames <- list(season_data1314, season_data1415, season_data1516, season_data1617, season_data1718, season_data1819)

all_seasons <- do.call("rbind", filenames)

head(season_data1314)

team_color <- function(team){
  if(team == "Arsenal"){return("red")}
  if(team == "Aston Villa"){return("#95BFE5")}
  if(team == "Cardiff"){return("#0070B5")}
  if(team == "Chelsea"){return("#034694")}
  if(team == "Crystal Palace"){return("#1b458f")}
  if(team == "Everton"){return("#274488")}
  if(team == "Fulham"){return("Black")}
  if(team == "Hull"){return("#f5971d")}
  if(team == "Liverpool"){return("#C8102E")}
  if(team == "Man City"){return("#6CABDD")}
  if(team == "Man United"){return("#DA291C")}
  if(team == "Newcastle"){return("#241F20")}
  if(team == "Norwich"){return("#FFF200")}
  if(team == "Southampton"){return("#034694")}
  if(team == "Stoke"){return("#E03A3E")}
  if(team == "Sunderland"){return("#EB172B")}
  if(team == "Swansea"){return("Black")}
  if(team == "Tottenham"){return("#132257")}
  if(team == "West Brom"){return("#122F67")}
  if(team == "West Ham"){return("#7A263A")}
  if(team == "Burnley"){return("#6C1D45")}
  if(team == "Leicester"){return("#003090")}
  if(team == "QPR"){return("#1d5ba4")}
  if(team == "Bournemouth"){return("#DA291C")}
  if(team == "Watford"){return("#FBEE23")}
  if(team == "Middlesbrough"){return("#6d6e71")}
  if(team == "Brighton"){return("#0057B8")}
  if(team == "Huddersfield"){return("#0E63AD")}
  if(team == "Wolves"){return("#FDB913")}
  return("White")
}

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"


reordered <- list("Date", "AwayTeam", "HomeTeam", "FTAG", "FTHG", "FTR", "HTAG", "HTHG", "HTR", "Referee")
dup <- all_seasons
names(dup) <- reordered
dup <- dup[, c(1,3,2,5,4,6,8,7,9,10)]

#switch Home and Away results
FTRaways <- which(dup$FTR == "A")
FTRhomes <- which(dup$FTR == "H")
dup$FTR[FTRaways] <- "H"
dup$FTR[FTRhomes] <- "A"

HTRaways <- which(dup$HTR == "A")
HTRhomes <- which(dup$HTR == "H")
dup$HTR[HTRaways] <- "H"
dup$HTR[HTRhomes] <- "A"

master <- drop_na(rbind(all_seasons, dup)) %>% 
  separate(Date, c("Day", "Month", "Year"), "[/]")

for (i in 1:nrow(master)){
  if (str_length(master$Year[i]) == 4){
    master$Year[i] <- master$Year[3:4]
  }
}


master$Date <- paste(master$Day, master$Month, master$Year, sep = "/")

master <- master[order(as.Date(master$Date, format="%d/%m/%Y"), decreasing = TRUE),]

master$date_diff <- as.Date(as.character("01/08/19"), format="%d/%m/%Y") - as.Date(master$Date, format="%d/%m/%Y")
master$date_weight <- exp((as.numeric(master$date_diff)-50)*-1)

master$GD <- master$FTHG - master$FTAG 

head(master)

master_sd <- sd(master$GD)
master_mean <- mean(master$GD)



teams <- levels(all_seasons$HomeTeam)
goals_table <- data.frame(matrix(0, ncol = 6, nrow =nlevels (all_seasons$HomeTeam))) 
names(goals_table) <- c("Team", "GoalsFor", "OffenseRating", "GoalsAgainst", "DefenseRating", "TotalRating")

goals_table$Team <- levels(all_seasons$HomeTeam)

for(i in 1:nrow(goals_table)){
  home_games <- which(all_seasons$HomeTeam == goals_table$Team[i])
  away_games <- which(all_seasons$AwayTeam == goals_table$Team[i])
  
  home_games <- home_games[which(master$date_diff[home_games] < 200)]
  away_games <- away_games[which(master$date_diff[home_games] < 200)]
  
  goals_table$GoalsFor[i] <- (sum(all_seasons$FTHG[home_games]/length(home_games)*master$date_weight[home_games]))
  + (sum(all_seasons$FTAG[away_games]/length(away_games)*master$date_weight[away_games]))
  
  goals_table$GoalsAgainst[i] <-
    (sum(all_seasons$FTAG[home_games]/length(home_games)*master$date_weight[home_games])) +
    (sum(all_seasons$FTHG[away_games]/length(away_games)*master$date_weight[away_games]))
}

goals_table <- goals_table[which(goals_table$GoalsFor != 0),]



goals_table$OffenseRating <- (round(pnorm(goals_table$GoalsFor, mean = mean(goals_table$GoalsFor), sd =
                                     sd(goals_table$GoalsFor))*10, 1)+10)/2

goals_table$DefenseRating <- (round((1 - pnorm(goals_table$GoalsAgainst, mean = mean(goals_table$GoalsAgainst), 
                                               sd = sd(goals_table$GoalsAgainst)))*10, 1) + 10)/2

goals_table$TotalRating <- round((goals_table$OffenseRating + goals_table$DefenseRating)/2, 1)


team_formatter <- formatter("span", style = x ~ style(font.weight = "bold", color = team_colors))
rival_formatter <- formatter("span", style = x ~ style(font.weight = "bold", color = rival_colors))


goals_table %>% 
  arrange(-TotalRating) %>% 
  select("Team" = Team, "Offense Rating" = OffenseRating, "Defense Rating" = DefenseRating, "Overall Rating" = TotalRating) %>%
  
  formattable(align = c("l", "c", "c", "c"), list( 
    "Offense Rating" = color_tile(customRed,customGreen),
    "Defense Rating" = color_tile(customRed,customGreen),
    "Overall Rating" = color_tile(customRed, customGreen)))


ref_table <- master %>% 
  group_by(HomeTeam, Referee) %>% 
  summarise(ref.aid = mean(GD)/(master_sd/sqrt(n())), count = n()) %>%
  filter(count >= 3 & !is.nan(ref.aid)) %>% 
  select(HomeTeam, Referee, ref.aid) %>% 
  arrange(ref.aid)

ref_sample <- rbind(head(ref_table), tail(ref_table))

ref_sample <- ref_sample[which(!duplicated(ref_sample$HomeTeam)),]

ref_sample$HomeTeam <- factor(ref_sample$HomeTeam, levels = ref_sample$HomeTeam[order(ref_sample$ref.aid)])

color_vector <- NULL
for(i in 1:nrow(ref_sample)){
  color_vector[i] <- team_color(ref_sample$HomeTeam[i])
}


ggplot(ref_sample, aes(x = HomeTeam, y = ref.aid)) +
  geom_col(fill = color_vector) +
  geom_text(aes(label = Referee), vjust = ifelse(ref_sample$ref.aid < 0, 1.3, -0.3), size = 3) +
  labs(title="Premier League Team-Referee Relationships", 
       subtitle="Calculated based on goal difference in each match with given team and given referee", 
       x="Team", 
       y="Referee Aid") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = pretty_breaks()) +
  geom_hline(aes(yintercept = 0))


master$ref.aid <- 0
for (i in 1:nrow(master)){
  j <- ref_table$ref.aid[which(ref_table$HomeTeam == master$HomeTeam[i] & 
                                 ref_table$Referee == master$Referee[i] )]
  master$ref.aid[i] <- ifelse(length(j) != 0, j, 0)
}


riv_table <- master %>% 
  group_by(HomeTeam, AwayTeam) %>% 
  summarise(Games_Played = n(), std_dev = sd(GD), mean.GD = mean(GD),
            riv.fact = mean(GD)/(master_sd/sqrt(n()))) %>%
  select("Team" = HomeTeam, "Opponent" = AwayTeam, "Games Played" = Games_Played, "Mean Goal Differential" =
           mean.GD, "Standard Deviation" = std_dev, riv.fact) %>% 
  arrange(-riv.fact)

names(riv_table)[6] <- "Opponent Factor"

riv_table %>% 
  formattable(align = c("l","l","c","c","c", "r"),
              list("Games Played" = color_tile(customGreen0, customGreen),
                   "Mean Goal Differential" = color_tile(customRed, customGreen),
                   "Standard Deviation" = color_tile(customGreen, customGreen0),
                   "Opponent Factor" = color_tile(customRed, customGreen)))



master$riv.aid <- 0
for (i in 1:nrow(master)){
  j <- riv_table$riv.fact[which(riv_table$Team == master$HomeTeam[i] & 
                                 riv_table$Opponent == master$AwayTeam[i] )]
  master$riv.aid[i] <- ifelse(length(j) != 0, j, 0)
}

mv_data <- read.csv("MV_data.csv", header = TRUE, stringsAsFactors = FALSE)[c(1,5,6)]

names(mv_data) <- c("Team", "Market_Value", "Growth")

for (i in 1:nrow(mv_data)){
  mv_data$MV[i] <- sub("€", "", mv_data$Market_Value[i])
  if(substr(mv_data$MV[i], nchar(mv_data$MV[i]), nchar(mv_data$MV[i])) == "m")
  {
    mv_data$MV[i] <- as.numeric(substr(mv_data$MV[i], 0, nchar(mv_data$MV[i])-1))*10^6}
  else
  {
    mv_data$MV[i] <- as.numeric(substr(mv_data$MV[i], 0,
                                                 nchar(mv_data$MV[i])-2))*10^9
  }
  mv_data$Growth[i] <- sub(",",".", mv_data$Growth[i])
}

mean_mv <- mean(as.numeric(mv_data$MV))
sd_mv <- sd(mv_data$MV)
n_mv <- nrow(mv_data)

mv_data$mv_factor <- (pnorm(as.numeric(mv_data$MV), mean = mean_mv, sd = sd_mv))

mv_data %>% 
  select(Team, "Market Value" = Market_Value, Growth, "Market Factor" = mv_factor) %>% 
  formattable(align = c("l"))

mv_data <- mv_data %>% 
  arrange(Team)
predictions <- data.frame(matrix(0, nrow = nlevels(master$HomeTeam)-1, ncol = 6))

names(predictions) <- c("Team", "OffenseRating", "DefenseRating", "OverallRating", "MarketFactor", "Chances")

predictions$Team <- levels(master$HomeTeam)[-3]



for (i in 1:nrow(predictions)){
  o_rating <- goals_table$OffenseRating[which(goals_table$Team == predictions$Team[i])]
  if(length(o_rating) == 0){predictions <- predictions[-c(i),]}else{
    predictions$OffenseRating[i] <- goals_table$OffenseRating[which(goals_table$Team == predictions$Team[i])]
    predictions$DefenseRating[i] <- goals_table$DefenseRating[which(goals_table$Team == predictions$Team[i])]
    predictions$OverallRating[i] <- goals_table$TotalRating[which(goals_table$Team == predictions$Team[i])]
  }
}

predictions <- predictions[which(predictions$OffenseRating != 0),]

for (i in 1:nrow(predictions)){
  mv <- mv_data$MV[which(mv_data$Team == predictions$Team[i])]
  if(length(mv) == 0){predictions <- predictions[-c(i),]}else{
    predictions$MV[i] <- mv
  }
}


predictions$Chances <- as.numeric(predictions$MV)*predictions$OverallRating

total_pred <- sum(predictions$Chances)

predictions$Chances <- percent(predictions$Chances/total_pred)

predictions %>% 
  arrange(-Chances) %>% 
  select("Team" = Team, "Chance of Winning" = Chances)

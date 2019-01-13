
# Import libraries
library(dplyr)
library(readr)
library(tidyr)
library(corrr)
library(highcharter)
library(RcppRoll)
library(tidyselect)
library(lubridate)
library(stringr)
library(zoo)
library(purrr)
library(caret)
library(xgboost)

##########################
#### Define functions ####
##########################

split_winner_loser_columns <- function(df) {
  # This function splits the raw data into two dataframes and appends them together then shuffles them
  # This output is a dataframe with only one player's stats on each row
  # Grab a df with only the Winner's stats
  winner = df %>% 
    select(-contains("Loser")) %>% # Select only the Winner columns + extra game info columns as a df
    rename_at( # Rename all columns containing "Winner" to "Player" 
      vars(contains("Winner")),
      ~str_replace(., "Winner", "Player")
    ) %>%
    mutate(Winner = 1) # Create a target column
  
  # Repeat the process with the loser's stats
  loser = df %>%
    select(-contains("Winner")) %>%
    rename_at(
      vars(contains("Loser")),
      ~str_replace(., "Loser", "Player")
    ) %>%
    mutate(Winner = 0)
  
  set.seed(183) # Set seed to replicate results - 183 is the most games played in a tennis match (Isner-Mahut)
  
  # Create a df that appends both the Winner and loser df together
  combined_df = winner %>% 
    rbind(loser) %>% # Append the loser df to the Winner df
    dplyr::slice(sample(1:n())) %>% # Randomise row order
    arrange(Match_Id) %>% # Arrange by Match_Id
    return()
}


gather_df <- function(df) {
  # This function puts the df back into its original format of each row containing stats for both players
  df %>%
    arrange(Match_Id) %>%
    filter(row_number() %% 2 != 0) %>% # Filter for every 2nd row, starting at the 1st index. e.g. 1, 3, 5
    rename_at( # Rename columns to player_1
      vars(contains("Player")),
      ~str_replace(., "Player", "player_1")
    ) %>%
    inner_join(df %>%
                 filter(row_number() %% 2 == 0) %>%
                 rename_at(
                   vars(contains("Player")), # Rename columns to player_2
                   ~str_replace(., "Player", "player_2")
                 ) %>%
                 select(Match_Id, contains("Player")),
               by=c('Match_Id')
    ) %>%
    select(Match_Id, player_1, player_2, Winner, everything()) %>%
    return()
}


add_ratio_features <- function(df) {
  # This function adds ratio features to the appended df
  df %>%
    mutate(
      F_Player_Serve_Win_Ratio = (Player_FirstServes_Won + Player_SecondServes_Won - Player_DoubleFaults) / 
        (Player_FirstServes_In + Player_SecondServes_In + Player_DoubleFaults), # Point Win ratio when serving
      F_Player_Return_Win_Ratio = Player_ReturnPoints_Won / Player_ReturnPoints_Faced, # Point win ratio when returning
      F_Player_BreakPoints_Per_Game = Player_BreakPoints / Total_Games, # Breakpoints per receiving game
      F_Player_Game_Win_Percentage = Player_Games_Won / Total_Games
    ) %>%
    mutate_at(
      vars(colnames(.), -contains("Rank"), -Tournament_Date),
      ~ifelse(is.na(.), 0, .)
    ) %>%
    return()
}


clean_missing_data = function(df) {
  # This function cleans missing data
  df %>%
    mutate(
      Winner_ReturnPoints_Faced = ifelse(is.na(Winner_ReturnPoints_Faced), Loser_FirstServes_In + Loser_SecondServes_In, Winner_ReturnPoints_Faced),
      Winner_ReturnPoints_Won = ifelse(is.na(Winner_ReturnPoints_Won), Winner_ReturnPoints_Faced - Loser_FirstServes_Won - Loser_SecondServes_Won, Winner_ReturnPoints_Won),
      Loser_ReturnPoints_Faced = ifelse(is.na(Loser_ReturnPoints_Faced), Winner_FirstServes_In + Winner_SecondServes_In, Winner_ReturnPoints_Faced),
      Loser_ReturnPoints_Won = ifelse(is.na(Loser_ReturnPoints_Won), Loser_ReturnPoints_Faced - Winner_FirstServes_Won - Winner_SecondServes_Won, Loser_ReturnPoints_Won),
      Winner_Aces = ifelse(is.na(Winner_Aces), mean(Winner_Aces, na.rm = TRUE), Winner_Aces),
      Loser_Aces = ifelse(is.na(Loser_Aces), mean(Loser_Aces, na.rm = TRUE), Loser_Aces),
      Winner_DoubleFaults = ifelse(is.na(Winner_DoubleFaults), mean(Winner_DoubleFaults, na.rm = TRUE), Winner_DoubleFaults),
      Loser_DoubleFaults = ifelse(is.na(Loser_DoubleFaults), mean(Loser_DoubleFaults, na.rm = TRUE), Loser_DoubleFaults),
      Winner_Rank = ifelse(is.na(Winner_Rank), 999, Winner_Rank),
      Loser_Rank = ifelse(is.na(Loser_Rank), 999, Loser_Rank)
    ) %>%
    mutate_at(
      vars(-contains("Rank"), -"Tournament_Date"),
      ~ifelse(is.na(.), 0, .)
    )
  
}


extract_latest_features_for_tournament = function(df, dte) {
  # This function extracts the latest features for tournaments (features before the tournament)
  df %>%
    filter(Tournament_Date == dte, Round_Description == "First Round", Tournament_Date != "2012-01-16") %>%
    group_by(Player) %>%
    select_at(
      vars(Match_Id, starts_with("F_"), Player_Rank)
    ) %>%
    rename(F_Player_Rank = Player_Rank) %>%
    ungroup() %>%
    mutate(Feature_Date = dte) %>%
    select(Player, Feature_Date, everything())
  
}


##############################
#### Create training data ####
##############################

# Read in men and womens data; randomise the data to avoid result leakage
mens = readr::read_csv('data/Final data/ATP_matches_final.csv', na = ".") %>%
  filter(Court_Surface == "Hard" | Court_Surface == "Indoor Hard") %>%
  mutate(Match_Id = row_number(), # Add a match ID column to be used as a key
         Tournament_Date = dmy(Tournament_Date), # Change Tournament to datetime
         Total_Games = Winner_Games_Won + Loser_Games_Won) %>% # Add a total games played column
  split_winner_loser_columns() %>% # Change the dataframe from wide to long
  add_ratio_features() %>% # Add features
  group_by(Player) %>%
  mutate_at( # Create a rolling mean with window 15 for each player. If the player hasn't played 15 games, use a cumulative mean
    vars(starts_with("F_")),
    ~coalesce(rollmean(., k = 15, align = "right", fill = NA_real_), cummean(.)) %>% lag()
  ) %>%
  ungroup()

womens = readr::read_csv('data/Final data/WTA_matches_final.csv', na = ".") %>%
  filter(Court_Surface == "Hard" | Court_Surface == "Indoor Hard") %>%
  mutate(Match_Id = row_number(), # Add a match ID column to be used as a key
         Tournament_Date = dmy(Tournament_Date), # Change Tournament to datetime
         Total_Games = Winner_Games_Won + Loser_Games_Won) %>% # Add a total games played column
  split_winner_loser_columns() %>% # Change the dataframe from wide to long
  add_ratio_features() %>% # Add features
  group_by(Player) %>%
  mutate_at( # Create a rolling mean with window 15 for each player. If the player hasn't played 15 games, use a cumulative mean
    vars(starts_with("F_")),
    ~coalesce(rollmean(., k = 15, align = "right", fill = NA_real_), cummean(.)) %>% lag()
  ) %>%
  ungroup()


# Create a df of only aus open and us open results for both mens and womens results
aus_us_open_results_mens = 
  mens %>%
  filter((Tournament == "Australian Open, Melbourne" | Tournament == "U.S. Open, New York") 
         & Round_Description != "Qualifying" & Tournament_Date != "2012-01-16") %>%
  select(Match_Id, Player, Tournament, Tournament_Date, Round_Description, Winner)

aus_us_open_results_womens = 
  womens %>%
  filter((Tournament == "Australian Open, Melbourne" | Tournament == "U.S. Open, New York") 
         & Round_Description != "Qualifying" & Tournament_Date != "2012-01-16") %>%
  select(Match_Id, Player, Tournament, Tournament_Date, Round_Description, Winner)


# Convert the feature matrix to long format
feature_matrix_long_mens = 
  aus_us_open_results_mens %>%
  distinct(Tournament_Date) %>%
  pull() %>%
  map_dfr(
    ~extract_latest_features_for_tournament(mens, .)
  ) %>%
  filter(Feature_Date != "2012-01-16") %>% # Filter out the first Aus open as we don't have enough data before it
  mutate_at( # Replace NAs with the mean
    vars(starts_with("F_")),
    ~ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )

feature_matrix_long_womens = 
  aus_us_open_results_womens %>%
  distinct(Tournament_Date) %>%
  pull() %>%
  map_dfr(
    ~extract_latest_features_for_tournament(womens, .)
  ) %>%
  filter(Feature_Date != "2012-01-16") %>% # Filter out the first Aus open as we don't have enough data before it
  mutate_at( # Replace NAs with the mean
    vars(starts_with("F_")),
    ~ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )

# Joining results to features
feature_matrix_wide_mens = aus_us_open_results_mens %>%
  inner_join(feature_matrix_long_mens %>% 
               select(-Match_Id), 
             by = c("Player", "Tournament_Date" = "Feature_Date")) %>%
  gather_df() %>%
  mutate(
    F_Serve_Win_Ratio_Diff = F_player_1_Serve_Win_Ratio - F_player_2_Serve_Win_Ratio,
    F_Return_Win_Ratio_Diff = F_player_1_Return_Win_Ratio - F_player_2_Return_Win_Ratio,
    F_Game_Win_Percentage_Diff = F_player_1_Game_Win_Percentage - F_player_2_Game_Win_Percentage,
    F_BreakPoints_Per_Game_Diff = F_player_1_BreakPoints_Per_Game - F_player_2_BreakPoints_Per_Game,
    F_Rank_Diff = (F_player_1_Rank - F_player_2_Rank),
    Winner = as.factor(Winner)
  ) %>%
  select(Match_Id, player_1, player_2, Tournament, Tournament_Date, Round_Description, Winner, everything())

feature_matrix_wide_womens = aus_us_open_results_womens %>%
  inner_join(feature_matrix_long_womens %>% 
               select(-Match_Id), 
             by = c("Player", "Tournament_Date" = "Feature_Date")) %>%
  gather_df() %>%
  mutate(
    F_Serve_Win_Ratio_Diff = F_player_1_Serve_Win_Ratio - F_player_2_Serve_Win_Ratio,
    F_Return_Win_Ratio_Diff = F_player_1_Return_Win_Ratio - F_player_2_Return_Win_Ratio,
    F_Game_Win_Percentage_Diff = F_player_1_Game_Win_Percentage - F_player_2_Game_Win_Percentage,
    F_BreakPoints_Per_Game_Diff = F_player_1_BreakPoints_Per_Game - F_player_2_BreakPoints_Per_Game,
    F_Rank_Diff = (F_player_1_Rank - F_player_2_Rank),
    Winner = as.factor(Winner)
  ) %>%
  select(Match_Id, player_1, player_2, Tournament, Tournament_Date, Round_Description, Winner, everything())


###################################
#### Create 2019 features data ####
###################################

# Let's create features for both men and women using the past 15 games that they have played

# Get the last 15 games played for each unique player
unique_players_mens = read_csv('data/Final data/men_final_submission_file.csv') %>% pull(player_1) %>% unique()
unique_players_womens = read_csv('data/Final data/women_final_submission_file.csv') %>% pull(player_1) %>% unique()

# Create a feature table for both mens and womens
lookup_feature_table_mens = read_csv('data/Final data/ATP_matches_final.csv', na = ".") %>%
  filter(Court_Surface == "Hard" | Court_Surface == "Indoor Hard") %>%
  mutate(Match_Id = row_number(), # Add a match ID column to be used as a key
         Tournament_Date = dmy(Tournament_Date), # Change Tournament to datetime
         Total_Games = Winner_Games_Won + Loser_Games_Won) %>% # Add a total games played column
  split_winner_loser_columns() %>% # Change the dataframe from wide to long
  add_ratio_features() %>%
  filter(Player %in% unique_players_mens) %>%
  group_by(Player) %>%
  top_n(15, Match_Id) %>%
  summarise(
    F_Player_Serve_Win_Ratio = mean(F_Player_Serve_Win_Ratio),
    F_Player_Return_Win_Ratio = mean(F_Player_Return_Win_Ratio),
    F_Player_BreakPoints_Per_Game = mean(F_Player_BreakPoints_Per_Game),
    F_Player_Game_Win_Percentage = mean(F_Player_Game_Win_Percentage),
    F_Player_Rank = last(Player_Rank)
  )

# Create a feature table for both mens and womens
lookup_feature_table_womens = read_csv('data/Final data/WTA_matches_final.csv', na = ".") %>%
  filter(Court_Surface == "Hard" | Court_Surface == "Indoor Hard") %>%
  mutate(Match_Id = row_number(), # Add a match ID column to be used as a key
         Tournament_Date = dmy(Tournament_Date), # Change Tournament to datetime
         Total_Games = Winner_Games_Won + Loser_Games_Won) %>% # Add a total games played column
  split_winner_loser_columns() %>% # Change the dataframe from wide to long
  add_ratio_features() %>%
  filter(Player %in% unique_players_womens) %>%
  group_by(Player) %>%
  top_n(15, Match_Id) %>%
  summarise(
    F_Player_Serve_Win_Ratio = mean(F_Player_Serve_Win_Ratio),
    F_Player_Return_Win_Ratio = mean(F_Player_Return_Win_Ratio),
    F_Player_BreakPoints_Per_Game = mean(F_Player_BreakPoints_Per_Game),
    F_Player_Game_Win_Percentage = mean(F_Player_Game_Win_Percentage),
    F_Player_Rank = last(Player_Rank)
  )


# Create a feature matrix for all the player_1s by joining to the lookup feature table on name
draw_player_1_mens = read_csv('data/Final data/men_final_submission_file.csv') %>%
  select(player_1) %>%
  left_join(lookup_feature_table_mens, by=c("player_1" = "Player")) %>%
  rename(F_player_1_Serve_Win_Ratio = F_Player_Serve_Win_Ratio,
         F_player_1_Return_Win_Ratio = F_Player_Return_Win_Ratio,
         F_player_1_BreakPoints_Per_Game = F_Player_BreakPoints_Per_Game,
         F_player_1_Game_Win_Percentage = F_Player_Game_Win_Percentage,
         F_player_1_Rank = F_Player_Rank)

draw_player_1_womens = read_csv('data/Final data/women_final_submission_file.csv') %>%
  select(player_1) %>%
  left_join(lookup_feature_table_womens, by=c("player_1" = "Player")) %>%
  rename(F_player_1_Serve_Win_Ratio = F_Player_Serve_Win_Ratio,
         F_player_1_Return_Win_Ratio = F_Player_Return_Win_Ratio,
         F_player_1_BreakPoints_Per_Game = F_Player_BreakPoints_Per_Game,
         F_player_1_Game_Win_Percentage = F_Player_Game_Win_Percentage,
         F_player_1_Rank = F_Player_Rank)

# Create a feature matrix for all the player_2s by joining to the lookup feature table on name
# draw_player_2_mens = read_csv('data/Final data/men_final_submission_file.csv') 

# men_extra <- data.frame(player_1 = as.character(), player_2 = as.character(), player_1_win_probability = as.character())
# player_1 <- draw_player_2_mens$player_2 %>% unique()
# player_2 <- "Zhe Li" %>% rep(127)
# player_1_win_probability <- 0.5 %>% rep(127)
# 
# men_extra <- data.frame(cbind(player_1, player_2, player_1_win_probability))
# men_extra$player_1 <- as.character(men_extra$player_1)
# men_extra$player_2 <- as.character(men_extra$player_2)
# men_extra$player_1_win_probability <- as.numeric(as.character(men_extra$player_1_win_probability))
# 
# draw_player_2_mens <- cbind(draw_player_2_mens, men_extra)

draw_player_2_mens <- read_csv('data/Final data/men_final_submission_file.csv') %>%
  select(player_2) %>%
  left_join(lookup_feature_table_mens, by=c("player_2" = "Player")) %>%
  rename(F_player_2_Serve_Win_Ratio = F_Player_Serve_Win_Ratio,
         F_player_2_Return_Win_Ratio = F_Player_Return_Win_Ratio,
         F_player_2_BreakPoints_Per_Game = F_Player_BreakPoints_Per_Game,
         F_player_2_Game_Win_Percentage = F_Player_Game_Win_Percentage,
         F_player_2_Rank = F_Player_Rank)

draw_player_2_womens = read_csv('data/Final data/women_final_submission_file.csv') %>%
  select(player_2) %>%
  left_join(lookup_feature_table_womens, by=c("player_2" = "Player")) %>%
  rename(F_player_2_Serve_Win_Ratio = F_Player_Serve_Win_Ratio,
         F_player_2_Return_Win_Ratio = F_Player_Return_Win_Ratio,
         F_player_2_BreakPoints_Per_Game = F_Player_BreakPoints_Per_Game,
         F_player_2_Game_Win_Percentage = F_Player_Game_Win_Percentage,
         F_player_2_Rank = F_Player_Rank)

# Bind the two dfs together and only select the players' names and features (which start with 'F_' for simplicity)
aus_open_2019_features_mens = draw_player_1_mens %>% 
  bind_cols(draw_player_2_mens) %>%
  select(player_1, player_2, everything()) %>%
  mutate(
    F_Serve_Win_Ratio_Diff = F_player_1_Serve_Win_Ratio - F_player_2_Serve_Win_Ratio,
    F_Return_Win_Ratio_Diff = F_player_1_Return_Win_Ratio - F_player_2_Return_Win_Ratio,
    F_Game_Win_Percentage_Diff = F_player_1_Game_Win_Percentage - F_player_2_Game_Win_Percentage,
    F_BreakPoints_Per_Game_Diff = F_player_1_BreakPoints_Per_Game - F_player_2_BreakPoints_Per_Game,
    F_Rank_Diff = (F_player_1_Rank - F_player_2_Rank)
  ) 

# remove Steve Darcis from the main df
steve_darcis1 <- aus_open_2019_features_mens %>%
  filter(player_1 == "Steve Darcis" | player_2 == "Steve Darcis")

steve_darcis1 <- steve_darcis1 %>% select(player_1, player_2)
steve_darcis1 <- steve_darcis1 %>% mutate(player_1_win_probability = ifelse(player_1 == "Steve Darcis", 0.05, 0.95))

aus_open_2019_features_mens <- aus_open_2019_features_mens %>%
  filter(player_1 != "Steve Darcis") %>%
  filter(player_2 != "Steve Darcis")



aus_open_2019_features_womens = draw_player_1_womens %>% 
  bind_cols(draw_player_2_womens) %>%
  select(player_1, player_2, everything()) %>%
  mutate(
    F_Serve_Win_Ratio_Diff = F_player_1_Serve_Win_Ratio - F_player_2_Serve_Win_Ratio,
    F_Return_Win_Ratio_Diff = F_player_1_Return_Win_Ratio - F_player_2_Return_Win_Ratio,
    F_Game_Win_Percentage_Diff = F_player_1_Game_Win_Percentage - F_player_2_Game_Win_Percentage,
    F_BreakPoints_Per_Game_Diff = F_player_1_BreakPoints_Per_Game - F_player_2_BreakPoints_Per_Game,
    F_Rank_Diff = (F_player_1_Rank - F_player_2_Rank)
  ) 


# remove Clara Burel from the main women's df
Clara_Burel <- aus_open_2019_features_womens %>%
  filter(player_1 == "Clara Burel" | player_2 == "Clara Burel")

Clara_Burel <- Clara_Burel %>% select(player_1, player_2)
Clara_Burel <- Clara_Burel %>% mutate(player_1_win_probability = ifelse(player_1 == "Clara Burel", 0.05, 0.95))


aus_open_2019_features_womens <- aus_open_2019_features_womens %>%
  filter(player_1 != "Clara Burel") %>%
  filter(player_2 != "Clara Burel")


set.seed(183)
dtest <- aus_open_2019_features_mens %>% select(-player_1, -player_2) %>% as.matrix() %>% xgb.DMatrix

X <- feature_matrix_wide_mens %>% select(-Match_Id, - player_1, -player_2, -Tournament, -Tournament_Date, -Round_Description, -Winner) %>% as.matrix()
y_var <- as.numeric(as.character(feature_matrix_wide_mens$Winner))

val <- caret::createDataPartition(y_var, p = 0.3, list = F) %>% c()

dtrain <- xgb.DMatrix(data = X[-val, ], label = y_var[-val])
dval <- xgb.DMatrix(data = X[val, ], label = y_var[val])
cols <- colnames(X)


p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "logloss",
          nthread = 4,
          eta = 0.05,
          max_depth = 6,
          min_child_weight = 30,
          gamma = 0,
          subsample = 0.85,
          colsample_bytree = 0.7,
          colsample_bylevel = 0.632,
          alpha = 0,
          lambda = 0,
          nrounds = 2000)

m_xgb <- xgb.train(p, dtrain, 1000, list(val = dval), maximize = FALSE,
                   print_every_n = 50, early_stopping_rounds = 30)

importance_matrix <- xgb.importance(colnames(X), model = m_xgb)

xgb.plot.importance(importance_matrix[1:30])


player_1_win_probability <- predict(m_xgb, dtest)


# Create submission df
mens_submission = aus_open_2019_features_mens %>%
  select(player_1,
         player_2)

mens_submission <- cbind(mens_submission, player_1_win_probability) %>% bind_rows(steve_darcis1)

mens_h20 <- read_csv("submission/datathon_submission_mens_495.csv")

ens_sub_male <- mens_h20 %>%
  left_join(mens_submission, by = c("player_1", "player_2"))
  
ens_sub_male <- ens_sub_male %>% mutate(ens_pred = (player_1_win_probability.x * .7) + (player_1_win_probability.y * .3))

ens_sub_male <- ens_sub_male[, c(1,2,5)]
ens_sub_male <- ens_sub_male %>% rename(player_1_win_probability = ens_pred)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Women's model
#~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(183)
dtest <- aus_open_2019_features_womens %>% select(-player_1, -player_2) %>% as.matrix() %>% xgb.DMatrix

X <- feature_matrix_wide_womens %>% select(-Match_Id, - player_1, -player_2, -Tournament, -Tournament_Date, -Round_Description, -Winner) %>% as.matrix()
y_var <- as.numeric(as.character(feature_matrix_wide_womens$Winner))

val <- caret::createDataPartition(y_var, p = 0.3, list = F) %>% c()

dtrain <- xgb.DMatrix(data = X[-val, ], label = y_var[-val])
dval <- xgb.DMatrix(data = X[val, ], label = y_var[val])
cols <- colnames(X)


p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "logloss",
          nthread = 4,
          eta = 0.05,
          max_depth = 6,
          min_child_weight = 30,
          gamma = 0,
          subsample = 0.85,
          colsample_bytree = 0.7,
          colsample_bylevel = 0.632,
          alpha = 0,
          lambda = 0,
          nrounds = 2000)

m_xgb <- xgb.train(p, dtrain, 1000, list(val = dval), maximize = FALSE,
                   print_every_n = 50, early_stopping_rounds = 30)

importance_matrix <- xgb.importance(colnames(X), model = m_xgb)

xgb.plot.importance(importance_matrix[1:30])


player_1_win_probability <- predict(m_xgb, dtest)


# Create submission df
womens_submission = aus_open_2019_features_womens %>%
  select(player_1,
         player_2)

womens_submission <- cbind(womens_submission, player_1_win_probability) %>% bind_rows(Clara_Burel)

womens_h20 <- read_csv("submission/datathon_submission_womens_495.csv")

ens_sub_female <- womens_h20 %>%
  left_join(womens_submission, by = c("player_1", "player_2"))

ens_sub_female <- ens_sub_female %>% mutate(ens_pred = (player_1_win_probability.x * .7) + (player_1_win_probability.y * .3))

ens_sub_female <- ens_sub_female[, c(1,2,5)]
ens_sub_female <- ens_sub_female %>% rename(player_1_win_probability = ens_pred)


# Export to CSV - this is the submission file!
ens_sub_male %>% write_csv("submission/datathon_submission_mens_495.csv")
ens_sub_female %>% write_csv("submission/datathon_submission_womens_495.csv")





# load library
library(tidyverse)

# load in my submission file
mens <- read_csv("submission/datathon_submission_mens_495.csv")
womens <- read_csv("submission/datathon_submission_womens_495.csv")

# create a vector of player names for both men and women
men_players <- c(mens$player_1, mens$player_2) %>% unique()
women_players <- c(womens$player_1, womens$player_2) %>% unique()

# define a function that takes part of a players name (usually a surname would be good), and whether if comes from men or women players
get_player_name <- function(gender, surname) {
  gender[str_detect(gender, surname)] %>% return()
}

# call the function to get the players full name
m1 <- get_player_name(men_players, "Copil")
m2 <- get_player_name(men_players, "Granollers")



# define a function to return who player 1 is and what the prediction was for them to win
check_predition <- function(df, one_player, second_player) {
  df %>% 
    filter(player_1 %in% c(one_player, second_player), player_2 %in% c(one_player, second_player)) %>%
    select(player_1, player_1_win_probability) %>%
    return()
}

# call the function to get the probability of player 1 winning
f1 <- get_player_name(women_players, "")
f2 <- get_player_name(women_players, "")

check_predition(womens, f1, f2)


m1 <- get_player_name(men_players, "Jarry")
m2 <- get_player_name(men_players, "Mayer")[1]

check_predition(mens, m1, m2)


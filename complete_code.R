## Install packages if necessary ----
#install.packages("baseballr")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("tidyr")
#install.packages("tibble")
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("tokenizers")
#install.packages("readr")
#install.packages("data.table")
#install.packages("progress")
#install.packages("gdata")




## Load packages ----
library(baseballr)
library(lubridate)
library(tidyr)
library(tibble)
library(tidyverse)
library(stringr)
library(tokenizers)
library(readr)
library(data.table)
library(progress)
library(gdata)


## Create play-by-play data frame ----

# Set j as the date on which games start. In this case, it is set as 
# Triple-A opening day
j <- as.Date("2022-04-05")

# Create necessary data frames
single_game_id <- data.frame()
single_game_data <- data.frame()
single_day_pitch_data <- data.frame()
total_plays <- data.frame()

# Creates play-by-play data frame
repeat {
  
  # Keep track of progress while the code runs
  print(paste(j, "Top of loop"))
  
  # Skips Monday (Monday's are MiLB off-days)
  if(weekdays.Date(j) == "Monday"){
    print(paste(j, "Is Monday"))
    j <- j + 1
  }
  
  # Gets the game ids for the given date
  id <- mlb_game_pks(j, level_ids = c(11))
  game_id <- as.data.frame(id$game_pk) 
  
  i <- 1 
  for (i in 1:nrow(game_id)) {
    try( {
      
      # Resets the single_game_id variable for every different game
      single_game_id <- id$game_pk[i]
      
      # Gets play-by-play data for a single game on a single day
      single_game_data <- mlb_pbp(single_game_id)
      
      # Play-by-play data for an entire day
      single_day_pitch_data <- rbind(single_day_pitch_data, single_game_data, fill = TRUE) 
      
      # Creates inning.description to delete pitch-by-pitch data, keeping only 
      # One pitch per at-bat
      single_day_pitch_data$inning.description <- paste(single_day_pitch_data$about.inning, single_day_pitch_data$result.description)
      cleaned_day_plays <- distinct(single_day_pitch_data, inning.description, .keep_all = TRUE)
      
    }, TRUE
    )
  }
  
  # Goes to the next day
  j <- j + 1
  
  # Stops when the current date is reached
  if (j == Sys.Date()) { 
    print(paste(j, "At breaking point"))
    break
  } 
}

# Clean environment
keep(cleaned_day_plays, sure = TRUE)

# Create csv file for optional use later
fwrite(cleaned_day_plays, "cleaned_plays.csv")

## index plays data frame ----

# Load in play-by-play data (if you have it in a csv), created indexed version 
# Of data frame
indexed_plays <- read.csv("cleaned_plays.csv")
#indexed_plays <- cleaned_day_plays
indexed_plays <- subset(indexed_plays, result.description != "TRUE")
indexed_plays$index_number <- row.names(indexed_plays)

# Chronologically sorted play-by-play data
final_plays <- indexed_plays[order(as.numeric(indexed_plays$index_number), decreasing = TRUE), ]

#Clean environment
keep(final_plays, sure = TRUE)

## create scoring log ----

# Create original score variables
original_away_score <- rep(NA, nrow(final_plays))
original_home_score <- rep(NA, nrow(final_plays))

# Creates accurate log of the score before each play
i <- 2
repeat{
  
  # Set the pre-play score equal to the post-play score of the previous play
  original_away_score[i] <- final_plays$result.awayScore[i-1]
  original_home_score[i] <- final_plays$result.homeScore[i-1]
  
  # If there is a new game, set both scores equal to zero
  if (isTRUE(final_plays$game_pk[i] != final_plays$game_pk[i - 1])){
    original_away_score[i] <- 0
    original_home_score[i] <- 0
  }
  
  # Stop the repeat when finished
  if (i == nrow(final_plays)){
    break
  }
  i <- i + 1
}

# Update play-by-play data to include the score before every play
final_plays %>% 
  mutate(details.awayScore = original_away_score, 
         details.homeScore = original_home_score) -> final_plays
final_plays$details.awayScore[1] <-  0 
final_plays$details.homeScore[1] <-  0

# Clean environment
keep(final_plays, sure = TRUE)

## create log of runners on base before the play ----

# Create necessary variables
matchup.preOnFirst.id <- rep(NA, nrow(final_plays))
matchup.preOnSecond.id <- rep(NA, nrow(final_plays))
matchup.preOnThird.id <- rep(NA, nrow(final_plays))
matchup.preOnFirst.name <- rep(NA, nrow(final_plays))
matchup.preOnSecond.name <- rep(NA, nrow(final_plays))
matchup.preOnThird.name <- rep(NA, nrow(final_plays))

# Create log player names and ids before each play
i <- 2
repeat{
  
  # Set the pre-play names and ids equal to the names equal to the post-play 
  # Names and ids of the previous play
  matchup.preOnFirst.id[i] <- final_plays$matchup.postOnFirst.id[i-1]
  matchup.preOnSecond.id[i] <- final_plays$matchup.postOnSecond.id[i-1]
  matchup.preOnThird.id[i] <- final_plays$matchup.postOnThird.id[i-1]
  matchup.preOnFirst.name[i] <- final_plays$matchup.postOnFirst.fullName[i-1]
  matchup.preOnSecond.name[i] <- final_plays$matchup.postOnSecond.fullName[i-1]
  matchup.preOnThird.name[i] <- final_plays$matchup.postOnThird.fullName[i-1]
  
  # If there is a new game, everything resets
  if (final_plays$game_pk[i] != final_plays$game_pk[i - 1]){
    matchup.preOnFirst.id[i] <- NA
    matchup.preOnSecond.id[i] <- NA
    matchup.preOnThird.id[i] <- NA
    matchup.preOnFirst.name[i] <- NA
    matchup.preOnSecond.name[i] <- NA
    matchup.preOnThird.name[i] <- NA
  }
  
  # If there is a new inning, everything resets
  if (final_plays$about.halfInning[i] != final_plays$about.halfInning[i - 1]){
    matchup.preOnFirst.id[i] <- NA
    matchup.preOnSecond.id[i] <- NA
    matchup.preOnThird.id[i] <- NA
    matchup.preOnFirst.name[i] <- NA
    matchup.preOnSecond.name[i] <- NA
    matchup.preOnThird.name[i] <- NA
  }
  
  # Stop the repeat when finished
  if (i == nrow(final_plays)){
    break
  }
  i <- i + 1
}

# Update final plays data frame to include pre-play baserunners
final_plays %>% 
  mutate(matchup.preOnFirst = matchup.preOnFirst.id, 
         matchup.preOnSecond = matchup.preOnSecond.id,
         matchup.preOnThird = matchup.preOnThird.id,
         matchup.preOnFirst.name = matchup.preOnFirst.name,
         matchup.preOnSecond.name = matchup.preOnSecond.name,
         matchup.preOnThird.name = matchup.preOnThird.name) -> final_plays

# Clean environment
keep(final_plays, sure = TRUE)

# There is an issue with the way some games are logged where the baserunners 
# Are displayed out of chronological order. This deletes those games
i <- 1 
repeat{ 
  if (isTRUE(final_plays$matchup.postOnFirst.id[i] == final_plays$matchup.batter.id[i + 1])) {
    final_plays <- final_plays %>% 
      filter(game_pk != game_pk[i])
  }
  if (i == nrow(final_plays)){
    break
  }
  i <- i + 1
}

# Clean environment
rm(i)


## create a log of batter and baserunner destinations for each play ----

# Create necessary variables
bat_d <- rep(NA, nrow(final_plays))
run1_d <- rep(NA, nrow(final_plays))
run2_d <- rep(NA, nrow(final_plays))
run3_d <- rep(NA, nrow(final_plays))

# Log batter destinations
for (i in 1:nrow(final_plays)) {
  
  # If the batter id equals the post-play id of a runner on a certain base, 
  # The batter destination is that base
  if (isTRUE(final_plays$matchup.batter.id[i] == final_plays$matchup.postOnFirst.id[i])) { 
    bat_d[i] <- 1}
  if (isTRUE(final_plays$matchup.batter.id[i] == final_plays$matchup.postOnSecond.id[i])) { 
    bat_d[i] <- 2}
  if (isTRUE(final_plays$matchup.batter.id[i] == final_plays$matchup.postOnThird.id[i])) { 
    bat_d[i] <- 3}
  
  # If the batter homers, their destination will be home
  if (isTRUE(grepl("homers", final_plays$result.description[i]))) {
    bat_d[i] <- 4
  }
}



# Split the result description by period, then search for "scores" to find 
# The runners who scored
scoring <- str_split(final_plays$result.description, ".   ", simplify = TRUE)
scoring <- sub("[0-9.]+$", "", scoring)

# Create a data frame of what runners scored on each play
i <- 1
repeat {
  for (j in 1:ncol(scoring)){
    
    # Get rid of plays in which no one scores
    if (isFALSE(grepl("scores", scoring[i, j]))){
      scoring[i, j] <-  NA
    }
    
    # If it is a scoring play, delete everything except for the player's name
    if (isTRUE(grepl("scores", scoring[i, j]))){
      scoring[i, j] <- gsub(" scores", "", scoring[i, j])
      scoring[i, j] <- gsub("scores", "", scoring[i, j])
    }
  }
  
  # Delete unnecessary space 
  scoring <- gsub(" ", "", scoring)
  if (i == nrow(scoring)){
    break
  }
  i <- i + 1
  
  # This step takes a while. This is a way of checking where one is at in the process
  print(i) 
}

# Delete unnecessary spaces
scoring <- gsub(" ", "", scoring)
final_plays$matchup.preOnFirst.name <- gsub(" ", "", final_plays$matchup.preOnFirst.name)
final_plays$matchup.preOnSecond.name <- gsub(" ", "", final_plays$matchup.preOnSecond.name)
final_plays$matchup.preOnThird.name <- gsub(" ", "", final_plays$matchup.preOnThird.name)

# Create final destination for runner on first
for (i in 1:nrow(final_plays)) {
  
  # If the runner on first id equals the post-play id of a runner on a certain base, 
  # The runner destination is that base
  if (isTRUE(final_plays$matchup.preOnFirst[i] == final_plays$matchup.postOnFirst.id[i])) { 
    run1_d[i] <- 1}
  if (isTRUE(final_plays$matchup.preOnFirst[i] == final_plays$matchup.postOnSecond.id[i])) { 
    run1_d[i] <- 2}
  if (isTRUE(final_plays$matchup.preOnFirst[i] == final_plays$matchup.postOnThird.id[i])) { 
    run1_d[i] <- 3}
  
  # If the runner on first scores, their destination will be home
  for (j in 1:ncol(scoring)) {
    if (isTRUE(final_plays$matchup.preOnFirst.name[i] == scoring[i, j])){
      run1_d[i] <- 4
    }
  }
}

# Create final destination for runner on second
for (i in 1:nrow(final_plays)) {
  
  # If the runner on second id equals the post-play id of a runner on a certain base, 
  # The runner destination is that base
  if (isTRUE(final_plays$matchup.preOnSecond[i] == final_plays$matchup.postOnSecond.id[i])) { 
    run2_d[i] <- 2}
  if (isTRUE(final_plays$matchup.preOnSecond[i] == final_plays$matchup.postOnThird.id[i])) { 
    run2_d[i] <- 3}
  
  # If the runner on second scores, their destination will be home
  for (j in 1:ncol(scoring)) {
    if (isTRUE(final_plays$matchup.preOnSecond.name[i] == scoring[i, j])){
      run2_d[i] <- 4
    }
  }
}

# Create final destination for runner on third
for (i in 1:nrow(final_plays)) {
  
  # If the runner on second id equals the post-play id of a runner on a certain base, 
  # The runner destination is that base
  if (isTRUE(final_plays$matchup.preOnThird[i] == final_plays$matchup.postOnThird.id[i])) { 
    run3_d[i] <- 3}
  
  # If the runner on third scores, their destination will be home
  for (j in 1:ncol(scoring)) {
    if (isTRUE(final_plays$matchup.preOnThird.name[i] == scoring[i, j])){
      run3_d[i] <- 4
    }
  }
}

# Add batter and baserunner destination to plays data frame
final_plays %>% 
  mutate(bat_dest = bat_d, run1_dest = run1_d, run2_dest = run2_d, 
         run3_dest = run3_d) -> final_plays

# Clean environment
keep(final_plays, sure = TRUE)

# Save csv of final plays
fwrite(final_plays, "final_plays.csv")


## Run expectancy matrix ----

# Create necessary data frames
re24_plays <- data_frame()
RUNS_1 <- data_frame()

# Set run-expectancy plays equal to previous data frame, or load in csv
re24_plays <- read.csv("final_plays.csv")
#re24_plays <- final_plays

# Set all NAs equal to 0, allows following code to run correctly
re24_plays[is.na(re24_plays)] <- 0

# Creates essential variables for the calculation of run-expectancy
re24_plays %>% 
  mutate(
    # RUNS equals the total number of runs in the game
    RUNS = details.homeScore + details.awayScore,
    
    # Create HALF.INNING so that the plays can be grouped by half inning 
    # later
    HALF.INNING = paste(game_pk, about.inning, about.halfInning),
    
    # OUTS.ON.PLAY is the number of outs recorded on the play
    OUTS.ON.PLAY = count.outs.end - count.outs.start,
    
    # Create RUNS.SCORED variable to track runs scored every play
    RUNS.SCORED = 
      (bat_dest > 3) + (run1_dest > 3) + 
      (run2_dest > 3) + (run3_dest > 3)) -> re24_plays


# Group by half inning (run expectancy matrices look at each half inning as 
# independent events)
re24_plays %>%
  group_by(HALF.INNING) %>%
  
  # Create crucial variables in the creation of the matrix
  summarize(Outs.Inning = sum(OUTS.ON.PLAY), 
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) -> half_innings

# Add half innings groups back to main data frame
# RUNS.ROI is the difference between the maximum runs scored in the inning 
# and the number of runs that have already been scored
re24_plays %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = abs(MAX.RUNS - RUNS)) -> re24_plays

# Creates the state (or situation) prior to the play
re24_plays %>% 
  
  # BASES represents which runners are currently on which base
  mutate(BASES = 
           paste(ifelse(matchup.preOnFirst > 0, 1, 0),
                 ifelse(matchup.preOnSecond > 0, 1, 0),
                 ifelse(matchup.preOnThird > 0, 1, 0), sep = ""),
         
         # STATE adds the number of outs to BASES
         STATE = paste(BASES, count.outs.start)) -> re24_plays

# Create the state (or situation) following  the play
re24_plays %>%
  
  # Find which runners end up where
  mutate(NRUNNER1 = 
           as.numeric(run1_dest == 1 | bat_dest == 1),
         NRUNNER2 = 
           as.numeric(run1_dest == 2 | run2_dest == 2 | 
                        bat_dest == 2),
         NRUNNER3 = 
           as.numeric(run1_dest == 3 | run2_dest == 3 |
                        run3_dest == 3 | bat_dest == 3),
         
         # Find the resulting outs on the play
         NOUTS = count.outs.start + OUTS.ON.PLAY,
         
         # Create new bases and state variables to reflect the results of the 
         # play
         NEW.BASES = paste(NRUNNER1, NRUNNER2, 
                           NRUNNER3, sep = ""),
         NEW.STATE = paste(NEW.BASES, NOUTS)) -> re24_plays

# Filter out plays that have the same state with no runs scored
re24_plays %>% 
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) -> re24_plays

# Filter out innings that end with less than three runs (these are innings 
# where walk-offs occur. In these instances, a team is playing to win the game 
# rather than maximize their runs, so the innings would not accurately reflect 
# run expectancy)
re24_plays %>%
  filter(Outs.Inning == 3) -> re24_plays_final

# Final calculations for run-expectancy matrix
re24_plays_final %>% 
  group_by(STATE) %>%
  
  # Each state recieves its corresponding run expectancy
  summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS_1 

# Create run expectancy matrix
re24 <- matrix(round(RUNS_1$Mean, 2), 8, 3)

# Give matrix appropriate row and column names
dimnames(re24)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(re24)[[1]] <- c("___", "__3", "_2_", "_23", 
                         "1__", "1_3", "12_", "123")

# Preparation work for finding run values
re24_plays %>%
  
  # Concentrate run values by state
  left_join(select(RUNS_1, -Outs), by = "STATE") %>%
  rename(Runs.State = Mean) %>%
  left_join(select(RUNS_1, -Outs), 
            by = c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = Mean) %>%
  replace_na(list(Runs.New.State = 0)) %>%
  
  # Find run value of each play
  mutate(run_value = Runs.New.State - Runs.State +
           RUNS.SCORED) -> re24_plays

# Clean environment
keep(re24_plays, re24, sure = TRUE)

## Run values ----

#OUTS
# Create necessary variable
outs <- rep(0, nrow(re24_plays))

# Find plays that resulted in outs
re24_plays %>% 
  filter(OUTS.ON.PLAY > 0) -> outs

# Find run value of outs
outs %>%
  summarize(mean_run_value = mean(run_value)) -> mean_outs

# HOME RUNS
# Create necessary variable
home_run <- rep(0, nrow(re24_plays))

# Find all instances of a home run
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("homers", re24_plays$result.description[i]))) {
    home_run[i] <- 1
  }
}

# Select plays with a home run
re24_plays %>% 
  mutate(home_runs = home_run) %>% 
  filter (home_runs == 1) -> home_runs

# Find run value of home run by finding the mean run values of all plays with 
# a home run
home_runs %>%
  summarize(mean_run_value = mean(run_value)) -> mean_hr

# Find the run value of a home run compared to that of an out
HR <- mean_hr - mean_outs


# SINGLES
# Create necessary variable
single <- rep(0, nrow(re24_plays))

# Find all instances of a single
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("singles", re24_plays$result.description[i]))) {
    single[i] <- 1
  }
}

# Select plays with a single
re24_plays %>% 
  mutate(singles = single) %>% 
  filter(singles == 1) -> singles

# Find run value of singles by finding the mean run values of all plays with
# a single
singles %>%
  summarize(mean_run_value = mean(run_value)) -> mean_singles

# Find the run value of a single to that of an out
SINGLE <- mean_singles - mean_outs


# DOUBLES
# Create necessary variable
double <- rep(0, nrow(re24_plays))

# Find all instances of a double
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("doubles", re24_plays$result.description[i]))) {
    double[i] <- 1
  }
}

# Select plays with a double
re24_plays %>% 
  mutate(doubles = double) %>% 
  filter(doubles == 1) -> doubles

# Find run value of doubles by finding the mean run values of all plays with
# a double
doubles %>%
  summarize(mean_run_value = mean(run_value)) -> mean_doubles

# Find the run value of a double to that of an out
DOUBLE <- mean_doubles - mean_outs


# TRIPLES
# Create necessary variable
triple <- rep(0, nrow(re24_plays))

# Find all instances of a triple
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("triples", re24_plays$result.description[i]))) {
    triple[i] <- 1
  }
}

# Select plays with a triple
re24_plays %>% 
  mutate(triples = triple) %>% 
  filter(triples == 1) -> triples

# Find run value of triples by finding the mean run values of all plays with
# a triple
triples %>%
  summarize(mean_run_value = mean(run_value)) -> mean_triples

# Find the run value of a triple to that of an out
TRIPLE <- mean_triples - mean_outs

# WALKS
# Create necessary variable
walk <- rep(0, nrow(re24_plays))

# Find all instances of a walk
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("walks", re24_plays$result.description[i]))) {
    walk[i] <- 1
    
  }
}

# Select plays with a walk
re24_plays %>% 
  mutate(walks = walk) %>% 
  filter(walks == 1) -> walks

# Find run value of walks by finding the mean run values of all plays with
# a walk
walks %>%
  summarize(mean_run_value = mean(run_value)) -> mean_walks

# Find the run value of a walk to that of an out
BB <- mean_walks - mean_outs


# HBP
# Create necessary variable
hit_by_pitch <- rep(0, nrow(re24_plays))

# Find all instances of a HBP
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("hit by pitch", re24_plays$result.description[i]))) {
    hit_by_pitch[i] <- 1
  }
}

# Select plays with a HBP
re24_plays %>% 
  mutate(hbp = hit_by_pitch) %>% 
  filter(hbp == 1) -> hbp

# Find run value of HBPs by finding the mean run values of all plays with
# a HBP
hbp %>%
  summarize(mean_run_value = mean(run_value)) -> mean_hbp

# Find the run value of a HBP to that of an out
HBP <- mean_hbp - mean_outs

## scaling run values------------------------------------------------------------------------
# Must find sacrifies flies and intentional walks for league OBP numbers
# Create necessary variable
sf <- rep(0, nrow(re24_plays))

# Find all instances of a sacrifice fly
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("sacrifice fly", re24_plays$result.description[i]))) {
    sf[i] <- 1
  }
}

# Select plays with a sacrifice fly
re24_plays %>% 
  mutate(sf = sf) -> re24_plays

# Create necessary variable
ibb <- rep(0, nrow(re24_plays))

# Find all instances of an intentional walk
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("intentionally walks", re24_plays$result.description[i]))) {
    ibb[i] <- 1
  }
}

# Select plays with an intentional walk
re24_plays %>% 
  mutate(ibb = ibb) -> re24_plays

# Clean environment

## linear weights -------
# Create "wOBA Multiplier" by calculating the league wOBA figure
woba_multiplier <- 
  (HBP*nrow(hbp) + BB*nrow(walks) + SINGLE*nrow(singles) + DOUBLE*nrow(doubles) 
   + TRIPLE*nrow(triples) + HR*nrow(home_runs))/(nrow(re24_plays) - sum(ibb) 
                                                 - sum(sf))
# Calculate the league OBP figure
league_obp <- 
  (nrow(hbp) + nrow(walks) + nrow(singles) + nrow(doubles) + nrow(triples) 
   + nrow(home_runs))/(nrow(re24_plays) - sum(ibb))

# Divide league OBP by league wOBA to get the "wOBA Scale"
woba_scale <- league_obp / woba_multiplier

# Multiply the play run values by the wOBA scale to obtain the weights used in
# the wOBA calculation
woba_weights <- c(woba_scale*HBP, woba_scale*BB, woba_scale*SINGLE, 
                  woba_scale*DOUBLE, woba_scale*TRIPLE, woba_scale*HR)

# Ensure the wOBA weights are the correct class
woba_weights <- as.numeric(woba_weights)

# Create an easily readable table containing the wOBA weights
linear_weights_table <- as.data.frame(woba_weights)

# Ensure the table is named correctly
rownames(linear_weights_table) <- c("HBP", "BB", "1B", "2B", "3B", "HR")
colnames(linear_weights_table) <- c("Weights")

# Clean environment
keep(linear_weights_table, re24, sure = TRUE)

# View the final product
View(linear_weights_table)

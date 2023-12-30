setwd("/home/joker/Fantasy_Football_Analysis")
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
source("./functions.R")

# LeagueID for API calls
leagueID <- "969743510867947520" # Revert Anything

# Easy Export for Dynasty Ranking values by players
# https://www.dynastysuperflex.com/ktc_values

################################################################################

# Round by round evaluation of the draft VBD
draft_data <- sleeper_api_call(leagueID, "/draft/969743511543201792/picks", override_url = TRUE)
full_name <- as.data.frame(paste(draft_data$metadata$first_name, draft_data$metadata$last_name))
draft_data <- cbind(full_name, draft_data)
colnames(draft_data)[1] <- 'full_name'
draft_data$full_name <- gsub("\\s+", "", draft_data$full_name)

draft_data_subs <- draft_data[,c(1:3,6)]

# Read in superflex dynasty draft rankings data and cleanup player names to "First Last"
dynasty_rankings <- read.csv("./Superflex.csv")
dynasty_rankings$Player <- gsub("(^[^(]+).*", "\\1", dynasty_rankings$Player)
dynasty_rankings$Player <- gsub("\\s+", "", dynasty_rankings$Player)

draft_table <- merge(draft_data_subs, dynasty_rankings, by.x = 'full_name', by.y = 'Player')
draft_table <- arrange(draft_table, draft_table$pick_no)

# Evaluate the roster value post draft of each roster
value_roster <- data.frame(
  roster_id = numeric(0),
  roster_value = numeric(0)
)
for (i in unique(draft_table$roster_id)) {
  data <- subset(draft_table, roster_id == i)
  draft_value <- sum(data$Value)
  curr_ros_val <- as.data.frame(t(data.frame(c(i,draft_value))))
  value_roster <- rbind(value_roster, curr_ros_val)
}
colnames(value_roster) <- c('roster_id', 'value')
value_roster <- arrange(value_roster, value_roster$roster_id)

# Difference in value compared to median roster value
value_roster <- cbind(value_roster, value_roster$value-median(value_roster$value))
colnames(value_roster)[3] <- "value_diff"

# Merge table to convert roster_id to display_name
rosters_data <- sleeper_api_call(leagueID, "/rosters")
users_data <- sleeper_api_call(leagueID, "/users")
rosid_dispname <- merge(rosters_data, users_data, by.x = 'owner_id', by.y = 'user_id')
rosid_dispname_merge_table <- select(rosid_dispname, c(roster_id, display_name))

# Final team value data
value_team <- merge(rosid_dispname_merge_table, value_roster, by.x = 'roster_id', by.y = 'roster_id')

value_plot <- ggplot(value_team, aes(x=display_name, y=value_diff)) + 
  geom_bar(stat = "identity", width=0.2) +
  xlab("Team") +
  ylab("Value - Median Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(value_plot)

################################################################################

# Draft simulator

# Initialized dataframe to hold rosters for simulated rosters
sim_roster <- data.frame(
  Player = character(0),
  Position = character(0),
  Team = character(0),
  Age = numeric(0),
  Value = numeric(0),
  Position.1 = character(0),
  Rank.Type = character(0)
)

# Initializing team specific dataframes for simulations
team_1 <- sim_roster
team_2 <- sim_roster
team_3 <- sim_roster
team_4 <- sim_roster
team_5 <- sim_roster
team_6 <- sim_roster
team_7 <- sim_roster
team_8 <- sim_roster
team_9 <- sim_roster
team_10 <- sim_roster
team_11 <- sim_roster
team_12 <- sim_roster

sim_league_rosters <- list(team_1, team_2, team_3, team_4, team_5, team_6, team_7, team_8,
                           team_9, team_10, team_11, team_12)

names(sim_league_rosters) <- c("team_1", "team_2", "team_3", "team_4", "team_5", "team_6", "team_7",
                               "team_8", "team_9", "team_10", "team_11", "team_12")

test_simple <- snake_draft(dynasty_rankings, sim_league_rosters, draft_style = simple_bpa)
test_smart <- snake_draft(dynasty_rankings, sim_league_rosters, draft_style = smart_bpa)


eval_sim_draft <- function (sim_draft) {
  
  for (i in seq(length(sim_draft))) {
    draft_vbd <- as.data.frame(lapply(sim_draft, function(x) sum(x$Value)))
  }
  return(draft_vbd)
}

raw_vbd_pos <- data_frame(
  team_1 = numeric(0),
  team_2 = numeric(0),
  team_3 = numeric(0),
  team_4 = numeric(0),
  team_5 = numeric(0),
  team_6 = numeric(0),
  team_7 = numeric(0),
  team_8 = numeric(0),
  team_9 = numeric(0),
  team_10 = numeric(0),
  team_11 = numeric(0),
  team_12 = numeric(0)
)
for (i in 1:1000) {
  draft_iter <- snake_draft(dynasty_rankings, sim_league_rosters, draft_style = smart_bpa)
  iter_val <- eval_sim_draft(draft_iter)
  raw_vbd_pos <- rbind(raw_vbd_pos, iter_val)
}
avg_vbd_pos <- as.data.frame(lapply(raw_vbd_pos, function(x) mean(x)))

################################################################################

# Game Theory for Draft Strategy


################################################################################
# Classify Draft strategies during the draft/post draft

# Live draft dashboard of players taken by position group

# Preseason projections by team, redraft vs dynasty value
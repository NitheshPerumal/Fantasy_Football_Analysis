setwd("/home/joker/Fantasy_Football_Analysis")
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
source("./functions.R")
# https://docs.google.com/spreadsheets/d/1ooiLXL5M9JyNhWZf7F7wZ5Lt4nUvIUhcQVeD_SaHdvE/edit?fbclid=IwAR0A_9jFEMbOw-DaFpmofV3YzXg_XiSoM27WBHkU30P7gszHrYW59yHnPr0#gid=1963842693  


# LeagueID for API calls
leagueID <- "969743510867947520" # Revert Anything

################################################################################

# Wins vs PF (near line = at expectations, under line = overepxectation, over line = underperforming)

# Raw data for rosters
rosters_data <- sleeper_api_call(leagueID, "/rosters")

# Data for users to trace back to roster data
users_data <- sleeper_api_call(leagueID, "/users")
userid_name_table <- select(users_data, user_id, display_name)

team_overall_data <- merge(rosters_data, userid_name_table, by.x='owner_id', by.y='user_id')

# Plot
wins_pf_plot <- ggplot(team_overall_data, aes(x=settings$wins, y=settings$fpts, label=display_name)) +
  geom_point() +
  geom_ribbon(aes(ymin = 1500,ymax = predict(lm(settings$fpts~settings$wins))),
              alpha = 0.15,fill = 'blue') +
  geom_smooth(method=lm , color="black", fill="black", se=TRUE, alpha = 0.5) +
  geom_text(hjust=0, vjust=0) +
  xlab('Wins') +
  ylab('Points For (PF)') + 
  ggtitle(label = "Wins vs PF",
          subtitle = "Blue = Overperforming | White = Underperforming | Black = @ Expectation") +
  theme_classic()

print(wins_pf_plot) 

################################################################################

# Boxplot of Points Scored for every team
boxplot_input <- boxplot_fpts(14)

boxplot_long_df <- pivot_longer(boxplot_input, 
                                cols = starts_with("week_matchup_data$points"), 
                                names_to = "Variable", 
                                values_to = "Value")

boxplot_PF <- ggplot(boxplot_long_df, aes(x = display_name, y = Value)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "teams", y = "PF", title = "Boxplot of PF") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(boxplot_PF)

################################################################################

# PF-PA for point differential
point_diff <- rosters_data$settings$fpts - rosters_data$settings$fpts_against
point_diff_data <- cbind(as.data.frame(rosters_data$roster_id), as.data.frame(point_diff))

rosterid_ownerid_table <- select(rosters_data, roster_id, owner_id)
point_diff_plot_in <- merge(point_diff_data, rosterid_ownerid_table, 
                            by.x = "rosters_data$roster_id", by.y = "roster_id")
point_diff_plot_in <- merge(point_diff_plot_in, userid_name_table, 
                            by.x = "owner_id", by.y = "user_id")

point_diff_plot_in <- arrange(point_diff_plot_in, desc(point_diff_plot_in$point_diff))

point_df_plot <- ggplot(point_diff_plot_in, aes(x= reorder(display_name,-point_diff),point_diff)) + 
  geom_bar(stat = "identity", width=0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(point_df_plot)

################################################################################

# Function to make API calls, if you want LeagueID as endpoint pass "" as arg for endpoint
sleeper_api_call <- function (leagueID, endpoint, override_url=FALSE) {
  
  if (override_url == FALSE) {
    league_url <- url <- paste("https://api.sleeper.app/v1/league/",leagueID, sep = "")
  } else {
    league_url <- url <- paste("https://api.sleeper.app/v1/")
  }
  
  endpoint_url <- paste(league_url,endpoint,sep="")
  endpoint_json <- GET(endpoint_url)
  
  endpoint_data <- fromJSON(rawToChar(endpoint_json$content))
  
  return(endpoint_data)
}


# Function to iterate through weekly matchup data and get fpts per week
boxplot_fpts <- function (current_week) {
  
  rosterid_ownerid_table <- select(rosters_data, roster_id, owner_id)
  boxplot_join_table <- merge(rosterid_ownerid_table, userid_name_table, by.x='owner_id', by.y='user_id')
  boxplot_input <- arrange(boxplot_join_table[,c(2,1,3)], roster_id)
  
  for (i in seq(current_week)) {
    path_api <- paste("/matchups/",i,sep="")
    week_matchup_data <- sleeper_api_call(leagueID, path_api)
    
    
    fpts_week <- as.data.frame(week_matchup_data$points)
    boxplot_input <- cbind(boxplot_input, fpts_week)
  }
  
  return(boxplot_input)
}


# Simple Best Player Availanle - will just pick the highest player by VBD - every draft will be the same
simple_bpa <- function (draftboard, team_roster) {
  
  max_QB <- 3
  max_RB <- 4
  max_WR <- 4
  max_TE <- 2
  
  max_list <- list(
    QB = max_QB,
    RB = max_RB,
    WR = max_WR,
    TE = max_TE
  )
  
  positions_count <- table(team_roster$Position.1)
  #positions_count <- as.numeric(lapply(positions_count, function(x) { x[is.na(x)] <- 0; return(x) }))
  # Calculate the remaining positions available to draft for QB, RB, WR, and TE
  remaining_positions <- list(
    QB = max(0, max_QB - positions_count["QB"]),
    RB = max(0, max_RB - positions_count["RB"]),
    WR = max(0, max_WR - positions_count["WR"]),
    TE = max(0, max_TE - positions_count["TE"])
  )
  
  
  for (i in seq(length(remaining_positions))) {
    if(is.na(remaining_positions[i])) {
      remaining_positions[i] <- unlist(max_list[i])
    }
  }
  
  remaining_positions_corr <- t(as.data.frame(sapply(remaining_positions, function(x) { x[is.na(x)] <- 0; return(x) })))
  
  if (nrow(team_roster) != 0) {
    # Filter draftboard by available positions
    db_filtered <- draftboard[draftboard$Position.1 %in% names(which(colSums(remaining_positions_corr) > 0 )),]
  } else {
    db_filtered <- draftboard
  }
  
  new_draftboard <- update_draftboard(draftboard, db_filtered[1,1])
  
  return(list(new_draftboard, db_filtered[1,]))
}


# Update the draftboard after a pick is made
update_draftboard <- function(draftboard, pick) {
  
  # Update the draftboard with the pick
  updated_draftboard <- draftboard[draftboard$Player != pick, ]
  
  return(updated_draftboard)
}


# Function to run a snake draft
snake_draft <- function (draftboard, team_dataframes, draft_style) {
  num_rounds <- 13  # Total number of rounds
  
  draft_results <- list()  # To store the draft results
  db_temp <- draftboard
  
  for (round in 1:num_rounds) {
    if (round %% 2 == 1) {
      # Odd round: Teams pick in regular order
      for (i in seq(length(sim_league_rosters))) {
        out <- draft_style(db_temp,sim_league_rosters[[i]])
        names(out) <- c("db", "pick")
        db_temp <- out$db
        sim_league_rosters[[i]] <- rbind(sim_league_rosters[[i]],out$pick)
      }
      
      
    } else {
      # Even round: Teams pick in reverse order
      for (i in rev(seq(length(sim_league_rosters)))) {
        out <- draft_style(db_temp,sim_league_rosters[[i]])
        names(out) <- c("db", "pick")
        db_temp <- out$db
        sim_league_rosters[[i]] <- rbind(sim_league_rosters[[i]],out$pick)
      }
      
    }
    
  }
  
  return(sim_league_rosters)
}


# Smarter Best Player Availanle - variance in picks based on VBD
smart_bpa <- function (draftboard, team_roster) {
  
  max_QB <- 3
  max_RB <- 4
  max_WR <- 4
  max_TE <- 2
  
  max_list <- list(
    QB = max_QB,
    RB = max_RB,
    WR = max_WR,
    TE = max_TE
  )
  
  positions_count <- table(team_roster$Position.1)
  #positions_count <- as.numeric(lapply(positions_count, function(x) { x[is.na(x)] <- 0; return(x) }))
  # Calculate the remaining positions available to draft for QB, RB, WR, and TE
  remaining_positions <- list(
    QB = max(0, max_QB - positions_count["QB"]),
    RB = max(0, max_RB - positions_count["RB"]),
    WR = max(0, max_WR - positions_count["WR"]),
    TE = max(0, max_TE - positions_count["TE"])
  )
  
  
  for (i in seq(length(remaining_positions))) {
    if(is.na(remaining_positions[i])) {
      remaining_positions[i] <- unlist(max_list[i])
    }
  }
  
  remaining_positions_corr <- t(as.data.frame(sapply(remaining_positions, function(x) { x[is.na(x)] <- 0; return(x) })))
  
  if (nrow(team_roster) != 0) {
    # Filter draftboard by available positions
    db_filtered <- draftboard[draftboard$Position.1 %in% names(which(colSums(remaining_positions_corr) > 0 )),]
  } else {
    db_filtered <- draftboard
  }
  
  db_filtered <- db_filtered[c(1:3),]
  denom <- sum(db_filtered$Value)
  db_filtered$Probability <- db_filtered$Value / denom
  
  selected_player <- sample(db_filtered$Player, size = 1, prob = db_filtered$Probability)
  
  new_draftboard <- update_draftboard(draftboard, selected_player)
  
  return(list(new_draftboard, db_filtered[db_filtered$Player == selected_player,]))
}


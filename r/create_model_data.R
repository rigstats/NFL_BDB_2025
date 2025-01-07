##############################
# CREATE MODELING DATASET ####
##############################


# Libraries ####

library(tidyverse)
library(data.table)


# Raw Data ####

# non-tracking data
games <- readr::read_csv('./input/games.csv')
players <- readr::read_csv('./input/players.csv')
plays <- readr::read_csv('./input/plays.csv')
player_play <- readr::read_csv('./input/player_play.csv')
nflverse_joined <- readRDS('./input/nflverse_joined.rds')


players <- 
   players |> 
   mutate(height_inches = map_int(
      str_split(height, "-"), 
      ~ as.integer(.x[1]) * 12 + as.integer(.x[2]))
   ) |> 
   select(-height, -birthDate, -collegeName)

plays <- 
   plays |> 
   mutate(distanceToGoal = ifelse(possessionTeam == yardlineSide,
                                  100 - yardlineNumber,
                                  yardlineNumber))


# Functions ####

# trim excess frames from tracking data
trim_tracking_frames <- 
   
   function(df_tracking, 
            start_events = c("line_set", "man_in_motion"), 
            end_events = c("ball_snap",
                           "autoevent_ballsnap",
                           "snap_direct"),
            keep_cols = TRUE) {
      
      
      tracking_events <- 
         df_tracking |> 
         filter(!is.na(event)) |> 
         distinct(gameId, playId, frameId, event)
      
      has_events <- 
         tracking_events |> 
         group_by(gameId, playId) |> 
         filter(sum(event %in% start_events, na.rm = T) > 0) |> 
         # filter(sum(event %in% end_events, na.rm = T) > 0) |> 
         ungroup()
      
      
      start_frames <- 
         df_tracking |> 
         distinct(gameId, playId, frameId, event) |> 
         semi_join(has_events, by = join_by(gameId, playId)) |> 
         group_by(gameId, playId) |> 
         mutate(frameIdStart = min(frameId[event %in% start_events], na.rm = T),
                frameIdEnd = max(frameId, na.rm = T)) |> 
         ungroup() |> 
         distinct(gameId, playId, frameIdStart, frameIdEnd)
      
      
      trimmed_tracking <- 
         df_tracking |> 
         semi_join(has_events, by = join_by(gameId, playId)) |> 
         inner_join(start_frames, by = join_by(gameId, playId)) |> 
         filter(frameId >= frameIdStart,
                frameId <= frameIdEnd) 
      
      if (keep_cols == FALSE) {
         
         trimmed_tracking$frameIdStart <- NULL
         trimmed_tracking$frameIdEnd <- NULL
         
      } 
      
      return(trimmed_tracking)
      
   }


# read raw csv, filter for pre-snap frames, then process/standardize coord features
read_process_tracking <- function(week_number) {
   
   tracking <- 
      
      data.table::fread(paste0('./input/tracking_week_', week_number, '.csv')) |> 
      
      # pre-snap frames only
      filter(frameType != "AFTER_SNAP") |> 
      
      tibble() |> 
      
      # determine the side of ball using offense and defense teams
      inner_join(plays |> 
                    select(gameId, playId, possessionTeam, defensiveTeam),
                 by = join_by(gameId, playId)) |> 
      mutate(side = ifelse(club == defensiveTeam, -1, 1)) |> 
      select(-possessionTeam, -defensiveTeam) |> 
      
      # convert direction and orientation features as cartesian coords
      mutate(dir = ((dir - 90) * -1) %% 360,
             o = ((o - 90) * -1) %% 360) |> 
      mutate(vx = s * cos(dir * pi / 180),
             vy = s * sin(dir * pi / 180),
             ax = a * cos(dir * pi / 180),
             ay = a * sin(dir * pi / 180),
             ox = cos(o * pi / 180),
             oy = sin(o * pi / 180)) |> 
      
      # standardize so all plays go from left-to-right
      mutate(x = ifelse(playDirection == "right", x, 120 - x),
             y = ifelse(playDirection == "right", y, 53.3 - y),
             across(.cols = c(vx, vy, ax, ay, ox, oy),
                    .fns = function(x) ifelse(playDirection == "right", x, -x)))
   

   return(tracking)
   
}


# main function to produce the `model_data` dataset
prep_tracking_for_model_v <- function(tracking_df, 
                                      games_df, 
                                      plays_df, 
                                      player_df,
                                      player_play_df,
                                      nflverse_df,
                                      n_frames = 40) {
   
   tracking_fb <- 
      tracking_df |> 
      filter(displayName == "football") |> 
      select(gameId, playId, frameId, x_fb = x, y_fb = y)
   
   tracking_dt <- 
      tracking_df |> 
      semi_join(plays_df |> select(gameId, playId, possessionTeam),
                by = join_by(gameId, playId, club == possessionTeam)) |> 
      #mutate(onOffense = side == 1 | displayName == "football") |> 
      #filter(onOffense) |> 
      #select(-onOffense) |> 
      group_by(gameId, playId) |> 
      mutate(line_set_idx = max(frameId) - n_frames) |> 
      ungroup() |> 
      filter(frameId >= line_set_idx) |> 
      # group_by(gameId, playId, frameId) |> 
      # mutate(x_fb = x[club == "football"],
      #        y_fb = y[club == "football"]) |> 
      # ungroup() |> 
      data.table::as.data.table()
   
   # join in fb coords and get relative positions for all players
   tracking_dt_joined <- 
      tracking_dt |> 
      filter(club != "football") |> 
      inner_join(tracking_fb, by = join_by(gameId, playId, frameId)) |> 
      mutate(x = x - x_fb,
             y = y - y_fb) |> 
      mutate(abs_y = abs(y)) |> 
      inner_join(player_df |> select(nflId, position)) |> 
      mutate(position = case_match(position,
                                   c('C', 'G', 'T') ~ 'OL',
                                   c('RB','FB','HB') ~ 'RB',
                                   .default = position))
   
   # Grouped summary stats for each position and each frame
   positional_aggregates <- 
      tracking_dt_joined |> 
      group_by(gameId, playId, frameId, nflId, position) |> 
      summarise(across(.cols = c(x, y, abs_y),
                       .fns = list(mean = mean,
                                   std = sd,
                                   max = max,
                                   min = min),
                       .names = "{.fn}_{.col}")) |> 
      ungroup() |> 
      group_by(gameId, playId, frameId, position) |> 
      mutate(across(.cols = starts_with(c("max", "min")),
                    .fns = list(mean = mean, std = sd, max = max, min = min),
                    .names = "pos_{.fn}_{.col}"),
             count = n()) |> 
      ungroup() |> 
      arrange(position) |> 
      select(gameId, playId, frameId, position, count, starts_with("pos")) |> 
      distinct() |> 
      pivot_wider(names_from = position,
                  names_glue = "{position}_{.value}",
                  values_from = -c(gameId, playId, position, frameId)) |> 
      select(gameId, playId, frameId, contains("count"), 
             RB_min_x = RB_pos_min_min_x,
             QB_min_x = QB_pos_min_min_x,
             TE_min_abs_y = TE_pos_min_min_abs_y)
   
   # Create in-game rolling counts of plays (n passes, rushes) and successes (n passes, rushes)
   rolling_stats_in_game <- 
      plays_df |> 
      select(gameId, playId, isDropback, possessionTeam, offenseFormation) |> 
      inner_join(nflverse_df |> 
                    filter(!is.na(xpass)) |> 
                    filter(pass == 1 | rush == 1) |> 
                    filter(down %in% c(1,2,3,4)) |> 
                    filter(qtr %in% c(1,2,3,4)) |> 
                    select(gameId, playId, week, xpass, success)) |> 
      arrange(week, gameId, playId, possessionTeam) |> 
      mutate(isDropback = as.numeric(isDropback)) |> 
      mutate(pass_success = ifelse(isDropback == 1 & success == 1, 1, 0),
             rush_success = ifelse(isDropback == 0 & success == 1, 1, 0)) |> 
      group_by(week, gameId, possessionTeam) |> 
      mutate(n_plays = seq_along(playId),
             csum_success = cumsum(success),
             csum_pass = cumsum(isDropback),
             csum_pass_success = cumsum(pass_success),
             csum_rush_success = cumsum(rush_success)) |> 
      ungroup() |> 
      mutate(pre_success = csum_success - success,
             pre_pass = csum_pass - isDropback,
             pre_pass_success = csum_pass_success - pass_success,
             pre_rush_success = csum_rush_success - rush_success) |> 
      mutate(pre_rush = n_plays - 1 - pre_pass, 
             pre_pass_success_rt = pre_pass_success / pre_pass,
             pre_success_prop_pass = pre_pass_success / pre_success,
             pre_rush_success_rt = pre_rush_success / pre_rush,
             pre_success_prop_rush = pre_rush_success / pre_success)
   
   
   positional_aggregates |> 
      inner_join(rolling_stats_in_game)
   
}




# Function Usage ####

# Initialize list for looping over all weeks
tracking_list <- list()
weeks <- c(1:9)

for(i in weeks) {
   
   current_week <- as.character(weeks[i])
   
   tracking_list[[i]] <- 
      read_process_tracking(current_week) |> 
      trim_tracking_frames(keep_cols = FALSE)
   
}

# bind results together from all 9 weeks
presnap_tracking_standardized <- 
   data.table::rbindlist(tracking_list) |> 
   tibble()

# save data for future use
saveRDS(presnap_tracking_standardized, "./input/presnap_tracking_standardized.rds")



# NOTE: will take some time to run
model_data <- prep_tracking_for_model_v(tracking_df = presnap_tracking_standardized,
                                        games_df = games,
                                        plays_df = plays,
                                        player_df = players,
                                        player_play_df = player_play,
                                        nflverse_df = nflverse_joined,
                                        n_frames = 40)

# remove columns
model_data$DT_count <- NULL
model_data$FS_count <- NULL
model_data$ILB_count <- NULL
model_data$OLB_count <- NULL

# minor cleaning / sorting of columns
model_data <- 
   model_data |> 
   relocate(week, gameId, playId, possessionTeam, frameId, 
            isDropback, xpass) |> 
   arrange(week, gameId, playId, possessionTeam, frameId) |> 
   rename(label = isDropback)

# save data for future use
readr::write_csv(model_data, "./input/model_data.csv")
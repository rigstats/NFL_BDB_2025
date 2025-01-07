################################
# STEP 01 - LOAD INPUT DATA ####
################################


# Libraries ####

library(tidyverse)
library(nflplotR)
library(nflreadr)


# Data ####

## Raw BDB datasets ----

# non-tracking data
games <- readr::read_csv('./input/games.csv')
players <- readr::read_csv('./input/players.csv')
plays <- readr::read_csv('./input/plays.csv')
player_play <- readr::read_csv('./input/player_play.csv')


# tracking data

# read in processed version if exists, create it if it doesn't
if (file.exists('./input/tracking_union_mod.rds')) {
   
   tracking <- readRDS('./input/tracking_union_mod.rds')
   
} else {
   
   # tracking files' names and paths
   tracking_files <- list.files(path = './input/', 
                                full.names = TRUE)[grep(pattern = 'tracking', 
                                                        x = list.files('./input/'))]
   
   # initialize list to store results
   tracking_list <- list()
   
   # loop over files
   for (i in seq_along(tracking_files)) {
      
      tracking_list[[i]] <- 
         readr::read_csv(file = tracking_files[i]) %>% 
         mutate(x = ifelse(playDirection == 'left', 120 - x, x),
                y = ifelse(playDirection == 'left', (160 / 3) - y, y),
                dir = ifelse(playDirection == 'left', dir + 180, dir),
                o = ifelse(playDirection == 'left', o + 180, o)) %>% 
         mutate(dir = ifelse(dir > 360, dir - 360, dir),
                o = ifelse(o > 360, o - 360, o))
      
   }
   
   # bind together results into single df
   tracking <- data.table::rbindlist(tracking_list) %>% tibble()
   
   # save results for future loading
   saveRDS(tracking, './input/tracking_union_mod.rds')
   #readr::write_csv(tracking, file = './data/tracking.csv')
   
   # if successful, remove the loop's temp objects from environment
   # if (file.exists('./data/tracking.rds') & nrow(tracking == 59327373)) {
   #    
   #    rm(list = c('tracking_files', 'tracking_list', 'i'))
   #    
   # }
   
}


## nflverse datasets

if (file.exists('./input/nflverse_joined.rds')) {
   
   nflverse_joined <- readRDS('./input/nflverse_joined.rds')
   
} else {
   
   # full PBP
   nflv_pbp <- 
      nflreadr::load_pbp(seasons = 2022) |> 
      tibble() |> 
      filter(week <= 9) |> 
      mutate(gameId = as.integer(old_game_id),
             playId = as.integer(play_id)) |> 
      select(week, 
             gameId,
             playId,
             game_id,
             posteam, 
             defteam, 
             posteam_score, 
             defteam_score, 
             score_differential, 
             down, 
             ydstogo,
             yardline_100,
             qtr,
             desc, 
             half_seconds_remaining,
             posteam_timeouts_remaining,
             defteam_timeouts_remaining,
             play_type,
             pass,
             rush,
             yards_gained, 
             success, 
             pass_length, 
             pass_location, 
             air_yards,
             yac = yards_after_catch,
             run_location, 
             run_gap, 
             ep,
             epa,
             air_epa,
             yac_epa,
             wp,
             wpa,
             passer_player_id:rushing_yards,
             xpass,
             pass_oe,
             cp,
             cpoe,
             series, series_success, series_result, 
             fixed_drive, fixed_drive_result, 
             drive_play_count, drive_time_of_possession, 
             drive_first_downs, drive_ended_with_score, 
             drive_start_transition, drive_end_transition)
   
   
   # FTN charting
   nflv_ftn <- 
      nflreadr::load_ftn_charting(seasons = 2022) |>  
      tibble() |> 
      filter(week <= 9) |> 
      select(week, 
             game_id = nflverse_game_id,
             playId = nflverse_play_id,
             starting_hash,
             qb_location,
             read_thrown,
             starts_with("n_"),
             starts_with("is_")) |> 
      mutate(across(.cols = where(is.logical),
                    .fns = function(x) as.integer(x)))
   
   # Participation (personnel, formations, ...)
   nflv_participation <- 
      nflreadr::load_participation(seasons = 2022) |> 
      tibble() |> 
      filter(nflverse_game_id %in% unique(nflv_ftn$game_id),
             possession_team != "") |> 
      mutate(gameId = as.integer(old_game_id)) |> 
      select(game_id = nflverse_game_id,
             gameId,
             playId = play_id,
             posteam = possession_team,
             personnelOff = offense_personnel,
             personnelDef = defense_personnel,
             manZone = defense_man_zone_type,
             coverageType = defense_coverage_type,
             # air_yards = ngs_air_yards,
             time_to_throw,
             route,
             was_pressure)
   
   nflverse_joined <- 
      nflv_pbp |> 
      inner_join(nflv_ftn, by = join_by(week, playId, game_id)) |> 
      inner_join(nflv_participation, by = join_by(gameId, playId, game_id, posteam)) |> 
      arrange(week, gameId, playId)
   
   
   saveRDS(nflverse_joined, file = "./input/nflverse_joined.rds")
   
}




nflv_colors <- 
   nflreadr::load_teams() |> 
   select(club = team_abbr,
          color1 = team_color,
          color2 = team_color2,
          color3 = team_color3) |> 
   rbind.data.frame(c("football","#000000","#000000","#000000")) |> 
   mutate(across(.fns = function(x) tidyr::replace_na(x, replace = "#000000")))


# saveRDS(nflv_colors, "./input/nflverse_colors.rds")
# readr::write_csv(nflv_join, "./input/nflverse_joined.csv")

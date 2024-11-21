################################
# STEP 01 - LOAD INPUT DATA ####
################################


# Libraries ####

library(dplyr)
library(ggplot2)
library(nflplotR)
library(nflreadr)
library(gganimate)


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
   
   nflv_participation <-
      nflreadr::load_participation(seasons = 2022) |> 
      tibble()
   
   nflv_ftn <-
      nflreadr::load_ftn_charting(seasons = 2022) |> 
      filter(week <= 9) |> 
      tibble()
   
   nflv_pbp <-
      nflreadr::load_pbp(seasons = 2022) |>
      filter(week <= 9) |> 
      tibble()
   
   nflverse_joined <-
      nflv_ftn |> 
      rename(play_id = nflverse_play_id) |> 
      select(-ftn_game_id, -ftn_play_id, -date_pulled, -season) |>
      inner_join(nflv_participation, 
                 by = join_by(nflverse_game_id, play_id)) |>
      inner_join(nflv_pbp |> 
                    select(nflverse_game_id = game_id, 
                           old_game_id, 
                           play_id,
                           play_type, pass, rush,
                           pass_length, pass_location, air_yards,
                           run_location, run_gap,
                           posteam, defteam,
                           success, yardline_100,
                           posteam_score, defteam_score, score_differential,
                           xpass, pass_oe, cp, cpoe,
                           game_half, 
                           game_seconds_remaining, half_seconds_remaining,
                           posteam_timeouts_remaining, 
                           defteam_timeouts_remaining,
                           wp, wpa),
                 by = join_by(old_game_id, nflverse_game_id, play_id)) |> 
      relocate(week, old_game_id, nflverse_game_id, play_id)
   
   saveRDS(nflverse_joined, "./input/nflverse_joined.rds")
   # readr::write_csv(nflv_join, "./input/nflverse_joined.csv")
   
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

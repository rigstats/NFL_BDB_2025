# LOAD EXTERNAL DATA ####


# Libraries ----
library(dplyr)
library(tidyr)
library(nflreadr)


# Data ----

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


readr::write_tsv(nflverse_joined, file = "./input/nflverse_joined.tsv")
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

# non-tracking data
games <- readr::read_csv('./input/games.csv')
players <- readr::read_csv('./input/players.csv')
plays <- readr::read_csv('./input/plays.csv')
player_play <- readr::read_csv('./input/player_play.csv')


# tracking data

tracking_union <- data.table::fread(input = "./tableau/tracking_union.csv")

saveRDS(tracking_union, './input/tracking_union.rds')

tracking <- readRDS('./input/tracking_union.rds')

tracking_tbl <- as_tibble(tracking)


# adjust coord variables
tracking_tbl$playDirection <- as.integer(tracking_tbl$playDirection == "right")

tracking_tbl$x <- ifelse(tracking_tbl$playDirection, 
                         tracking_tbl$x, 
                         120 - tracking_tbl$x)

tracking_tbl$y <- ifelse(tracking_tbl$playDirection, 
                         tracking_tbl$y, 
                         (160/3) - tracking_tbl$y)

tracking_tbl$dir <- ifelse(tracking_tbl$playDirection, 
                           tracking_tbl$dir, 
                           180 + tracking_tbl$dir)

tracking_tbl$dir <- ifelse(tracking_tbl$dir > 360, 
                           tracking_tbl$dir - 360,
                           tracking_tbl$dir)

tracking_tbl$o <- ifelse(tracking_tbl$playDirection, 
                         tracking_tbl$o, 
                         180 + tracking_tbl$o)

tracking_tbl$o <- ifelse(tracking_tbl$o > 360, 
                         tracking_tbl$o - 360,
                         tracking_tbl$o)


# simplify other vars
tracking_tbl$week <- stringr::str_extract(string = tracking_tbl$table_name,
                                          pattern = "[0-9]")

tracking_tbl$week <- as.integer(tracking_tbl$week)

tracking_tbl$table_name <- NULL

saveRDS(tracking_tbl, "./input/tracking_union_mod.rds")


tracking_tbl %>% 
   mutate(x = ifelse(playDirection == 'left', 120 - x, x),
          y = ifelse(playDirection == 'left', (160 / 3) - y, y),
          dir = ifelse(playDirection == 'left', dir + 180, dir),
          o = ifelse(playDirection == 'left', o + 180, o)) %>% 
   mutate(dir = ifelse(dir > 360, dir - 360, dir),
          o = ifelse(o > 360, o - 360, o))
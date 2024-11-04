################################
# STEP 02 - PROCESS DATA #######
################################


# run script to load data
source('./r/01_load_data.R')


# Plays ####

plays_adj <- 
   tracking %>% 
   distinct(gameId, playId, playDirection) %>% 
   
   # join `playDirection` column from `tracking` df to create/correct yardline columns 
   left_join(plays, by = join_by(gameId, playId)) %>% 
   
   # version of `yardlineNumber` but on a 1-100 yardline scale instead of 1-50
   mutate(yardline_100 = ifelse(playDirection == 'right', 
                                absoluteYardlineNumber - 10, 
                                110 - absoluteYardlineNumber)) %>% 
   
   # yardline of first-down marker
   mutate(yardline_fDown = yardline_100 + yardsToGo) %>% 
   
   # corrected version of column to adjust for left-facing plays
   mutate(absoluteYardlineNumber = yardline_100 + 10) %>% 
   
   # convert to binary
   mutate(playNullifiedByPenalty = ifelse(playNullifiedByPenalty == 'Y', 1L, 0L)) %>% 
   
   # drop unused / irrelevant columns
   select(-yardlineNumber, -yardlineSide, -gameClock) %>% 
   
   # clean names of others
   rename(xpass = passProbability,
          wp_home = preSnapHomeTeamWinProbability,
          wp_away = preSnapVisitorTeamWinProbability,
          wpa_home = homeTeamWinProbabilityAdded,
          wpa_away = visitorTeamWinProbilityAdded,
          ep = expectedPoints,
          epa = expectedPointsAdded)


#relocate(yardline_100, yardline_fDown, absoluteYardlineNumber, yardsToGo, yardlineNumber, possessionTeam, yardlineSide, .after = playDirection)


# Games ####

games_adj <- 
   
   games %>% 
   
   # drop unused / irrelevant columns
   select(-season, -gameDate, -gameTimeEastern) %>% 
   
   # clean names of others
   rename(home = homeTeamAbbr,
          away = visitorTeamAbbr,
          homeScore = homeFinalScore,
          awayScore = visitorFinalScore) %>% 
   
   # rearrange, sort 
   relocate(week) %>% 
   
   arrange(week, gameId)


# Players ####

# drop unused / irrelevant columns, 
# create columns to group positions,
# add identifiers for duplicated players (mult. teams, numbers, etc.)
players_adj <- 
   players %>% 
   mutate(positionGroup = 
             case_when(position %in% c('CB', 'DB', 'FS', 'SS') ~ 'DB',
                       position %in% c('OLB', 'DT', 'DE', 'NT') ~ 'DL',
                       position %in% c('T', 'G', 'C', 'LS') ~ 'OL',
                       position %in% c('ILB', 'MLB') ~ 'LB',
                       position %in% c('RB', 'FB') ~ 'RB',
                       position %in% c('WR', 'TE', 'QB') ~ position,
                       TRUE ~ 'ERR')) %>% 
   mutate(isOffense = case_when(positionGroup %in% c('DB', 'DL', 'LB') ~ 0,
                                positionGroup %in% c('OL', 'RB', 'WR', 'TE', 'QB') ~ 1,
                                TRUE ~ NA_real_)) %>% 
   inner_join(tracking %>% distinct(nflId, gameId, club, jerseyNumber)) %>% 
   inner_join(games_adj %>% distinct(gameId, week)) %>% 
   group_by(nflId, club, jerseyNumber, position, positionGroup, isOffense, displayName) %>% 
   summarise(n_games = n(),
             week_from = min(week),
             week_to = max(week)) %>% 
   ungroup() %>% 
   add_count(nflId, name = "n_occs") %>% 
   #filter(n > 1) %>% 
   arrange(nflId, week_from)


# Tracking ####

## General Cleaning / Transforming ----

tracking_adj <- 
   
   tracking %>% 
   
   # remove "autoevent_" versions of event labels
   mutate(event = case_when(event == 'autoevent_passinterrupted' ~ 'pass_interrupted',
                            event == 'autoevent_ballsnap' ~ 'ball_snap',
                            event == 'autoevent_passforward' ~ 'pass_forward',
                            TRUE ~ event)) %>% 
   
   # round values
   mutate(across(.cols = c(x, y, s, a, dis), .fns = function(x) round(x, digits = 2)),
          across(.cols = c(o, dir), .fns = function(x) round(x, digits = 0))) %>% 
   
   # join `plays` dataset
   inner_join(plays_adj %>% distinct(gameId, playId, possessionTeam, ballCarrierId),
              by = join_by(gameId, playId)) %>% 
   
   # create flag variables `is_offense` and `is_ball_carrier`
   mutate(clubTeam = case_when(club == possessionTeam ~ 'OFF',
                               club == 'football' ~ 'BALL',
                               club != possessionTeam ~ 'DEF',
                               TRUE ~ 'ERR'),
          isBallCarrier = ifelse(nflId == ballCarrierId & club != 'football', 1, 0)) %>% 
   
   # remove unneeded columns
   select(-time, -playDirection, -possessionTeam, -ballCarrierId)


## Events ----

# df of all gameId,playId with the first and last frameId with non-null events
tracking_frame_windows <- 
   tracking %>% 
   filter(!is.na(event)) %>% 
   distinct(gameId, playId, frameId, event) %>% 
   mutate(event = 
             case_when(event == 'autoevent_passinterrupted' ~ 'pass_interrupted',
                       event == 'autoevent_ballsnap' ~ 'ball_snap',
                       event == 'autoevent_passforward' ~ 'pass_forward',
                       TRUE ~ event)) %>% 
   group_by(gameId, playId) %>% 
   filter(frameId == min(frameId) | frameId == max(frameId)) %>% 
   ungroup() %>% 
   add_count(gameId, playId) %>% 
   arrange(gameId, playId, frameId, event) %>% 
   group_by(gameId, playId, frameId) %>% 
   mutate(seq_event = seq_along(frameId)) %>% 
   ungroup() %>% 
   filter(seq_event == 1) %>% 
   select(-n, -seq_event) %>% 
   group_by(gameId, playId) %>% 
   mutate(start_end = seq_along(event)) %>% 
   ungroup() %>% 
   mutate(start_end = ifelse(start_end == 1, 'start', 'end')) %>% 
   tidyr::pivot_wider(id_cols = c(gameId, playId), 
                      names_from = start_end, 
                      values_from = frameId)

# TBD - 
#  alt method to identify moment (frameId, event) where ball carrier 
#     changes or becomes "active" (catching a pass, receiving handoff, 
#     QB runs past the LOS, etc.)
#  This may have significance bc defenses need to swarm the ball ==> must react to
#     ball and/or its carrier at all times and treat as a fluid volatile target
tracking %>% 
   filter(!is.na(event)) %>% 
   distinct(gameId, playId, frameId, event) %>% 
   mutate(event = 
             case_when(event == 'autoevent_passinterrupted' ~ 'pass_interrupted',
                       event == 'autoevent_ballsnap' ~ 'ball_snap',
                       event == 'autoevent_passforward' ~ 'pass_forward',
                       TRUE ~ event)) %>% 
   mutate(event_type = 
             case_when(event %in% c('line_set', 'man_in_motion', 'shift') ~ 'PRESNAP',
                       event %in% c('ball_snap', 'snap_direct') ~ 'SNAP',
                       event %in% c('pass_interrupted', 'out_of_bounds', 'tackle',
                                    'qb_sack', 'qb_slide', 'touchdown', 'safety',
                                    'pass_outcome_touchdown', 'fumble') ~ 'END',
                       event %in% c('run', 'handoff', 'pass_arrived', 'pass_shovel',
                                    'pass_outcome_caught') ~ 'BALL_CARRIER',
                       TRUE ~ 'OTHER')) %>% 
   group_by(gameId, playId) %>% 
   mutate(start_frame = min(frameId[event_type != 'PRESNAP'], na.rm = TRUE),
          end_frame = min(frameId[event_type == 'END'], na.rm = TRUE)) %>% 
   #mutate(start_frame = ifelse(is.infinite(start_frame), min(frameId[event_type == 'SNAP'], na.rm = TRUE), start_frame)) %>% 
   ungroup() %>% 
   distinct(gameId, playId, start_frame, end_frame)
   #filter(frameId == start_frame | frameId == end_frame) %>% 
   #arrange(gameId, playId, frameId, event_type, event)
#   filter(is.infinite(start_frame))


# TBD - function to extract tracking data for ball carrier
fn_get_carrier_path <- function(tracking_data, plays_data, gameId_ = NULL, playId_ = NULL){
   
   # carrierId <- plays_data$ballCarrierId[plays_data$gameId == gameId_ & plays_data$playId == playId_]
   
   carrierTracking <- 
      tracking_data %>% 
      semi_join(plays_data %>% select(gameId, playId, nflId = ballCarrierId), 
                by = join_by(gameId, playId, nflId)) %>% 
      group_by(gameId, playId, nflId) %>% 
      mutate(carrierStart = 
                min(frameId[event %in% c('run', 'handoff', 'pass_arrived', 'snap_direct',
                                         'pass_shovel', 'pass_outcome_caught', 'lateral')],
                    na.rm = TRUE),
             carrierEnd =
                min(frameId[event %in% c('pass_interrupted', 'out_of_bounds', 'tackle',
                                         'qb_sack', 'qb_slide', 'touchdown', 'safety',
                                         'pass_outcome_touchdown', 'fumble')],
                    na.rm = TRUE)) %>% 
      # correct instances where plays don't have any of the above start events
      mutate(carrierStart = 
                ifelse(is.infinite(carrierStart), 
                       min(frameId[event == 'ball_snap'], na.rm = TRUE),
                       carrierStart)) %>% 
      ungroup() %>% 
      mutate(isActiveCarrier = ifelse(frameId >= carrierStart & frameId <= carrierEnd, 1, 0))
   
   
   return(carrierTracking)
   
}


test_carrier_paths <- fn_get_carrier_path(tracking_data = tracking_adj, plays_data = plays_adj)

test_carrier_paths %>% 
   filter(gameId == 2022101603, playId <= 662) %>% 
   ggplot() +
   geom_point(aes(x = x, y = y, alpha = factor(isActiveCarrier), color = factor(playId)), 
              #alpha = 0.25,
              #color = 'gray40',
              fill = 'black',
              size = 4) +
   geom_point(data = . %>% filter(!is.na(event)),
              aes(x = x, y = y, group = playId),
              color = "red", size = 2, alpha = 1) +
   ggrepel::geom_label_repel(data = . %>% filter(!is.na(event)),
                             aes(x = x, y = y, label = event),
                             #nudge_x = -2.5, #nudge_y = -0.25,
                             alpha=0.85, max.overlaps = 25) +
   coord_equal(#xlim = c(10,110), 
      #ylim = c(0,160/3)
   ) +
   theme_minimal() +
   theme(legend.position = "none")


## Subsets - Player Roles ----

#  players ID'd as ball carrier (from `plays` dataset)
tracking_ball_carrier <- 
   tracking %>% 
   semi_join(plays_adj %>% rename(nflId = ballCarrierId), 
             by = join_by(gameId, playId, nflId)) %>% 
   select(gameId, playId, frameId, event,
          bc_club = club,
          bc_number = jerseyNumber,
          bc_id = nflId, 
          bc_name = displayName,
          bc_x = x,
          bc_y = y,
          bc_o = o, 
          bc_dir = dir,
          bc_s = s)

#  players ID'd with a tackle event (from `tackles` dataset)
tracking_tacklers <- 
   tracking %>% 
   inner_join(tackles, #%>% filter(tackle == 1), 
             by = join_by(gameId, playId, nflId)) %>% 
   select(gameId, playId, frameId, event,
          tackle, assist, forcedFumble, missed = pff_missedTackle,
          tk_club = club,
          tk_number = jerseyNumber,
          tk_id = nflId, 
          tk_name = displayName,
          tk_x = x,
          tk_y = y,
          tk_o = o, 
          tk_dir = dir,
          tk_s = s)

# ball
tracking_ball <- 
   tracking %>% 
   filter(club == 'football') %>% 
   select(gameId, playId, frameId, event,
          # fb_id = nflId, 
          # fb_name = displayName,
          # fb_number = jerseyNumber,
          fb_x = x,
          fb_y = y)


# test creating a df of defenders joined to ball carriers' tracking 
# to be able to compute distances and angles to ball carrier
tracking_def_vs_bc <- 
   tracking_adj %>% 
   filter(clubTeam != 'BALL', isBallCarrier == 0, clubTeam == 'DEF') %>% 
   inner_join(tracking_adj %>% 
                 filter(isBallCarrier == 1) %>% 
                 select(gameId, playId, frameId, event, 
                        bc_club = club,
                        bc_number = jerseyNumber,
                        bc_id = nflId, 
                        bc_name = displayName,
                        bc_x = x,
                        bc_y = y,
                        bc_o = o, 
                        bc_dir = dir),
              by = join_by(gameId, playId, frameId, event)) %>% 
   mutate(dx_bc = bc_x - x,
          dy_bc = bc_y - y) %>% 
   select(gameId, playId, frameId, event, nflId, jerseyNumber, clubTeam, 
          x, y, o, dir, bc_id, bc_x, bc_y, bc_o, bc_dir, 
          dx_bc, dy_bc)


# look at plays where contact was made
#  - how far away was defender to the BC?
#  - was a tackle made afterwards? if so, what variables lend themselves to this?
tracking_def_vs_bc %>% 
   mutate(dist_to_bc = abs(sqrt(dx_bc^2 + dy_bc^2))) %>% 
   # group_by(gameId, playId) %>% 
   # filter(sum(event == 'first_contact', na.rm = TRUE) > 0) %>% 
   # ungroup() %>% 
   filter(event == 'first_contact') %>% 
   slice_min(order_by = dist_to_bc, n = 1, by = c(gameId, playId)) %>% 
   filter(gameId == 2022090800) %>% 
   ggplot() +
   #geom_point(aes(x = dx_bc, y = dy_bc), alpha = 0.25, size = 4) +
   geom_spoke(aes(x = x, y = y, angle = (dir+90)/360*2*pi, radius = 1.5),
              color = "red") +
   geom_spoke(aes(x = bc_x, y = bc_y, angle = (bc_dir+90)/360*2*pi, radius = 1.5),
              color = "blue") +
   geom_segment(aes(x = x, xend = bc_x, y = y, yend = bc_y), alpha = 0.5)


# test plotting tracking animation for ball carrier and tacklers only
test_anim <- 
   tracking_ball_carrier %>% 
   filter(playId == 80, gameId == 2022090800) %>% 
   tidyr::fill(event) %>% 
   mutate(frame_event = paste0(1000 + frameId, "_", event)) %>% 
   ggplot() + 
   geom_point(aes(x = bc_x, y = bc_y), 
              size = 5, color = 'blue', fill = 'white', alpha = 0.5) + 
   geom_text(aes(x = bc_x, y = bc_y, label = bc_number), 
             color = 'white', alpha = 0.75, size = 3) +
   geom_point(data = tracking_tacklers %>% 
                 filter(playId == 80, gameId == 2022090800) %>% 
                 tidyr::fill(event) %>% 
                 mutate(frame_event = paste0(1000 + frameId, "_", event)), 
              aes(x = tk_x, y = tk_y), 
              color = 'salmon', fill = 'white', alpha = 0.5, size = 5) +
   geom_text(data = tracking_tacklers %>% 
                filter(playId == 80, gameId == 2022090800) %>% 
                tidyr::fill(event) %>% 
                mutate(frame_event = paste0(1000 + frameId, "_", event)), 
             aes(x = tk_x, y = tk_y, label = tk_number), 
             color = 'white', alpha = 0.75, size = 3) +
   coord_equal(xlim = c(0, 120), ylim = c(0, 53.33)) +
   #coord_cartesian(xlim = c(0,120), ylim = c(0,53.33)) +
   theme_minimal() +
   labs(title = "Frame_Event: {closest_state}") +
   transition_states(frame_event) +
   ease_aes('linear') +
   NULL

gganimate::animate(test_anim, 
                   #fps = 10, 
                   #start_pause = 10, 
                   end_pause = 30, rewind = TRUE,
                   duration = 5
                   #nframes = 30
                   #width = 720
                   )

# plotly animation
test_plotly_base <- 
   tracking %>% 
   filter(playId == 101, gameId == 2022090800) %>% 
   plotly::plot_ly(x = ~x,
                   y = ~y,
                   #color = ~club, 
                   #symbol = ~club,
                   text = ~displayName,
                   size = I(150),
                   hoverinfo = 'text')

test_plotly_base %>% 
   plotly::add_markers(color = ~club,
                       symbol = ~club,
                       alpha = 0.25,
                       alpha_stroke = ~frameId/,
                       #size = 4,
                       showlegend = TRUE) %>% 
   plotly::add_markers(color = ~club,
                       symbol = ~club,
                       frame = ~frameId, 
                       ids = ~displayName) %>% 
   plotly::animation_opts(frame = 100, transition = 0, redraw = FALSE)

%>% 
   plotly::animation_slider(currentvalue = list(prefix = "frame: ", 
                                                font = list(color = "red")))

test_plotly

# add identifiers for if a player is:
#  - player who is stated as ball carrier on a given play 
#     - pass plays will still only have 1 player listed as ball carrier
#        ==> QBs will not have this label, even if they technically carried ball first
#  - player who made a tackle/assist/FF/missed tackle on a given play

tracking %>% 
   distinct(gameId, playId, nflId, displayName, jerseyNumber, club) %>% 
   left_join(players_adj %>% select(nflId:displayName),
             by = join_by(nflId, displayName, club, jerseyNumber)) %>% 
   left_join(plays_adj %>% select(gameId, playId, ballCarrierId, playDescription, 
                                  possessionTeam, defensiveTeam, yardline_100, yardline_fDown),
             by = join_by(gameId, playId)) %>% 
   arrange(gameId, playId, isOffense, nflId) %>% 
   print(n = 50)

plays_adj %>% 
   select(gameId, playId, ballCarrierId, playDescription, possessionTeam, defensiveTeam,
          yardline_100, yardline_fDown) %>% 
   left_join(tracking, by = join_by(gameId, playId)) %>% 
   mutate()

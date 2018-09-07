# Attribution -------------------------------------------------------------

# Manny's HTM scrape || 09/15/17 #
# This code was written by Matthew Barlowe of @matt_barlowe on Twitter. #
# All credit to him for creation of the script which can found at https://github.com/mcbarlowe/xGmodel #
# XGoals # Last edit: Matt (2018-01-14)
# Description: XGoals produce an introductory expected goals model
#' Attribution statement
#' @export
xg.attribution <- function() {
  return("# This code was written by Matthew Barlowe of @matt_barlowe on Twitter.\n# All credit to him for creation of the script which can found at https://github.com/mcbarlowe/xGmodel \n# Last edit: Matt (2018-01-14) \n# Description: XGoals produce an introductory expected goals model. ")
}

xg.train_model <- function(data_dir = "./data", train_season = c(2016:2017), test_season = c(2018)) {
  coltype<-'iicciidccccccciiiccccccccccccccccciiiiccc'

  train<-tibble::tibble()
  for(s in train_season){
    f<-file.path(data_dir, s, 'compiled.pbp')
    pbp<-readr::read_delim(f, delim='|', col_types = coltype)
    train<-dplyr::bind_rows(train, pbp)
  }

  test<-tibble::tibble()
  for(s in test_season){
    f<-file.path(data_dir, s, 'compiled.pbp')
    pbp<-readr::read_delim(f, delim='|', col_types = coltype)
    train<-dplyr::bind_rows(test, pbp)
  }

  train<-xg.prep_data(train)

  test<-xg.prep_data(test)

  xGmodel <- glm(is_goal ~ poly(distance, 3, raw = TRUE) +
                   poly(shot_angle, 3, raw = TRUE) + event_detail +
                   shooter_strength +
                   is_rebound + is_rush,
                 data = Train_Fenwick_Data,
                 family = binomial(link = 'logit'))

  save(xGmodel, file = "xGmodel.rda")


}

xg.prep_data<-function(pbp){
  pbp<-is_home(pbp)
  pbp<-is_home(pbp)

  pbp <- pbp %>% dplyr::group_by(game_id) %>%
    dplyr::arrange(event_index, .by_group = TRUE) %>%
    dplyr::mutate(time_diff = game_seconds - lag(game_seconds))


  pbp$time_diff[is.na(pbp$time_diff)] <- 0
  pbp$is_home[is.na(pbp$is_home)] <- 0

  pbp$is_rebound <- ifelse(pbp$time_diff < 3 &
                             pbp$event_type %in% fenwick_events &
                             pbp$event_team == lag(pbp$event_team),
                           1, 0)

  pbp$is_rebound[is.na(pbp$is_rebound)] <- 0

  pbp$is_rush <- ifelse(pbp$time_diff < 4 &
                          lag(abs(pbp$coords_x)) < 25 &
                          pbp$event_type %in% fenwick_events,
                        1, 0)

  pbp$is_rebound[is.na(pbp$time_diff)] <- 0
  pbp$is_rush[is.na(pbp$is_rush)] <- 0
  pbp<- filter(pbp, event_type %in% c("SHOT", "MISS", "GOAL"))
  pbp <- filter(pbp, !is.na(event_detail))

  pbp$event_detail<- as.factor(pbp$event_detail)
  pbp <- filter(pbp, coords_x != 'NA' & coords_y != 'NA')

  pbp$coords_y <- ifelse(pbp$coords_x < 0, -1 * pbp$coords_y, pbp$coords_y)

  pbp$shot_angle <- (asin(abs(pbp$coords_y)/sqrt((87.95 - abs(pbp$coords_x))^2 + pbp$coords_y^2))*180)/ 3.14

  pbp$shot_angle <- ifelse(abs(pbp$coords_x) > 88, 90 + (180-(90 + pbp$shot_angle)), pbp$shot_angle)

  pbp$distance <- sqrt((87.95 - abs(pbp$coords_x))^2 + pbp$coords_y^2)

  even_strength <- c('5v5', '4v4', '3v3', 'EvE', '5v0')
  home_advantage <- c('5v4', '5v3', '4v3')
  away_advantage <- c('4v5', '3v5', '3v4')
  home_empty_net <- c('Ev5', 'Ev4', 'Ev3')
  away_empty_net <- c('5vE', '4vE', '3vE')

  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% even_strength, 'EV', NA)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% home_advantage & pbp$is_home == 1, 'PP', pbp$shooter_strength)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% away_advantage & pbp$is_home == 1, 'SH', pbp$shooter_strength)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% home_advantage & pbp$is_home == 0, 'SH', pbp$shooter_strength)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% away_advantage & pbp$is_home == 0, 'PP', pbp$shooter_strength)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% home_empty_net & pbp$is_home == 1, 'PP', pbp$shooter_strength)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% home_empty_net & pbp$is_home == 0, 'EN', pbp$shooter_strength)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% away_empty_net & pbp$is_home == 1, 'EN', pbp$shooter_strength)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% away_empty_net & pbp$is_home == 0, 'PP', pbp$shooter_strength)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% home_empty_net & pbp$is_home == 1, 'PP', pbp$shooter_strength)
  pbp$shooter_strength <- ifelse(pbp$game_strength_state %in% c('Ev0', '0vE'), 'PS', pbp$shooter_strength)

  return(pbp)


}

is_home <- function(dataframe){
  dataframe$is_home <- ifelse(dataframe$event_team ==
                                dataframe$home_team, 1 , 0)
  return(dataframe)
}

is_goal <- function(dataframe){
  dataframe$is_goal <- ifelse(dataframe$event_type == "GOAL", 1, 0)
  return(dataframe)
}

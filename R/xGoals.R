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

#' Train xG model
#'
#' @param data_dir the data directory with compiled .pbp files in season subfolders, also where to save model
#' @param train_season vector of seasons (as season end year as integer) to use for training
#' @param test_season vector of seasons (as season end year as integer) to use for testing
#' @param save_model Whether to save the model to disk
#' @param evaluate whether to evaluate the model's performance
#'
#' @return the xGmodel
#' @export
xg.train_model <- function(data_dir = "./data", train_season = c(2016:2017), test_season = c(2018), save_model = TRUE, evaluate = TRUE) {
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
    test<-dplyr::bind_rows(test, pbp)
  }

  train<-xg.prep_data(train)

  test<-xg.prep_data(test)

  xGmodel <- stats::glm(is_goal ~ poly(distance, 3, raw = TRUE) +
                        poly(shot_angle, 3, raw = TRUE) + event_detail +
                        shooter_strength +
                        is_rebound + is_rush,
                        data = train,
                        family = binomial(link = 'logit'),
                        model = FALSE, x = FALSE, y = FALSE)

  xGmodel <- compress_model(xGmodel)

  if(evaluate){
    test$xG <- stats::predict(xGmodel, test, type = 'response')
    cat("Area under the curve:", pROC::auc(pROC::roc(is_goal ~ xG, data = test)))
  }

  if(save_model){
    save(xGmodel, file = file.path(data_dir, "xGmodel.rda"))
    message("Model saved at ", file.path(data_dir, "xGmodel.rda", "."))
  }

  return(xGmodel)
}

xg.prep_data<-function(pbp){
  fenwick_events <- c('SHOT', 'MISS', 'GOAL')

  pbp<-is_home(pbp)
  pbp<-is_goal(pbp)

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
  pbp <- filter(pbp, event_type %in% fenwick_events)
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

xg.by_player<-function(model, season){
  NULL
}

xg.by_team<-function(model, season){
  NULL
}

xg.by_coord<-function(model, season){
  NULL
}

xs.by_team<-function(model, season){
  NULL
}

xs.by_player<-function(model, season){
  NULL
}

compress_model<-function(m){
  m$y <- c()
  m$model <- c()

  m$residuals <- c()
  m$effects <- c()
  m$linear.predictors <- c()
  m$weights <- c()
  m$qr$qr <- c()
  m$prior.weights <- c()

  return(m)
}

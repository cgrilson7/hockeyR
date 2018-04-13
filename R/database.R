#' Merge Season into Database
#' @description The volume of data can be too much for personal computers to store in memory. Use of a database allows analysis of everything at once. This will collapse a season into a database.
#'
#' @param season_dir Directory of the season
#' @param con A connection to the database (see \code{\link{db.get_db_connection}()}).
#' @param what Which of roster, shifts and/or pbp to collapse into the database
#'
#' @export
db.merge_season_to_db<-function(season_dir, con, what=c('roster','shifts','pbp')){
  coltype<-list(roster = 'ccccciicicccccc', shifts = 'iiccccccciicccccdd', pbp= 'iicciidccccccciiiccccccccccccccccciiiiccc')
  colrequired<-list(roster = c("team_name", "team", "venue", "num_first_last", "game_date",
                               "game_id", "season", "session", "player_number", "team_num",
                               "first_name", "last_name", "player_name", "name_match", "player_position"),
                    shifts = c("shift_number", "game_period", "shift_start", "shift_end",
                               "shift_duration", "num_first_last", "team", "venue", "game_date",
                               "game_id", "season", "session", "home_team", "away_team", "player_name",
                               "team_num", "start_seconds", "end_seconds"),
                    pbp = c("season", "game_id", "game_date", "session", "event_index",
                            "game_period", "game_seconds", "event_type", "event_description",
                            "event_detail", "event_team", "event_player_1", "event_player_2",
                            "event_player_3", "event_length", "coords_x", "coords_y", "players_substituted",
                            "home_on_1", "home_on_2", "home_on_3", "home_on_4", "home_on_5",
                            "home_on_6", "away_on_1", "away_on_2", "away_on_3", "away_on_4",
                            "away_on_5", "away_on_6", "home_goalie", "away_goalie", "home_team",
                            "away_team", "home_skaters", "away_skaters", "home_score", "away_score",
                            "game_score_state", "game_strength_state", "highlight_code")
  )
  what <- what[what %in% c('roster','shifts','pbp')]
  for (w in what){
    cat("Parsing", w, "   ")
    files<-list.files(path = season_dir, pattern = w, full.names = TRUE)
    if(length(f) == 0){
      stop('No files of type ', w, ' to import from ', season_dir, '.')
    }
    if(!dplyr::db_has_table(con, w)){
      stop('Table ', w, ' does not exist in the database. Please create table with db.create_table() before adding data.')
    }
    for (f in files){
      game_data<-readr::read_delim(f, delim='|', col_types = coltype[[w]])
      game_data <- game_data[colnames(game_data) %in% colrequired[[w]], ]
      dplyr::db_write_into(con, w, game_data)
    }
    cat("\r")
  }
}

#' Get Database Connection
#' @description A convenience function for connecting to the database using RPostgreeSQL
#'
#' @param user the username to connect to the database
#' @param password the password to connect to the database
#' @param dbname the database's name
#' @param dbtype the database type. Only tested with PostgreSQL databases
#'
#' @return con = a database connection for use
#' @export
db.get_db_connection<-function(user='hockey', password='analytic', dbname = 'hockeystats', dbtype='PostgreSQL'){
  con<-RPostgreSQL::dbConnect(dbtype, dbname=dbname, user=user, password=password,host = "localhost", port = 5432)
  return(con)
}

#' Create Table in DB
#' @description Create the table in the database, if it doesn't already exist
#'
#' @param con A connection to the database (see \code{\link{db.get_db_connection}()}).
#' @param what Which of roster, shifts and/or pbp table to create in the database .
#'
#' @return
#' @export
#'
#' @examples
db.create_table<-function(con, what=c('roster','shifts','pbp')){
  empty_df<-list(
    roster = data.frame(
      team_name = character(),
      team = character(),
      venue = character(),
      num_first_last = character(),
      game_date = character(),
      game_id = integer(),
      season = integer(),
      session = character(),
      player_number = integer(),
      team_num = character(),
      first_name = character(),
      last_name = character(),
      player_name = character(),
      name_match = character(),
      player_position = character()
    ),
    shifts = data.frame(
      shift_number = integer(),
      game_period = integer(),
      shift_start = character(),
      shift_end = character(),
      shift_duration = character(),
      num_first_last = character(),
      team = character(),
      venue = character(),
      game_date = character(),
      game_id = integer(),
      season = integer(),
      session = character(),
      home_team = character(),
      away_team = character(),
      player_name = character(),
      team_num = character(),
      start_seconds = integer(),
      end_seconds = integer()
    ),
    pbp = data.frame(
      season = integer(),
      game_id = integer(),
      game_date = character(),
      session = character(),
      event_index = integer(),
      game_period = integer(),
      game_seconds = integer(),
      event_type = character(),
      event_description = character(),
      event_detail = character(),
      event_team = character(),
      event_player_1 = character(),
      event_player_2 = character(),
      event_player_3 = character(),
      event_length = integer(),
      coords_x = integer(),
      coords_y = integer(),
      players_substituted = character(),
      home_on_1 = character(),
      home_on_2 = character(),
      home_on_3 = character(),
      home_on_4 = character(),
      home_on_5 = character(),
      home_on_6 = character(),
      away_on_1 = character(),
      away_on_2 = character(),
      away_on_3 = character(),
      away_on_4 = character(),
      away_on_5 = character(),
      away_on_6 = character(),
      home_goalie = character(),
      away_goalie = character(),
      home_team = character(),
      away_team = character(),
      home_skaters = integer(),
      away_skaters = integer(),
      home_score = integer(),
      away_score = integer(),
      game_score_state = character(),
      game_strength_state = character(),
      highlight_code = integer()
    )
  )
  for (w in what){
    if(dplyr::db_has_table(con, w)){
      stop('Table ', w, ' already exists in database.')
    }
    RPostgreSQL::dbGetQuery(con,
                            RPostgreSQL::postgresqlBuildTableDefinition(
                              'PostgreSQL',
                              name = w,
                              row.names = FALSE,
                              obj = empty_df[[w]]
                              ))
  }
}

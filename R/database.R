#' Merge Season into Database
#' @description The volume of data can be too much for personal computers to store in memory. Use of a database allows analysis of everything at once. This will collapse a season into a database.Tables should be created before this. For more details see the help vignette: \code{vignette("Database-Setup", package = "hockeyR")}.
#'
#' @param season_dir Directory of the season
#' @param con A connection to the database (see \code{\link{db.get_db_connection}()}).
#' @param what Which of roster, shifts and/or pbp to collapse into the database
#'
#' @export
db.merge_season_to_db<-function(season_dir, con, what=c('roster','shifts','pbp')){
  coltype<-list(roster = 'ccccciicicccccc', 
                shifts = 'iiccccccciicccccdd', 
                pbp= 'iicciidccccccciiiccccccccccccccccciiiiccc')
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
    if(length(files) == 0){
      stop('No files of type ', w, ' to import from ', season_dir, '.')
    }
    if(!RPostgreSQL::dbExistsTable(con, w)){
      stop('Table ', w, ' does not exist in the database. Please create table with db.create_table() before adding data.')
    }
    for (f in files){
      game_data<-readr::read_delim(f, delim='|', col_types = coltype[[w]])
      game_data <- game_data[colnames(game_data) %in% colrequired[[w]], ]
      RPostgreSQL::dbWriteTable(con, w, game_data, append=TRUE, row.names = FALSE)
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
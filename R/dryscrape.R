# This can scrape from 20032004 onward

# Attribution -------------------------------------------------------------

# Manny's HTM scrape || 09/15/17 #
# This code was written by Emmanuel Perry of @mannyelk on twitter and creator of corsica.hockey. #
# All credit to him for creation of the script and can found at github.com/mannyelk #
# DRYSCRAPE # Last edit: Manny (2017-07-02)
# Description Dryscrape contains all functions and tools related to scraping data for Corsica
ds.attribution <- function() {
  message("# Manny's HTM scrape - 09//15//17 \n# This code was written by Emmanuel Perry of @mannyelk on twitter and creator of corsica.hockey.\n# All credit to him for creation of the script and can found at github.com/mannyelk \n# DRYSCRAPE # Last edit: Manny (2017-07-02)\n# Description Dryscrape contains all functions and tools related to scraping data for Corsica")
}


# Data Structures ---------------------------------------------------------
#' Get all games
#'
#' @return a list of games as character values
#' @export
ds.get_all_games <- function() {
  return(as.character(c(
    20001:21271, 30111:30117, 30121:30127, 30131:30137, 30141:30147, 30151:30157,
    30161:30167, 30171:30177, 30181:30187, 30211:30217, 30221:30227, 30231:30237, 30241:30247, 30311:30317,
    30321:30327, 30411:30417
  )))
}

#' Get User Agents
#'
#' @return Returns a list of random user agents ca. 2017
#' @export
ds.get_user_agents <- function() {
  return(c(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.5.2171.95 Safari/537.36",
    "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36"
  ))
}

ds.pbp_colnames <- c(
  "season", "game_id", "game_date", "session", "event_index", "game_period", "game_seconds",
  "event_type", "event_description", "event_detail", "event_team", "event_player_1", "event_player_2",
  "event_player_3", "event_length", "coords_x", "coords_y", "players_substituted", "home_on_1", "home_on_2",
  "home_on_3", "home_on_4", "home_on_5", "home_on_6", "away_on_1", "away_on_2", "away_on_3", "away_on_4",
  "away_on_5", "away_on_6", "home_goalie", "away_goalie", "home_team", "away_team", "home_skaters", "away_skaters",
  "home_score", "away_score", "game_score_state", "game_strength_state", "highlight_code"
)

ds.team_list <- c(
  "ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA",
  "L.A", "MIN", "MTL", "N.J", "NSH", "NYI", "NYR", "OTT", "PHI", "PIT", "S.J", "STL", "T.B", "TOR", "VAN",
  "WPG", "WSH", "PHX", "ATL", "VGK", "L.V"
)

ds.espn_codes <- data.frame(event = c(
  "FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL", "STOP",
  "PRDY", "PSTR", "PEND", "PERD", "SOC", "GEnd", "SOut", "error", "TAKE", "GIVE", "early intermission",
  "nothing", "nothing"
), code = as.character(c(
  502, 503, 504, 505, 506, 507, 508, 509, 516, 517, 518,
  519, 520, 521, 522, 0, 9999, 1401, 1402, -2147483648, 1, 5
)))


# Meta Functions ----------------------------------------------------------

#' Get PBP
#' @description get_pbp() imports the PBP page corresponding to a given year and game ID and returns a list object.
#'
#' @param season a season
#' @param game_id a single game id
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#' @importFrom dplyr %>%
#'
#' @return a list object
#' @keywords internal
ds.get_pbp <- function(season, game_id, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  url <- paste("http://www.nhl.com/scores/htmlreports/",
    as.character(season),
    "/PL0",
    as.character(game_id),
    ".HTM",
    sep = ""
  )

  raw_text <- NULL

  while (class(raw_text) != "character" & try_tolerance > 0) {
    try(
      url %>%
        RCurl::getURL(
          header = FALSE,
          .opts = RCurl::curlOptions(
            referer = "nhl.com",
            verbose = FALSE,
            followLocation = TRUE,
            useragent = agents[sample(1:length(agents), 1)]
          )
        )
    ) ->
    raw_text

    try_tolerance <- try_tolerance - 1
  }

  html <- xml2::read_html(raw_text)

  all <- rvest::html_nodes(html, "td")
  body <- rvest::html_nodes(html, ".bborder")
  full_text <- rvest::html_text(all)
  body_text <- rvest::html_text(body)

  pbp_list <- list(full_text, body_text)

  return(pbp_list)
}

#' Get Shifts
#' @description get_shifts() imports the shift report page corresponding to a given year, game ID and venue and returns a list object.
#'
#' @param season a season
#' @param game_id a single game id
#' @param venue home or away
#' @param source htm or json
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#' @importFrom dplyr %>%
#'
#' @return a list object
#' @keywords internal
ds.get_shifts <- function(season, game_id, venue, source, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  if (tolower(source) == "htm") {
    if (tolower(venue) == "home") {
      url <- paste("http://www.nhl.com/scores/htmlreports/",
        season,
        "/TH0",
        game_id,
        ".HTM",
        sep = ""
      )
    } else if (tolower(venue) == "away") {
      url <- paste("http://www.nhl.com/scores/htmlreports/",
        season,
        "/TV0",
        game_id,
        ".HTM",
        sep = ""
      )
    }

    raw_text <- NULL

    while (class(raw_text) != "character" & try_tolerance > 0) {
      try(
        url %>%
          RCurl::getURL(
            header = FALSE,
            .opts = RCurl::curlOptions(
              referer = "nhl.com",
              verbose = FALSE,
              followLocation = TRUE,
              useragent = agents[sample(1:length(agents), 1)]
            )
          )
      ) ->
      raw_text

      try_tolerance <- try_tolerance - 1
    }

    html <- xml2::read_html(raw_text)

    outer_text <- rvest::html_text(rvest::html_nodes(html, ".border"))
    inner_text <- rvest::html_text(rvest::html_nodes(html, ".bborder"))

    shifts_list <- list(outer_text, inner_text)

    return(shifts_list)
  } else if (tolower(source) == "json") {
    year <- substr(season, 0, 4)

    url <- paste("http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=",
      as.character(year),
      "0",
      as.character(game_id),
      sep = ""
    )

    raw_text <- NULL
    json_check <- NULL

    while ({
      class(raw_text) != "character" | class(json_check) != "list"
    } & try_tolerance > 0) {
      try(
        url %>%
          RCurl::getURL(
            header = FALSE,
            .opts = RCurl::curlOptions(
              referer = "nhl.com",
              verbose = FALSE,
              followLocation = TRUE,
              useragent = agents[sample(1:length(agents), 1)]
            )
          )
      ) ->
      raw_text

      json_check <- try(rjson::fromJSON(raw_text), silent = TRUE)

      try_tolerance <- try_tolerance - 1
    }

    raw_json <- try(rjson::fromJSON(raw_text), silent = TRUE)

    if (class(raw_json) == "try-error") {
      raw_json <- NULL
    }

    return(raw_json)
  } else if (tolower(source) == "pbp") {
    shift_pbp <- ds.get_pbp(season_, game_id_, try_tolerance, agents)
    roster <- ds.parse_roster(ds.get_roster(season_, game_id_, try_tolerance, agents))

    shift_pbp_body <- shift_pbp[[2]]
    matrix(shift_pbp_body,
      byrow = TRUE,
      ncol = 8
    ) %>%
      data.frame() %>%
      dplyr::filter(X2 != "Per") ->
    shift_pbp_raw

    home_team_ <- gsub(" On Ice", "", shift_pbp_body[8])
    away_team_ <- gsub(" On Ice", "", shift_pbp_body[7])

    roster$team[roster$venue == "Home"] <- home_team_
    roster$team[roster$venue == "Away"] <- away_team_

    shift_pbp_raw %>%
      dplyr::filter(
        X4 != "",
        X2 != ""
      ) %>%
      dplyr::mutate(
        away_team = away_team_,
        home_team = home_team_,
        time_elapsed = regmatches(X4, regexpr("[0-9]+:[0-9]{2}", X4)),
        game_seconds = 1200 * (nabs(X2) - 1) + ds.seconds_from_ms(time_elapsed),
        away_on_ice = lapply(stringr::str_match_all(X7, pattern = "[0-9]+"), function(x) paste(sort(x), collapse = ",")),
        home_on_ice = lapply(stringr::str_match_all(X8, pattern = "[0-9]+"), function(x) paste(sort(x), collapse = ","))
      ) %>%
      dplyr::rename(game_period = X2) %>%
      dplyr::select(
        game_period,
        home_team,
        away_team,
        time_elapsed,
        game_seconds,
        home_on_ice,
        away_on_ice
      ) %>%
      tidyr::separate(away_on_ice, c("a1", "a2", "a3", "a4", "a5", "a6"), extra = "drop", sep = ",", convert = TRUE, fill = "right") %>%
      tidyr::separate(home_on_ice, c("h1", "h2", "h3", "h4", "h5", "h6"), extra = "drop", sep = ",", convert = TRUE, fill = "right") %>%
      dplyr::mutate(
        a1 = ifelse(is.na(a1), no = paste0(away_team, a1), yes = NA),
        a2 = ifelse(is.na(a2), no = paste0(away_team, a2), yes = NA),
        a3 = ifelse(is.na(a3), no = paste0(away_team, a3), yes = NA),
        a4 = ifelse(is.na(a4), no = paste0(away_team, a4), yes = NA),
        a5 = ifelse(is.na(a5), no = paste0(away_team, a5), yes = NA),
        a6 = ifelse(is.na(a6), no = paste0(away_team, a6), yes = NA),
        h1 = ifelse(is.na(h1), no = paste0(home_team, h1), yes = NA),
        h2 = ifelse(is.na(h2), no = paste0(home_team, h2), yes = NA),
        h3 = ifelse(is.na(h3), no = paste0(home_team, h3), yes = NA),
        h4 = ifelse(is.na(h4), no = paste0(home_team, h4), yes = NA),
        h5 = ifelse(is.na(h5), no = paste0(home_team, h5), yes = NA),
        h6 = ifelse(is.na(h6), no = paste0(home_team, h6), yes = NA)
      ) %>%
      data.frame() -> shift_pbp_df

    shifts_df <- ds.parse_shifts_from_pbp(shift_pbp_df, roster) %>%
      dplyr::mutate(
        game_date = NA,
        game_id = paste0(season_, "0", game_id_),
        season = NA,
        session = NA,
        player_id = NA
      ) %>%
      dplyr::select(
        game_date,
        game_id,
        season,
        session,
        shift_number,
        shift_period,
        shift_start,
        shift_end,
        shift_duration,
        team,
        player_id,
        player_name_first,
        player_name_last
      )

    return(shifts_df)
  }
}

#' Get Roster
#' @description get_roster() imports the Roster page corresponding to a given year and game ID and returns a character vector
#'
#' @param season a season
#' @param game_id a single game id
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#' @importFrom dplyr %>%
#'
#' @return character vector
#' @keywords internal
ds.get_roster <- function(season, game_id, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  url <- paste("http://www.nhl.com/scores/htmlreports/",
    as.character(season),
    "/RO0",
    as.character(game_id),
    ".HTM",
    sep = ""
  )

  raw_text <- NULL

  while (class(raw_text) != "character" & try_tolerance > 0) {
    try(
      url %>%
        RCurl::getURL(
          header = FALSE,
          .opts = RCurl::curlOptions(
            referer = "nhl.com",
            verbose = FALSE,
            followLocation = TRUE,
            useragent = agents[sample(1:length(agents), 1)]
          )
        )
    ) ->
    raw_text

    try_tolerance <- try_tolerance - 1
  }

  html <- xml2::read_html(raw_text)

  all <- rvest::html_nodes(html, "tr")
  full_text <- rvest::html_text(all)

  return(full_text)
}

#' Get Highlights
#' @description get_highlights() imports the highlights page corresponding to a given year and game ID and returns a JSON list object.
#'
#' @param season a season
#' @param game_id a single game id
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#' @importFrom dplyr %>%
#'
#' @return a json list object
#' @keywords internal
ds.get_highlights <- function(season, game_id, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  url <- paste("http://live.nhle.com/GameData/",
    as.character(season),
    "/",
    substr(as.character(season), 0, 4),
    "0",
    as.character(game_id),
    "/gc/gcgm.jsonp",
    sep = ""
  )

  raw_text <- NULL
  json_check <- NULL

  while ({
    class(raw_text) != "character" | class(json_check) != "list"
  } & try_tolerance > 0) {
    try(
      url %>%
        RCurl::getURL(
          header = FALSE,
          .opts = RCurl::curlOptions(
            referer = "nhl.com",
            verbose = FALSE,
            followLocation = TRUE,
            useragent = agents[sample(1:length(agents), 1)]
          )
        )
    ) ->
    raw_text

    clean_text <- gsub("^.+?\\(\\{", "\\{", raw_text)
    json_check <- try(rjson::fromJSON(clean_text), silent = TRUE)

    try_tolerance <- try_tolerance - 1
  }

  clean_text <- gsub("^.+?\\(\\{", "\\{", raw_text)

  raw_json <- try(rjson::fromJSON(clean_text), silent = TRUE)

  if (class(raw_json) == "try-error") {
    raw_json <- NULL
  }

  return(raw_json)
}

#' Get Coordinates
#' @description get_coordinates() imports the event coordinates corresponding to a given year and game ID and returns a list object.
#'
#' @param season a season
#' @param game_id a single game id
#' @param source espn or nhl
#' @param date the date of the game (for espn)
#' @param away_team the away team
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#' @importFrom dplyr %>%
#'
#' @return list object
#' @keywords internal
ds.get_coordinates <- function(season, game_id, source, date, away_team, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  if (tolower(source) == "espn") {
    day <- gsub("-", "", as.character(date))

    url <- paste("http://scores.espn.go.com/nhl/scoreboard?date=",
      day,
      sep = ""
    )

    raw_text <- NULL

    while (class(raw_text) != "character" & try_tolerance > 0) {
      try(
        url %>%
          RCurl::getURL(
            header = FALSE,
            .opts = RCurl::curlOptions(
              referer = "sports.espn.go.com",
              verbose = FALSE,
              followLocation = TRUE,
              useragent = agents[sample(1:length(agents), 1)]
            )
          )
      ) ->
      raw_text

      try_tolerance <- try_tolerance - 1
    }

    game_ids <- unique(unlist(regmatches(raw_text, gregexpr("gameId=[0-9]+", raw_text))))
    teams <- toupper(gsub("team/_/name/|>|</div>", "", unique(unlist(regmatches(raw_text, gregexpr("team/_/name/[a-zA-Z]+|>(Coyotes|Thrashers)</div>", raw_text))))))

    teams[which(teams == "PHX")] <- "ARI"
    teams[which(teams == "TB")] <- "T.B"
    teams[which(teams == "NJ")] <- "N.J"
    teams[which(teams == "SJ")] <- "S.J"
    teams[which(teams == "LA")] <- "L.A"
    teams[which(teams == "COYOTES")] <- "ARI"
    teams[which(teams == "THRASHERS")] <- "ATL"

    if (as.numeric(season) < 20110000) {
      teams[which(teams == "WPG")] <- "ATL"
    }

    matrix(unique(teams),
      byrow = TRUE,
      ncol = 2
    ) %>%
      data.frame() ->
    team_mat

    cbind(
      game_ids,
      team_mat
    ) %>%
      data.frame() %>%
      dplyr::rename(
        awayteam = X1,
        hometeam = X2
      ) ->
    url_match

    game_url <- dplyr::first(as.character(url_match$game_ids[which(as.character(url_match$awayteam) == as.character(away_team) | as.character(url_match$hometeam) == as.character(away_team))]))

    url <- paste("http://sports.espn.go.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&",
      game_url,
      sep = ""
    )

    raw_text <- NULL

    while (class(raw_text) != "character" & try_tolerance > 0) {
      try(
        url %>%
          RCurl::getURL(
            header = FALSE,
            .opts = RCurl::curlOptions(
              referer = "sports.espn.go.com",
              verbose = FALSE,
              followLocation = TRUE,
              useragent = agents[sample(1:length(agents), 1)]
            )
          )
      ) ->
      raw_text

      try_tolerance <- try_tolerance - 1
    }

    events <- unlist(regmatches(raw_text, gregexpr("<Play.*?/Play>", raw_text)))

    if (length(events) > 0) {
      do.call(
        cbind,
        strsplit(events, "[\\[~]")
      ) %>%
        t() %>%
        data.frame() %>%
        dplyr::select(5, 3, 4, 6, 7, 11) ->
      event_mat

      colnames(event_mat) <- c(
        "event_code",
        "xcoord",
        "ycoord",
        "time",
        "period",
        "description"
      )

      event_mat$event_type <- ds.espn_codes$event[match(event_mat$event_code, ds.espn_codes$code)]
      event_mat$seconds <- 1200 * (nabs(event_mat$period) - 1) + ds.seconds_from_ms(event_mat$time)

      return(event_mat)
    } else {
      return(NULL)
    }
  } else if (tolower(source) == "nhl") {
    year <- substr(season, 0, 4)

    url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
      as.character(year),
      "0",
      as.character(game_id),
      "/feed/live?site=en_nhl",
      sep = ""
    )

    raw_text <- NULL
    json_check <- NULL

    while ({
      class(raw_text) != "character" | class(json_check) != "list"
    } & try_tolerance > 0) {
      try(
        url %>%
          RCurl::getURL(
            header = FALSE,
            .opts = RCurl::curlOptions(
              referer = "nhl.com",
              verbose = FALSE,
              followLocation = TRUE,
              useragent = agents[sample(1:length(agents), 1)]
            )
          )
      ) ->
      raw_text

      json_check <- try(rjson::fromJSON(raw_text), silent = TRUE)

      try_tolerance <- try_tolerance - 1
    }

    raw_json <- try(rjson::fromJSON(raw_text), silent = TRUE)

    if (class(raw_json) == "try-error") {
      return(NULL)
    } else {
      event_mat <- dcapply(raw_json$liveData$plays$allPlays,
        ds.parse_event,
        "rbind",
        cores = 1
      )

      event_mat$game_id <- na_if_null(nabs(raw_json$gameData$game$pk))

      return(event_mat)
    }
  }
}

#' Get Team Profile
#' @description get_team_profile() imports the team profile page corresponding to a given team ID and returns a JSON list object.
#'
#' @param team_id the team id to get
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#' @importFrom dplyr %>%
#'
#' @return json list
#' @keywords internal
ds.get_team_profile <- function(team_id, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  url <- paste("https://statsapi.web.nhl.com/api/v1/teams/",
    as.character(team_id),
    sep = ""
  )

  raw_text <- NULL
  json_check <- NULL

  while ({
    class(raw_text) != "character" | class(json_check) != "list"
  } & try_tolerance > 0) {
    try(
      url %>%
        RCurl::getURL(
          header = FALSE,
          .opts = RCurl::curlOptions(
            referer = "nhl.com",
            verbose = FALSE,
            followLocation = TRUE,
            useragent = agents[sample(1:length(agents), 1)]
          )
        )
    ) ->
    raw_text

    json_check <- try(rjson::fromJSON(raw_text))

    try_tolerance <- try_tolerance - 1
  }

  raw_json <- try(rjson::fromJSON(raw_text))

  if (class(raw_json) == "try-error") {
    raw_json <- NULL
  }

  return(raw_json)
}

#' Get Player Profile
#' @description get_player_profile() imports the player profile page corresponding to a given player ID and returns a JSON list object.
#'
#' @param player_id the player id
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#' @importFrom dplyr %>%
#'
#' @return json list objet
#' @keywords internal
ds.get_player_profile <- function(player_id, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  url <- paste("https://statsapi.web.nhl.com/api/v1/people/",
    as.character(player_id),
    sep = ""
  )

  raw_text <- NULL
  json_check <- NULL

  while ({
    class(raw_text) != "character" | class(json_check) != "list"
  } & try_tolerance > 0) {
    try(
      url %>%
        RCurl::getURL(
          header = FALSE,
          .opts = RCurl::curlOptions(
            referer = "nhl.com",
            verbose = FALSE,
            followLocation = TRUE,
            useragent = agents[sample(1:length(agents), 1)]
          )
        )
    ) ->
    raw_text

    json_check <- try(rjson::fromJSON(raw_text))

    try_tolerance <- try_tolerance - 1
  }

  raw_json <- try(rjson::fromJSON(raw_text))

  if (class(raw_json) == "try-error") {
    raw_json <- NULL
  }

  return(raw_json)
}

#' Get Schedule
#' @description get_schedule() imports the schedule page corresponding to a given date range and returns a JSON list object.
#'
#' @param start Start Date
#' @param end End Date
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#' @importFrom dplyr %>%
#'
#' @return a json list object
#' @export
ds.get_schedule <- function(start, end, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  url <- paste("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
    as.character(start),
    "&endDate=",
    as.character(end),
    sep = ""
  )

  raw_text <- NULL
  json_check <- NULL

  while ({
    class(raw_text) != "character" | class(json_check) != "list"
  } & try_tolerance > 0) {
    try(
      url %>%
        RCurl::getURL(
          header = FALSE,
          .opts = RCurl::curlOptions(
            referer = "nhl.com",
            verbose = FALSE,
            followLocation = TRUE,
            useragent = agents[sample(1:length(agents), 1)]
          )
        )
    ) ->
    raw_text

    json_check <- try(rjson::fromJSON(raw_text))

    try_tolerance <- try_tolerance - 1
  }

  raw_json <- try(rjson::fromJSON(raw_text))

  if (class(raw_json) == "try-error") {
    raw_json <- NULL
  }

  return(raw_json)
}

#' Parse PBP Event
#' @description parse_event() parses a single event from the PBP JSON object and returns a data frame
#' @param x pbp json
#' @importFrom dplyr %>%
#'
#' @return data frame
#' @keywords internal
ds.parse_event <- function(x) {
  x$players %>%
    sapply(function(p) as.character(p$player$id)) %>%
    unlist() %>%
    c(rep(NA,
      times = (4 - length(x$players))
    )) ->
  player_ids

  data.frame(
    game_date = NA,
    game_id = NA,
    season = NA,
    session = NA,
    event_id = na_if_null(nabs(x$about$eventIdx)),
    event_code = na_if_null(as.character(x$result$eventCode)),
    event_type = na_if_null(as.character(x$result$eventTypeId)),
    event_description = na_if_null(as.character(x$result$description)),
    event_detail = na_if_null(as.character(x$result$secondaryType)),
    datetime = na_if_null(as.character(lubridate::parse_date_time(x$about$dateTime, "y-m-d.H:M:S."))),
    game_period = na_if_null(nabs(x$about$period)),
    period_time_elapsed = na_if_null(as.character(x$about$periodTime)),
    period_time_remaining = na_if_null(as.character(x$about$periodTimeRemaining)),
    event_team = na_if_null(as.character(x$team$id)),
    event_player_1 = na_if_null(player_ids[1]),
    event_player_2 = na_if_null(player_ids[2]),
    event_player_3 = na_if_null(player_ids[3]),
    event_player_4 = na_if_null(player_ids[4]),
    coords_x = na_if_null(x$coordinates$x),
    coords_y = na_if_null(x$coordinates$y),
    highlight_id = na_if_null(nabs(x$about$eventId))
  ) ->
  event_df

  return(event_df)
}

#' Parse Highlight
#' @description parse_highlight() parses a single highlight from the Highlights JSON object and returns a data frame
#' @param x the highlights json
#'
#' @return a data frame.
#' @keywords internal
ds.parse_highlight <- function(x) {
  data.frame(
    game_date = NA,
    game_id = NA,
    season = NA,
    session = NA,
    event_id = na_if_null(x$id),
    highlight_id = na_if_null(x$feeds[[1]]$neulionId),
    event_team_1 = na_if_null(x$t1),
    event_team_2 = na_if_null(x$t2),
    event_period = na_if_null(x$p),
    event_seconds = na_if_null(x$sip),
    event_type = na_if_null(x$type)
  ) ->
  highlight_df

  return(highlight_df)
}

#' Parse Game
#' @description parse_game() parses a single game from the Schedule >> Date JSON object and returns a data frame.
#' @note parse_game() is an inner function for \code{parse_date()}.
#' @seealso parse_date
#'
#' @param x the schedule json
#'
#' @return a data frame
#' @keywords internal
ds.parse_game <- function(x) {
  data.frame(
    game_id = na_if_null(nabs(x$gamePk)),
    game_date = na_if_null(as.character(as.Date(x$gameDate))),
    season = na_if_null(as.character(x$season)),
    session = na_if_null(as.character(x$gameType)),
    game_status = na_if_null(as.character(x$status$detailedState)),
    away_team_id = na_if_null(nabs(x$teams$away$team$id)),
    home_team_id = na_if_null(nabs(x$teams$home$team$id)),
    game_venue = na_if_null(as.character(x$venue$name)),
    game_datetime = na_if_null(as.character(lubridate::parse_date_time(x$gameDate, "y-m-d.H:M:S.")))
  ) ->
  game_df

  return(game_df)
}

#' Parse Date
#' @description parse_date() parses a single date from the Schedule JSON object and returns a data frame.
#' @note parse_date() uses an inner function \code{parse_game()}.
#' @seealso parse_game
#'
#' @param x the schedule json with one or more games on a date
#'
#' @return data.frame
#' @keywords internal
ds.parse_date <- function(x) {
  date_df <- dcapply(x$games,
    ds.parse_game,
    "rbind",
    cores = 1
  )

  return(date_df)
}

#' Parse Player
#' @description parse_player() parses a single player from the PBP JSON object and returns a data frame.
#'
#' @param x the pbp json
#'
#' @return data frame of player
#' @keywords internal
ds.parse_player <- function(x) {
  data.frame(
    player_id = x$person$id,
    player_name = x$person$fullName,
    player_number = x$jerseyNumber,
    position = x$position$code
  ) ->
  player_df

  return(player_df)
}

#' Parse Roster
#' @description parse_roster parses a roster object from ds.get_roster() and returns a data frame.
#'
#' @param x a roster object from ds.get_roster()
#'
#' @return a data frame of the rosters
#' @keywords internal
ds.parse_roster <- function(roster) {
  roster_lists_starts <- grep(pattern = "^#\\r\\nPos\\r\\nName\\r\\n$", x = roster)
  scratches_starts <- grep(pattern = "^Scratches", x = roster)
  roster_lists <- roster_lists_starts[roster_lists_starts < scratches_starts]
  away_list <- roster[c((roster_lists[1] + 1):(roster_lists[2] - 1))]
  home_list <- roster[c((roster_lists[2] + 1):(scratches_starts - 2))]
  away_roster <- as.data.frame(
    stringr::str_match(away_list, pattern = "([0-9]+)\r\n([A-Z])\r\n([A-Z\\-\\.]+)\\s([A-Z\\-\\.\\s]+)(?:\\([C|A]\\))?\r\n")[, 2:5],
    stringsAsFactors = FALSE
  )
  colnames(away_roster) <- c("player_number", "player_position", "first_name", "last_name")
  away_roster$last_name <- as.character(trimws(away_roster$last_name))
  away_roster$venue <- "Away"
  home_roster <- as.data.frame(
    stringr::str_match(home_list, pattern = "([0-9]+)\r\n([A-Z])\r\n([A-Z\\-\\.]+)\\s([A-Z\\-\\.\\s]+)(?:\\([C|A]\\))?\r\n")[, 2:5],
    stringsAsFactors = FALSE
  )
  colnames(home_roster) <- c("player_number", "player_position", "first_name", "last_name")
  home_roster$last_name <- as.character(trimws(home_roster$last_name))
  home_roster$venue <- "Home"
  rosters <- rbind(home_roster, away_roster)

  rosters$num_first_last <- apply(rosters, 1, function(x) sprintf("%s %s, %s", as.character(x[1]), x[4], x[3]))
  return(rosters)
}

#' Seconds from (HH):MM:SS
#' @description seconds_from_ms() returns a numeric vector of representation in seconds of a given vector in M:S format.
#'
#' @param ms minute:second time
#' @importFrom dplyr %>%
#'
#' @return time in seconds
#' @keywords internal
ds.seconds_from_ms <- function(ms) {
  strsplit(as.character(ms), ":") %>%
    unlist() %>%
    nabs() %>%
    matrix(
      ncol = 2,
      byrow = TRUE
    ) ->
  time_mat

  seconds <- 60 * time_mat[, 1] + time_mat[, 2]

  return(seconds)
}


#' MM:SS from seconds
#'
#' @param seconds Number of seconds
#'
#' @return MM:SS of play time in that period (returns from 00:00 to 20:00, 1201 seconds = 00:01)
#' @keywords internal
ds.ms_from_seconds <- function(seconds) {
  seconds <- round(seconds)
  seconds <- seconds %% 1200
  return(sprintf("%02d:%02d", seconds %/% 60, seconds %% 60))
}

#' Clean Nums
#' clean_nums() returns a list of player number identifiers for a given event description
#'
#' @param x event description(s)
#'
#' @return list
#' @keywords internal
ds.clean_nums <- function(x) {
  t <- gsub("#|ONGOAL - ", "", as.character(unlist(x)))
  t2 <- list(c(t, rep(NA, times = (3 - length(t)))))
  return(t2)
}

#' Parse Shifts
#' @description parse_shifts() returns a matrix containing shift information for a single player
#'
#' @param player player to parse
#' @param venue home or away
#' @param inner inner
#' @param outer outer
#' @importFrom dplyr %>%
#'
#' @return matrix
#' @keywords internal
ds.parse_shifts <- function(player, venue, inner, outer) {
  if (tolower(venue) == "home") {
    index <- which(outer[-1] == player)

    inner[which(inner == "Shift #" | inner == "Pr\u00e9sence #Shift #")[index]:(which(inner == "SHF" | inner == "PR/SHF")[index] - 3)] %>%
      matrix(
        ncol = 6,
        byrow = TRUE
      ) %>%
      data.frame() %>%
      dplyr::mutate(
        num_first_last = player,
        venue = venue
      ) %>%
      dplyr::filter(X2 != "Per") %>%
      data.frame() ->
    shift_mat
  } else if (tolower(venue) == "away") {
    index <- which(outer[-1] == player)

    inner[which(inner == "Shift #" | inner == "Pr\u00e9sence #Shift #")[index]:(which(inner == "SHF" | inner == "PR/SHF")[index] - 3)] %>%
      matrix(
        ncol = 6,
        byrow = TRUE
      ) %>%
      data.frame() %>%
      dplyr::mutate(
        num_first_last = player,
        venue = venue
      ) %>%
      dplyr::filter(X2 != "Per") %>%
      data.frame() ->
    shift_mat
  }

  return(shift_mat)
}

#' Parse Shift
#' @description parse_shift() parses a single shift from the Shifts JSON object and returns a data frame.
#'
#' @param x a shift object
#'
#' @return a data frame of the shift
#' @keywords internal
ds.parse_shift <- function(x) {
  data.frame(
    game_date = NA,
    game_id = na_if_null(nabs(x$gameId)),
    season = NA,
    session = NA,
    shift_number = na_if_null(nabs(x$eventNumber)),
    shift_period = na_if_null(nabs(x$period)),
    shift_start = na_if_null(as.character(x$startTime)),
    shift_end = na_if_null(as.character(x$endTime)),
    shift_duration = na_if_null(as.character(x$duration)),
    team = na_if_null(as.character(x$teamAbbrev)),
    player_id = na_if_null(as.character(x$playerId)),
    player_name_fist = na_if_null(as.character(x$firstName)),
    player_name_last = na_if_null(as.character(x$lastName))
  ) ->
  shift_df

  return(shift_df)
}

#' Parse Shifts from PBP
#' @description Sometimes the .json or .htm shift reports are unavailable. This creates approximate shift table from the pbp.
#'
#' @param shift_pbp_df the formatted shift pbp dataframe
#' @param roster a roster collected from the .htm
#'
#' @return shifts in a dataframe
#' @keywords internal
ds.parse_shifts_from_pbp <- function(shift_pbp_df, roster) {
  all_shifts <- data.frame(
    shift_number = integer(),
    shift_period = integer(),
    shift_start_seconds = numeric(),
    shift_end_seconds = numeric(),
    shift_length = numeric(),
    team = character(),
    player_name_first = character(),
    player_name_last = character()
  )
  shift_number <- 0

  shift_pbp_df[1, c("h1", "h2", "h3", "h4", "h5", "h6", "a1", "a2", "a3", "a4", "a5", "a6")] <- NA

  hometeam <- roster[1, "team"]
  roster$venue <- ifelse(roster$team == hometeam, yes = "home", no = "away")
  roster %>%
    tidyr::unite(team_num, team, player_number, sep = "", remove = FALSE) -> roster
  for (p in 1:nrow(roster)) {
    player <- roster[p, ]
    player_num <- player$team_num
    if (player$venue == "home") {
      pcol <- c("h1", "h2", "h3", "h4", "h5", "h6")
    } else {
      pcol <- c("a1", "a2", "a3", "a4", "a5", "a6")
    }
    shifts <- data.frame(
      shift_number = integer(),
      shift_period = integer(),
      shift_start_seconds = numeric(),
      shift_end_seconds = numeric(),
      length = integer(),
      stringsAsFactors = FALSE
    )

    for (i in 1:(nrow(shift_pbp_df) - 1)) {
      if (player_num %in% shift_pbp_df[i, pcol]) {
        if (player_num %in% shift_pbp_df[i - 1, pcol] &&
          player_num %in% shift_pbp_df[i + 1, pcol]) {
          next
        } else if (player_num %in% shift_pbp_df[i - 1, pcol] &&
          !(player_num %in% shift_pbp_df[i + 1, pcol])) {
          # end
          shifts[nrow(shifts), "shift_end_seconds"] <- mean(c(shift_pbp_df[i, "game_seconds"], shift_pbp_df[i + 1, "game_seconds"]))
          shifts[nrow(shifts), "length"] <- shifts[nrow(shifts), "shift_end_seconds"] - shifts[nrow(shifts), "shift_start_seconds"]
        } else if (!(player_num %in% shift_pbp_df[i - 1, pcol]) &&
          (player_num %in% shift_pbp_df[i + 1, pcol])) {
          # start
          shift_number <- shift_number + 1
          shifts[nrow(shifts) + 1, ] <- c(shift_number, shift_pbp_df[i, "game_period"], mean(c(shift_pbp_df[i - 1, "game_seconds"], shift_pbp_df[i, "game_seconds"])), 0, 0)
        } else if (!(player_num %in% shift_pbp_df[i - 1, pcol]) &&
          !(player_num %in% shift_pbp_df[i + 1, pcol])) {
          shift_number <- shift_number + 1
          shifts[nrow(shifts) + 1, ] <- c(shift_number, shift_pbp_df[i, "game_period"], shift_pbp_df[i, "game_seconds"], shift_pbp_df[i, "game_seconds"], 0)
        }
      }
    }
    if (nrow(shifts) > 0) {
      shifts$team <- player$team
      shifts$player_name_first <- player$first_name
      shifts$player_name_last <- player$last_name

      all_shifts <- rbind(all_shifts, shifts)
    }
  }

  all_shifts$shift_start <- ds.ms_from_seconds(all_shifts$shift_start_seconds)
  all_shifts$shift_end <- ds.ms_from_seconds(all_shifts$shift_end_seconds)
  all_shifts$shift_duration <- ds.ms_from_seconds(all_shifts$length)

  return(all_shifts)
}

#' Is On
#' @description is_on() returns a numeric vector indicating 1 if a given player is on ice during the event corresponding to the row index in the given PBP object
#'
#' @param player a vector of players
#' @param pbp a pbp object
#' @param venue home or away
#'
#' @return numeric vector of 1 or 0 indicating player on ice
#' @keywords internal
ds.is_on <- function(player, pbp, venue) {
  regex <- paste(player,
    ",|",
    player,
    "$",
    sep = ""
  )

  if (venue == "Home") {
    data.frame(cumsum(1 * (grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$home_team) -
      1 * (grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$home_team))) ->
    is_on
  } else if (venue == "Away") {
    data.frame(cumsum(1 * (grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$away_team) -
      1 * (grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$away_team))) ->
    is_on
  }

  colnames(is_on) <- player

  return(is_on)
}

#' Is On Goalie
#' @description find_goalie() returns a vector containing all goaltenders in a given player vector
#'
#' @param players a vector of players
#' @param roster roster information
#'
#' @return a vector of goalies, if any, else NA
#' @keywords internal
ds.find_goalie <- function(players, roster) {
  index <- which(players %in% roster$team_num[which(roster$player_position == "G")])
  goalie <- na_if_null(players[index])

  return(goalie)
}

#' Fix Names
#' @description fix_names() returns a vector of player names corrected for multiple spelling variants
#'
#' @param name_vect a vector of player names
#'
#' @return a corrected vector of player names
#' @export
ds.fix_names <- function(name_vect) {
  name_vect[which(name_vect == "PK.SUBBAN" | name_vect == "P.K.SUBBAN")] <- "P.K..SUBBAN"
  name_vect[which(name_vect == "TJ.OSHIE" | name_vect == "T.J.OSHIE")] <- "T.J..OSHIE"
  name_vect[which(name_vect == "BJ.CROMBEEN" | name_vect == "B.J.CROMBEEN" | name_vect == "BRANDON.CROMBEEN")] <- "B.J..CROMBEEN"
  name_vect[which(name_vect == "ILJA.BRYZGALOV")] <- "ILYA.BRYZGALOV"
  name_vect[which(name_vect == "CAMERON.BARKER")] <- "CAM.BARKER"
  name_vect[which(name_vect == "CHRIS.VANDE VELDE")] <- "CHRIS.VANDEVELDE"
  name_vect[which(name_vect == "DANIEL.CARCILLO")] <- "DAN.CARCILLO"
  name_vect[which(name_vect == "DANIEL.CLEARY")] <- "DAN.CLEARY"
  name_vect[which(name_vect == "DANIEL.GIRARDI")] <- "DAN.GIRARDI"
  name_vect[which(name_vect == "DAVID JOHNNY.ODUYA")] <- "JOHNNY.ODUYA"
  name_vect[which(name_vect == "DAVID.BOLLAND")] <- "DAVE.BOLLAND"
  name_vect[which(name_vect == "DWAYNE.KING")] <- "DJ.KING"
  name_vect[which(name_vect == "EVGENII.DADONOV")] <- "EVGENY.DADONOV"
  name_vect[which(name_vect == "FREDDY.MODIN")] <- "FREDRIK.MODIN"
  name_vect[which(name_vect == "HARRISON.ZOLNIERCZYK")] <- "HARRY.ZOLNIERCZYK"
  name_vect[which(name_vect == "J P.DUMONT" | name_vect == "JEAN-PIERRE.DUMONT")] <- "J-P.DUMONT"
  name_vect[which(name_vect == "JEAN-FRANCOIS.JACQUES")] <- "J-F.JACQUES"
  name_vect[which(name_vect == "JONATHAN.AUDY-MARCHESSAULT")] <- "JONATHAN.MARCHESSAULT"
  name_vect[which(name_vect == "JOSHUA.HENNESSY")] <- "JOSH.HENNESSY"
  name_vect[which(name_vect == "KRISTOPHER.LETANG")] <- "KRIS.LETANG"
  name_vect[which(name_vect == "KRYSTOFER.BARCH")] <- "KRYS.BARCH"
  name_vect[which(name_vect == "MARTIN.ST LOUIS")] <- "MARTIN.ST. LOUIS"
  name_vect[which(name_vect == "MATTHEW.CARLE")] <- "MATT.CARLE"
  name_vect[which(name_vect == "MATTHEW.DUMBA")] <- "MATT.DUMBA"
  name_vect[which(name_vect == "JOSEPH.CORVO")] <- "JOE.CORVO"
  name_vect[which(name_vect == "TOBY.ENSTROM")] <- "TOBIAS.ENSTROM"
  name_vect[which(name_vect == "MICHAEL.SANTORELLI")] <- "MIKE.SANTORELLI"
  name_vect[which(name_vect == "MICHAEL.CAMMALLERI")] <- "MIKE.CAMMALLERI"
  name_vect[which(name_vect == "MICHAEL.FERLAND")] <- "MICHEAL.FERLAND"
  name_vect[which(name_vect == "PIERRE.PARENTEAU" | name_vect == "PIERRE-ALEX.PARENTEAU" | name_vect == "PA.PARENTEAU" | name_vect == "P.A.PARENTEAU" | name_vect == "P-A.PARENTEAU")] <- "P.A..PARENTEAU"
  name_vect <- gsub("ALEXANDER.|ALEXANDRE.", "ALEX.", name_vect)
  name_vect <- gsub("CHRISTOPHER.", "CHRIS.", name_vect)
  name_vect[which(name_vect == "NICOLAS.PETAN")] <- "NIC.PETAN"
  name_vect[which(name_vect == "NIKOLAI.KULEMIN")] <- "NIKOLAY.KULEMIN"
  name_vect[which(name_vect == "MATTHEW.BENNING")] <- "MATT.BENNING"
  name_vect[which(name_vect == "JAMES.HOWARD")] <- "JIMMY.HOWARD"
  name_vect[which(name_vect == "EMMANUEL.FERNANDEZ")] <- "MANNY.FERNANDEZ"
  name_vect[which(name_vect == "EMMANUEL.LEGACE")] <- "MANNY.LEGACE"
  name_vect[which(name_vect == "SIMEON.VARLAMOV")] <- "SEMYON.VARLAMOV"
  name_vect[which(name_vect == "MAXIME.TALBOT")] <- "MAX.TALBOT"
  name_vect[which(name_vect == "MITCHELL.MARNER")] <- "MITCH.MARNER"
  name_vect[which(name_vect == "ANDREW.MILLER")] <- "DREW.MILLER"
  name_vect[which(name_vect == "EDWARD.PURCELL")] <- "TEDDY.PURCELL"
  name_vect[which(name_vect == "NICKLAS.GROSSMAN")] <- "NICKLAS.GROSSMANN"

  return(name_vect)
}

# General Functions -------------------------------------------------------

#' Who
#' @description who() searches a given player ID and returns the player's full name
#'
#' @param player_id player id to get the name of
#'
#' @return the player's name
#' @export
ds.who <- function(player_id) {
  player <- ds.get_player_profile(player_id)

  full_name <- player$people[[1]]$fullName

  return(full_name)
}

#' Scrape Team Profile
#' @description scrape_team_profile() collects and parses the data for a team corresponsing to a given team ID.
#'
#' @param team_id Team ID to get
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#'
#' @return a data frame of team info
#' @export
ds.scrape_team_profile <- function(team_id, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  team_id_ <- nabs(team_id)

  team <- ds.get_team_profile(team_id_, try_tolerance, agents)

  data.frame(
    team_id = na_if_null(nabs(team$teams[[1]]$id)),
    team_name = na_if_null(team$teams[[1]]$name),
    team_alias = na_if_null(team$teams[[1]]$abbreviation),
    team_venue = na_if_null(team$teams[[1]]$venue$name),
    team_location = na_if_null(team$teams[[1]]$locationName),
    team_city = na_if_null(team$teams[[1]]$venue$city),
    team_division_id = na_if_null(nabs(team$teams[[1]]$division$id)),
    team_division_name = na_if_null(team$teams[[1]]$division$name),
    team_conference_id = na_if_null(nabs(team$teams[[1]]$conference$id)),
    team_conference_name = na_if_null(team$teams[[1]]$conference$name),
    franchise_id = na_if_null(nabs(team$teams[[1]]$franchiseId)),
    is_active = na_if_null(as.logical(team$teams[[1]]$active))
  ) ->
  team_df

  return(team_df)
}

#' Scrape Player Profile
#' @description scrape_player_profile() collects and parses the data for a player corresponsing to a given player ID.
#'
#' @param player_id the player ID to get
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#'
#' @return data frame of player info
#' @export
ds.scrape_player_profile <- function(player_id, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  player_id_ <- nabs(player_id)

  player <- ds.get_player_profile(player_id_, try_tolerance, agents)

  data.frame(
    player_id = na_if_null(nabs(player$people[[1]]$id)),
    player_name_first = na_if_null(as.character(player$people[[1]]$firstName)),
    player_name_last = na_if_null(as.character(player$people[[1]]$lastName)),
    player_name_full = na_if_null(as.character(player$people[[1]]$fullName)),
    player_jerseynum = na_if_null(nabs(player$people[[1]]$primaryNumber)),
    player_position = na_if_null(as.character(player$people[[1]]$primaryPosition$code)),
    player_birth_date = na_if_null(as.character(as.Date(player$people[[1]]$birthDate))),
    player_birth_city = na_if_null(as.character(player$people[[1]]$birthCity)),
    player_birth_country = na_if_null(as.character(player$people[[1]]$birthCountry)),
    player_nationality = na_if_null(as.character(player$people[[1]]$nationality)),
    player_height = na_if_null(as.character(player$people[[1]]$height)),
    player_weight = na_if_null(nabs(player$people[[1]]$weight)),
    player_handedness = na_if_null(as.character(player$people[[1]]$shootsCatches)),
    is_active = na_if_null(as.logical(player$people[[1]]$active)),
    is_rookie = na_if_null(as.logical(player$people[[1]]$rookie))
  ) ->
  player_df

  return(player_df)
}

#' Scrape Schedule
#' @description scrape_schedule() collects and parses the schedule data for a range corresponsing to a given start and end date.
#'
#' @param start the start of the date range for the schedule
#' @param end the end of the date range for the schedule
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#'
#' @return data frame schedule
#' @export
ds.scrape_schedule <- function(start, end, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  start_ <- as.character(start)
  end_ <- as.character(end)

  sched <- ds.get_schedule(start_, end_, try_tolerance, agents)

  sched_df <- dcapply(sched$dates,
    ds.parse_date,
    "rbind",
    cores = 1
  )

  return(sched_df)
}

#' Scrape Game
#' @description scrape_game() collects and parses the data for a game corresponsing to a given season and game ID.
#'
#' @param season The season of the game
#' @param game_id the game id to scrape and process
#' @param try_tolerance the number of tries before moving on.
#' @param agents a useragent, selected from the list of useragents.
#' @importFrom dplyr %>%
#'
#' @return list object containing c([[1]] = PBP, [[2]] = Shifts, [[3]] = Highlights) is returned
#' @export
ds.scrape_game <- function(season, game_id, try_tolerance = 3, agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  season_ <- as.character(season)
  game_id_ <- as.character(game_id)

  pbp <- ds.get_pbp(season_, game_id_, try_tolerance, agents)
  home_shifts <- ds.get_shifts(season_, game_id_, venue = "home", source = "htm", try_tolerance, agents)
  away_shifts <- ds.get_shifts(season_, game_id_, venue = "away", source = "htm", try_tolerance, agents)
  roster <- ds.get_roster(season, game_id_, try_tolerance, agents)
  highlights <- ds.get_highlights(season_, game_id_, try_tolerance, agents)

  pbp_full <- pbp[[1]]
  pbp_body <- pbp[[2]]

  home_shifts_outer <- home_shifts[[1]]
  home_shifts_inner <- home_shifts[[2]]
  away_shifts_outer <- away_shifts[[1]]
  away_shifts_inner <- away_shifts[[2]]

  highlight_df <- dcapply(highlights$video$events,
    ds.parse_highlight,
    "rbind",
    cores = 1
  )

  matrix(pbp_body,
    byrow = TRUE,
    ncol = 8
  ) %>%
    data.frame() %>%
    dplyr::filter(X2 != "Per") ->
  pbp_raw

  highlight_df <- dcapply(highlights$video$events,
    ds.parse_highlight,
    "rbind",
    cores = 1
  )

  if (!is.null(pbp_raw) & nrow(pbp_raw) > 0) {
    gsub("^[a-zA-Z]*, ", "", pbp_full[grep("^[a-zA-Z]*, ", pbp_full)]) %>%
      as.Date(format = "%B %d, %Y") %>%
      dplyr::first() %>%
      as.character() ->
    game_date_

    game_id_unique <- paste(substr(season_, 0, 4),
      "0",
      as.character(game_id_),
      sep = ""
    )

    session_ <- ifelse(nabs(game_id_) > 30000,
      "P",
      "R"
    )

    home_team_ <- gsub(" On Ice", "", pbp_body[8])
    away_team_ <- gsub(" On Ice", "", pbp_body[7])

    # Removing this
    # home_team_[which(home_team_ == "PHX")] <- "ARI"; away_team_[which(away_team_ == "PHX")] <- "ARI"

    coordinates_df <- ds.get_coordinates(season_, game_id_, source = "espn", date = game_date_, away_team = away_team_, try_tolerance, agents)

    if (!is.null(coordinates_df)) {
      dupe_check <- coordinates_df %>%
        dplyr::filter(nabs(period) < 5, event_type == "GOAL") %>%
        dplyr::group_by(seconds) %>%
        dplyr::summarise(dupes = n()) %>%
        dplyr::filter(dupes > 1) %>%
        data.frame()
    } else {
      dupe_check <- data.frame()
    }

    if (is.null(coordinates_df) == TRUE | nrow(dupe_check) > 0) {
      coordinates_df <- ds.get_coordinates(season_, game_id_, source = "nhl", date = game_date_, away_team = away_team_, try_tolerance, agents)

      coordinates_df %>%
        dplyr::rename(
          time = period_time_elapsed,
          xcoord = coords_x,
          ycoord = coords_y,
          period = game_period,
          description = event_description
        ) %>%
        dplyr::mutate(
          event_code = NA,
          seconds = 1200 * (nabs(period) - 1) + ds.seconds_from_ms(time),
          event_type = as.character(event_type)
        ) %>%
        dplyr::select(
          event_code,
          xcoord,
          ycoord,
          time,
          period,
          description,
          event_type,
          seconds
        ) %>%
        data.frame() ->
      coordinates_df

      coordinates_df$event_type[which(coordinates_df$event_type == "MISSED_SHOT")] <- "MISS"
      coordinates_df$event_type[which(coordinates_df$event_type == "BLOCKED_SHOT")] <- "BLOCK"
      coordinates_df$event_type[which(coordinates_df$event_type == "FACEOFF")] <- "FAC"
      coordinates_df$event_type[which(coordinates_df$event_type == "GIVEAWAY")] <- "GIVE"
      coordinates_df$event_type[which(coordinates_df$event_type == "TAKEAWAY")] <- "TAKE"
      coordinates_df$event_type[which(coordinates_df$event_type == "PENALTY")] <- "PENL"

      coordinates_df %>%
        dplyr::filter(
          nabs(period) < 5,
          event_type == "GOAL"
        ) %>%
        dplyr::group_by(seconds) %>%
        dplyr::summarise(dupes = n()) %>%
        dplyr::filter(dupes > 1) %>%
        data.frame() ->
      dupe_check

      if (nrow(dupe_check) > 0) {
        coordinates_df <- NULL
      }
    }

    pbp_raw %>%
      dplyr::filter(
        X4 != "",
        X2 != ""
      ) %>%
      dplyr::mutate(
        game_date = game_date_,
        game_id = game_id_unique,
        season = as.character(season_),
        session = session_,
        home_team = home_team_,
        away_team = away_team_,
        time_elapsed = regmatches(X4, regexpr("[0-9]+:[0-9]{2}", X4)),
        game_seconds = 1200 * (nabs(X2) - 1) + ds.seconds_from_ms(time_elapsed),
        event_team = unlist(lapply(regmatches(as.character(X6), gregexpr(paste("(^", paste(ds.team_list, collapse = "|^"), ")", sep = ""), as.character(X6))), na_if_null)),
        event_player_1 = unlist(lapply(regmatches(as.character(X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(X6))), ds.clean_nums))[seq(1, 3 * length(X6), 3)],
        event_player_2 = unlist(lapply(regmatches(as.character(X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(X6))), ds.clean_nums))[seq(2, 3 * length(X6), 3)],
        event_player_3 = unlist(lapply(regmatches(as.character(X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(X6))), ds.clean_nums))[seq(3, 3 * length(X6), 3)],
        event_zone = gsub(". [zZ]one", "", unlist(lapply(regmatches(as.character(X6), gregexpr("[a-zA-Z]{3}. [zZ]one", as.character(X6))), na_if_null))),
        event_detail = gsub(",|, |[A-Z]+ |#[0-9]+ |[A-Z]{2,}.", "", unlist(lapply(regmatches(as.character(X6), gregexpr(", [a-zA-Z|-]+,|[A-Z] .+[(].{4,}[)],|[A-Z] .+[(][a-zA-Z]{3,}[)],", as.character(X6))), na_if_null)))
      ) %>%
      dplyr::rename(
        game_period = X2,
        event_type = X5,
        event_description = X6
      ) %>%
      dplyr::select(
        game_period,
        event_type,
        event_description,
        game_date:event_detail
      ) %>%
      data.frame() -> pbp_df

    dplyr::bind_rows(
      pbp_df %>%
        dplyr::filter(event_type == "FAC") %>%
        dplyr::mutate(
          event_player_1 = paste(away_team, event_player_1, sep = ""),
          event_player_2 = paste(home_team, event_player_2, sep = ""),
          event_player_3 = NA
        ),

      pbp_df %>%
        dplyr::filter(event_type %in% c("HIT", "BLOCK", "PENL")) %>%
        dplyr::group_by(event_team) %>%
        dplyr::mutate(
          event_player_1 = paste(dplyr::first(event_team), event_player_1, sep = ""),
          event_player_2 = paste(unique(c(home_team, away_team))[which(unique(c(home_team, away_team)) != dplyr::first(event_team))], event_player_2, sep = ""),
          event_player_3 = NA
        ),

      pbp_df %>%
        dplyr::filter(event_type %in% c("SHOT", "MISS", "GIVE", "TAKE")) %>%
        dplyr::group_by(event_team) %>%
        dplyr::mutate(
          event_player_1 = paste(dplyr::first(event_team), event_player_1, sep = ""),
          event_player_2 = NA,
          event_player_3 = NA
        ),

      pbp_df %>%
        dplyr::filter(event_type %in% c("GOAL")) %>%
        dplyr::group_by(event_team) %>%
        dplyr::mutate(
          event_player_1 = paste(dplyr::first(event_team), event_player_1, sep = ""),
          event_player_2 = paste(dplyr::first(event_team), event_player_2, sep = ""),
          event_player_3 = paste(dplyr::first(event_team), event_player_3, sep = "")
        ),

      pbp_df %>%
        dplyr::filter(event_type %in% c("FAC", "HIT", "BLOCK", "PENL", "SHOT", "MISS", "GIVE", "TAKE", "GOAL") == FALSE) %>%
        data.frame()
    ) %>%
      dplyr::mutate(
        event_player_1 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, event_player_1),
        event_player_2 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, event_player_2),
        event_player_3 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, event_player_3)
      ) %>%
      data.frame() -> pbp_df
  } else {
    return(list(NULL, NULL, NULL, NULL, NULL))
  }

  if (!is.null(roster)) {
    regmatches(as.character(roster[1]), gregexpr("[0-9]+(\\\r\\\n|\\\n)[A-Z]+(\\\r\\\n|\\\n)[A-Z )(-]+(\\\r\\\n|\\\n)", as.character(roster[1]))) %>%
      unlist() %>%
      strsplit("(\\\r\\\n|\\\n)") %>%
      unlist() %>%
      matrix(
        ncol = 3,
        byrow = TRUE
      ) %>%
      data.frame() %>%
      dplyr::rename(
        player_number = X1,
        player_position = X2,
        player_name = X3
      ) -> pos_match

    pos_match$name_match <- gsub("[^A-Z]|\\([A-Z]+\\)", "", pos_match$player_name)
  }

  if (!is.null(home_shifts) & !is.null(away_shifts) & length(home_shifts_outer[-1]) > 0 & length(away_shifts_outer[-1]) > 0) {
    dplyr::bind_rows(
      data.frame(
        team_name = home_shifts_outer[1],
        team = home_team_,
        venue = "Home",
        num_first_last = home_shifts_outer[-1]
      ),

      data.frame(
        team_name = away_shifts_outer[1],
        team = away_team_,
        venue = "Away",
        num_first_last = away_shifts_outer[-1]
      )
    ) %>%
      data.frame() %>%
      dplyr::filter(grepl("[A-Z0-9]", num_first_last) == TRUE) %>%
      dplyr::mutate(
        game_date = game_date_,
        game_id = game_id_unique,
        season = as.character(season_),
        session = session_,
        player_number = unlist(regmatches(as.character(num_first_last), gregexpr("^[0-9]+", as.character(num_first_last)))),
        team_num = paste(team, player_number, sep = "")
      ) %>%
      data.frame() ->
    roster_df

    strsplit(
      gsub(
        "^[0-9]+ ",
        "",
        roster_df$num_first_last
      ),
      ", "
    ) %>%
      unlist() %>%
      as.character() %>%
      matrix(
        ncol = 2,
        byrow = TRUE
      ) %>%
      data.frame() -> name_mat

    roster_df$first_name <- name_mat[, 2]
    roster_df$last_name <- name_mat[, 1]
    roster_df$player_name <- paste(roster_df$first_name, roster_df$last_name, sep = ".")
    roster_df$name_match <- gsub("[^A-Z]|\\([A-Z]+\\)", "", roster_df$player_name)
    roster_df$player_position <- pos_match$player_position[match(roster_df$name_match, pos_match$name_match)]

    dplyr::bind_rows(
      do.call(
        rbind,
        lapply(as.list(home_shifts_outer[-1]),
          ds.parse_shifts,
          venue = "Home",
          outer = home_shifts_outer,
          inner = home_shifts_inner
        )
      ) %>%
        data.frame() %>%
        dplyr::mutate(team = home_team_),

      do.call(
        rbind,
        lapply(as.list(away_shifts_outer[-1]),
          ds.parse_shifts,
          venue = "Away",
          outer = away_shifts_outer,
          inner = away_shifts_inner
        )
      ) %>%
        data.frame() %>%
        dplyr::mutate(team = away_team_)
    ) %>%
      data.frame() %>%
      dplyr::rename(
        shift_number = X1,
        game_period = X2,
        shift_start = X3,
        shift_end = X4,
        shift_duration = X5
      ) %>%
      dplyr::select(
        shift_number:shift_duration,
        num_first_last,
        team,
        venue
      ) %>%
      dplyr::mutate(
        game_date = game_date_,
        game_id = game_id_unique,
        season = as.character(season_),
        session = session_,
        home_team = home_team_,
        away_team = away_team_
      ) %>%
      data.frame() -> shifts_df

    shifts_df$player_name <- roster_df$player_name[match(shifts_df$num_first_last, roster_df$num_first_last)]
    shifts_df$game_period <- as.character(shifts_df$game_period)
    shifts_df$game_period[which(shifts_df$game_period == "OT")] <- "4"
    shifts_df$team_num <- paste(shifts_df$team, gsub("[^0-9]", "", shifts_df$num_first_last), sep = "")

    do.call(
      rbind,
      strsplit(as.character(shifts_df$shift_start), " / ")
    ) %>%
      data.frame() -> start_mat

    do.call(
      rbind,
      strsplit(as.character(shifts_df$shift_end), " / ")
    ) %>%
      data.frame() -> end_mat

    shifts_df$start_seconds <- 1200 * (nabs(shifts_df$game_period) - 1) + ds.seconds_from_ms(start_mat[, 1])
    shifts_df$end_seconds <- 1200 * (nabs(shifts_df$game_period) - 1) + ds.seconds_from_ms(end_mat[, 1])

    shifts_df$end_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] <- shifts_df$start_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] + ds.seconds_from_ms(shifts_df$shift_duration[which(shifts_df$end_seconds < shifts_df$start_seconds)])

    shifts_df %>%
      dplyr::filter(ds.seconds_from_ms(shift_duration) + (start_seconds - 1200 * (nabs(game_period) - 1)) <= 1200) %>%
      data.frame() -> shifts_df
  } else {
    shifts <- ds.get_shifts(season_, game_id_, venue = NULL, source = "json", try_tolerance, agents)

    shifts_df <- dcapply(shifts$data,
      ds.parse_shift,
      "rbind",
      cores = 1
    )

    if (!is.null(shifts_df)) {
      shifts_df %>%
        dplyr::mutate(
          game_date = game_date_,
          season = as.character(season_),
          session = session_,
          home_team = home_team_,
          away_team = away_team_
        ) %>%
        data.frame() ->
      shifts_df

      year <- substr(season, 0, 4)

      url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
        as.character(year),
        "0",
        as.character(game_id),
        "/feed/live?site=en_nhl",
        sep = ""
      )

      raw_text <- NULL
      json_check <- NULL

      while ({
        class(raw_text) != "character" | class(json_check) != "list"
      } & try_tolerance > 0) {
        try(
          url %>%
            RCurl::getURL(
              header = FALSE,
              .opts = RCurl::curlOptions(
                referer = "nhl.com",
                verbose = FALSE,
                followLocation = TRUE,
                useragent = agents[sample(1:length(agents), 1)]
              )
            )
        ) ->
        raw_text

        json_check <- try(rjson::fromJSON(raw_text), silent = TRUE)

        try_tolerance <- try_tolerance - 1
      }

      raw_json <- try(rjson::fromJSON(raw_text), silent = TRUE)

      if (class(raw_json) == "try-error") {
        raw_json <- NULL
      }

      home_roster <- raw_json$liveData$boxscore$teams$home
      away_roster <- raw_json$liveData$boxscore$teams$away

      home_player_data <- dcapply(home_roster$players,
        ds.parse_player,
        "rbind",
        cores = 1
      )

      away_player_data <- dcapply(away_roster$players,
        ds.parse_player,
        "rbind",
        cores = 1
      )

      dplyr::bind_rows(
        home_player_data %>%
          dplyr::mutate(
            team = home_roster$team$abbreviation,
            team_name = toupper(home_roster$team$name),
            venue = "Home"
          ),

        away_player_data %>%
          dplyr::mutate(
            team = away_roster$team$abbreviation,
            team_name = toupper(away_roster$team$name),
            venue = "Away"
          )
      ) %>%
        data.frame() ->
      player_data

      player_data$team_num <- paste(player_data$team, player_data$player_number, sep = "")

      name_match <- dcapply(player_data$player_id,
        ds.scrape_player_profile,
        "rbind",
        cores = 1
      )

      player_data %>%
        dplyr::mutate(
          first_name = toupper(name_match$player_name_first[match(player_id, name_match$player_id)]),
          last_name = toupper(name_match$player_name_last[match(player_id, name_match$player_id)]),
          num_first_last = NA,
          game_date = game_date_,
          game_id = game_id_unique,
          season = as.character(season_),
          session = session_,
          home_team = home_team_,
          away_team = away_team_,
          player_name = paste(first_name, last_name, sep = "."),
          name_match = gsub("[^A-Z]|\\([A-Z]+\\)", "", player_name),
          player_position = substr(position, 0, 1)
        ) %>%
        dplyr::select(
          team_name,
          team,
          venue,
          num_first_last,
          game_date,
          game_id,
          season,
          session,
          player_number,
          team_num,
          first_name,
          last_name,
          player_name,
          name_match,
          player_position
        ) %>%
        data.frame() ->
      roster_df

      shifts_df %>%
        dplyr::rename(game_period = shift_period) %>%
        dplyr::mutate(
          num_first_last = NA,
          venue = ifelse(team == home_team_,
            "Home",
            "Away"
          ),
          game_date = game_date_,
          game_id = game_id_unique,
          season = as.character(season_),
          session = session_,
          home_team = home_team_,
          away_team = away_team_,
          player_name = player_data$player_name[match(player_id, player_data$player_id)],
          team_num = player_data$team_num[match(player_id, player_data$player_id)],
          start_seconds = 1200 * (nabs(game_period) - 1) + ds.seconds_from_ms(shift_start),
          end_seconds = 1200 * (nabs(game_period) - 1) + ds.seconds_from_ms(shift_end)
        ) %>%
        dplyr::select(
          shift_number,
          game_period,
          shift_start,
          shift_end,
          shift_duration,
          num_first_last,
          team,
          venue,
          game_date,
          game_id,
          season,
          session,
          home_team,
          away_team,
          player_name,
          team_num,
          start_seconds,
          end_seconds
        ) %>%
        data.frame() ->
      shifts_df

      shifts_df$end_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] <- shifts_df$start_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] + ds.seconds_from_ms(shifts_df$shift_duration[which(shifts_df$end_seconds < shifts_df$start_seconds)])

      shifts_df %>%
        dplyr::filter(ds.seconds_from_ms(shift_duration) + (start_seconds - 1200 * (nabs(game_period) - 1)) <= 1200) %>%
        data.frame() ->
      shifts_df
    } else {
      shifts_df <- ds.get_shifts(season_, game_id_, venue = NULL, source = "pbp", try_tolerance, agents)
      roster_df <- ds.parse_roster(ds.get_roster(season_, game_id_, try_tolerance, agents))
      roster_df %>%
        tidyr::unite(player_name, first_name, last_name, sep = ".", remove = FALSE) %>%
        tidyr::unite(name_match, first_name, last_name, sep = "", remove = FALSE) %>%
        dplyr::mutate(
          team_name = NA,
          team = ifelse(venue == "Home", yes = home_team_, no = away_team_),
          game_date = game_date_,
          game_id = game_id_unique,
          season = as.character(season_),
          session = session_
        ) %>%
        tidyr::unite(team_num, team, player_num, sep = "", remove = FALSE) %>%
        dplyr::select(
          team_name,
          team,
          venue,
          num_first_last,
          game_date,
          game_id,
          season,
          session,
          player_number,
          team_num,
          first_name,
          last_name,
          player_name,
          name_match,
          player_position
        ) %>%
        data.frame() ->
      roster_df

      if (is.null(shifts_df)) {
        return(list(NULL, NULL, NULL, NULL, NULL))
      }
    }
  }

  if (!is.null(highlight_df)) {
    highlight_df %>%
      dplyr::filter(
        nabs(event_period) < 5,
        event_type == 505
      ) %>%
      dplyr::group_by(event_id) %>%
      dplyr::summarise(dupes = n()) %>%
      dplyr::filter(dupes > 1) %>%
      data.frame() ->
    dupe_check

    if (nrow(dupe_check) > 0) {
      highlight_df <- NULL
    } else {
      highlight_df %>%
        dplyr::mutate(
          game_date = game_date_,
          game_id = game_id_unique,
          season = as.character(season_),
          session = session_,
          home_team = home_team_,
          away_team = away_team_
        ) %>%
        data.frame() ->
      highlight_df
    }
  }

  if (!is.null(coordinates_df)) {
    coordinates_df %>%
      dplyr::mutate(
        game_date = game_date_,
        game_id = game_id_unique,
        season = as.character(season_),
        session = session_,
        home_team = home_team_,
        away_team = away_team_
      ) %>%
      data.frame() ->
    coordinates_df
  }

  game_list <- list(
    pbp_df,
    roster_df,
    shifts_df,
    highlight_df,
    coordinates_df
  )

  return(game_list)
}

#' Compile Games
#' @description compile_games() collects, parses and compiles all game data corresponding to a given vector of game IDs and season.
#'
#' @param games the specific games to compile
#' @param season the season to compile (games are in this chosen season)
#' @param pause delay between compilations
#' @param try_tolerance the number of tries before skipping the game
#' @param agents user agents, selected from a set.
#' @importFrom foreach %do%
#' @importFrom dplyr %>%
#'
#' @return list object containing c([[1]] = PBP, [[2]] = Roster, [[3]] = Shifts) is returned
#' @export
ds.compile_games <- function(games, season, pause = 1, try_tolerance = 3,
                             agents = hockeyR::ds.get_user_agents()[sample(1:4, size = 1)]) {
  chgames <- as.character(games)
  nested_games <- foreach::foreach(g = chgames) %do% {
    cat("\r", g, "...", sep = "")
    inner <- ds.scrape_game(season, g, try_tolerance, agents)
    Sys.sleep(pause)
    cat("\r")
    return(inner)
  }

  unpacked <- do.call(Map, c(rbind, nested_games))

  pbp <- unpacked[[1]]
  roster <- unpacked[[2]]
  shifts <- unpacked[[3]]
  highlights <- unpacked[[4]]
  coords <- unpacked[[5]]

  roster$player_name <- ds.fix_names(roster$player_name)
  shifts$player_name <- ds.fix_names(shifts$player_name)

  dplyr::bind_rows(
    shifts %>%
      dplyr::filter(!is.na(shift_duration)) %>%
      dplyr::group_by(
        game_id,
        game_date,
        season,
        session,
        home_team,
        away_team,
        team,
        game_period,
        start_seconds
      ) %>%
      dplyr::rename(game_seconds = start_seconds, event_team = team) %>%
      dplyr::summarise(
        event_type = "ON",
        players_substituted = paste(unique(team_num), collapse = ", ")
      ) %>%
      data.frame(),

    shifts %>%
      dplyr::filter(!is.na(shift_duration)) %>%
      dplyr::group_by(
        game_id,
        game_date,
        season,
        session,
        home_team,
        away_team,
        team,
        game_period,
        end_seconds
      ) %>%
      dplyr::rename(game_seconds = end_seconds, event_team = team) %>%
      dplyr::summarise(
        event_type = "OFF",
        players_substituted = paste(unique(team_num), collapse = ", ")
      ) %>%
      data.frame()
  ) -> shift_summary

  if (!is.null(highlights)) {
    highlights$event_match <- ifelse(highlights$event_type == 505,
      "GOAL",
      "SHOT"
    )

    dplyr::left_join(pbp,
      highlights %>%
        dplyr::mutate(game_seconds = 1200 * (nabs(event_period) - 1) + nabs(event_seconds)) %>%
        dplyr::rename(highlight_code = highlight_id) %>%
        dplyr::select(game_id, game_seconds, event_match, highlight_code) %>%
        data.frame(),
      by = c("game_id" = "game_id", "game_seconds" = "game_seconds", "event_type" = "event_match")
    ) %>%
      data.frame() ->
    new_pbp
  } else {
    pbp %>%
      dplyr::mutate(highlight_code = NA) %>%
      data.frame() ->
    new_pbp
  }

  if (!is.null(coords)) {
    dplyr::left_join(new_pbp,
      coords %>%
        dplyr::rename(
          coords_x = xcoord,
          coords_y = ycoord
        ) %>%
        dplyr::select(game_id, seconds, event_type, coords_x, coords_y) %>%
        data.frame(),
      by = c("game_id" = "game_id", "game_seconds" = "seconds", "event_type" = "event_type")
    ) %>%
      data.frame() ->
    new_pbp
  } else {
    new_pbp %>%
      dplyr::mutate(
        coords_x = NA,
        coords_y = NA
      ) %>%
      data.frame() ->
    new_pbp
  }

  new_pbp %>%
    dplyr::group_by(game_id, game_seconds, event_description) %>%
    dplyr::slice(1) %>%
    data.frame() ->
  new_pbp

  new_pbp$game_period <- nabs(new_pbp$game_period)
  shift_summary$game_period <- nabs(shift_summary$game_period)

  dplyr::bind_rows(
    new_pbp,
    shift_summary
  ) %>%
    dplyr::mutate(priority = 1 * (event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK")) +
      2 * (event_type == "GOAL") +
      3 * (event_type == "STOP") +
      4 * (event_type == "PENL") +
      5 * (event_type == "OFF") +
      6 * (event_type == "ON") +
      7 * (event_type == "FAC")) %>%
    dplyr::group_by(game_id) %>%
    dplyr::arrange(
      game_period,
      game_seconds,
      priority
    ) %>%
    dplyr::mutate(event_index = cumsum(!is.na(game_id))) %>%
    data.frame() ->
  new_pbp

  home_on_mat <- dcapply(as.list(unique(shifts$team_num)),
    ds.is_on,
    "cbind",
    cores = 1,
    pbp = dplyr::arrange(
      new_pbp,
      game_id,
      event_index
    ),
    venue = "Home"
  )

  away_on_mat <- dcapply(as.list(unique(shifts$team_num)),
    ds.is_on,
    "cbind",
    cores = 1,
    pbp = dplyr::arrange(
      new_pbp,
      game_id,
      event_index
    ),
    venue = "Away"
  )

  which(home_on_mat == 1,
    arr.ind = TRUE
  ) %>%
    data.frame() %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(
      home_on_1 = colnames(home_on_mat)[unique(col)[1]],
      home_on_2 = colnames(home_on_mat)[unique(col)[2]],
      home_on_3 = colnames(home_on_mat)[unique(col)[3]],
      home_on_4 = colnames(home_on_mat)[unique(col)[4]],
      home_on_5 = colnames(home_on_mat)[unique(col)[5]],
      home_on_6 = colnames(home_on_mat)[unique(col)[6]]
    ) %>%
    data.frame() ->
  home_on_df

  which(away_on_mat == 1,
    arr.ind = TRUE
  ) %>%
    data.frame() %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(
      away_on_1 = colnames(away_on_mat)[unique(col)[1]],
      away_on_2 = colnames(away_on_mat)[unique(col)[2]],
      away_on_3 = colnames(away_on_mat)[unique(col)[3]],
      away_on_4 = colnames(away_on_mat)[unique(col)[4]],
      away_on_5 = colnames(away_on_mat)[unique(col)[5]],
      away_on_6 = colnames(away_on_mat)[unique(col)[6]]
    ) %>%
    data.frame() ->
  away_on_df

  do.call(
    c,
    home_on_df[, -1] %>%
      split(1:nrow(home_on_df)) %>%
      lapply(
        ds.find_goalie,
        roster
      )
  ) %>%
    as.character() ->
  home_goalie

  do.call(
    c,
    away_on_df[, -1] %>%
      split(1:nrow(away_on_df)) %>%
      lapply(
        ds.find_goalie,
        roster
      )
  ) %>%
    as.character() ->
  away_goalie

  new_pbp %>%
    dplyr::arrange(
      game_id,
      event_index
    ) %>%
    dplyr::mutate(
      home_on_1 = NA,
      home_on_2 = NA,
      home_on_3 = NA,
      home_on_4 = NA,
      home_on_5 = NA,
      home_on_6 = NA,
      home_goalie = NA,
      away_on_1 = NA,
      away_on_2 = NA,
      away_on_3 = NA,
      away_on_4 = NA,
      away_on_5 = NA,
      away_on_6 = NA,
      away_goalie = NA
    ) ->
  full_pbp

  full_pbp$home_on_1[home_on_df$row] <- home_on_df$home_on_1
  full_pbp$home_on_2[home_on_df$row] <- home_on_df$home_on_2
  full_pbp$home_on_3[home_on_df$row] <- home_on_df$home_on_3
  full_pbp$home_on_4[home_on_df$row] <- home_on_df$home_on_4
  full_pbp$home_on_5[home_on_df$row] <- home_on_df$home_on_5
  full_pbp$home_on_6[home_on_df$row] <- home_on_df$home_on_6
  full_pbp$home_goalie[home_on_df$row] <- home_goalie

  full_pbp$away_on_1[away_on_df$row] <- away_on_df$away_on_1
  full_pbp$away_on_2[away_on_df$row] <- away_on_df$away_on_2
  full_pbp$away_on_3[away_on_df$row] <- away_on_df$away_on_3
  full_pbp$away_on_4[away_on_df$row] <- away_on_df$away_on_4
  full_pbp$away_on_5[away_on_df$row] <- away_on_df$away_on_5
  full_pbp$away_on_6[away_on_df$row] <- away_on_df$away_on_6
  full_pbp$away_goalie[away_on_df$row] <- away_goalie

  full_pbp$event_player_1 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$event_player_1, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$event_player_2 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$event_player_2, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$event_player_3 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$event_player_3, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$home_on_1 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$home_on_1, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$home_on_2 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$home_on_2, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$home_on_3 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$home_on_3, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$home_on_4 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$home_on_4, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$home_on_5 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$home_on_5, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$home_on_6 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$home_on_6, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$away_on_1 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$away_on_1, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$away_on_2 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$away_on_2, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$away_on_3 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$away_on_3, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$away_on_4 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$away_on_4, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$away_on_5 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$away_on_5, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$away_on_6 <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$away_on_6, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$home_goalie <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$home_goalie, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]
  full_pbp$away_goalie <- roster$player_name[match(
    paste(full_pbp$game_id, full_pbp$away_goalie, sep = "."),
    paste(roster$game_id, roster$team_num, sep = ".")
  )    ]

  full_pbp %>%
    dplyr::group_by(game_id) %>%
    dplyr::arrange(event_index) %>%
    dplyr::mutate(
      home_skaters = 6 - 1 * (is.na(home_on_1) == TRUE) -
        1 * (is.na(home_on_2) == TRUE) -
        1 * (is.na(home_on_3) == TRUE) -
        1 * (is.na(home_on_4) == TRUE) -
        1 * (is.na(home_on_5) == TRUE) -
        1 * (is.na(home_on_6) == TRUE) -
        1 * (!is.na(home_goalie)),
      away_skaters = 6 - 1 * (is.na(away_on_1) == TRUE) -
        1 * (is.na(away_on_2) == TRUE) -
        1 * (is.na(away_on_3) == TRUE) -
        1 * (is.na(away_on_4) == TRUE) -
        1 * (is.na(away_on_5) == TRUE) -
        1 * (is.na(away_on_6) == TRUE) -
        1 * (!is.na(away_goalie)),
      home_score = cumsum(event_type == "GOAL" & event_team == home_team) - 1 * (event_type == "GOAL" & event_team == home_team),
      away_score = cumsum(event_type == "GOAL" & event_team == away_team) - 1 * (event_type == "GOAL" & event_team == away_team),
      event_length = nabs(dplyr::lead(game_seconds, 1) - game_seconds)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      game_strength_state = paste(ifelse(is.na(home_goalie) == TRUE,
        "E",
        home_skaters
      ),
      ifelse(is.na(away_goalie) == TRUE,
        "E",
        away_skaters
      ),
      sep = "v"
      ),
      game_score_state = paste(home_score,
        away_score,
        sep = "v"
      )
    ) %>%
    dplyr::select(dplyr::one_of(ds.pbp_colnames)) %>%
    dplyr::arrange(
      game_id,
      event_index
    ) %>%
    data.frame() ->
  full_pbp

  full_pbp$event_team[which(full_pbp$event_team == "PHX")] <- "ARI"

  new_game_list <- list(
    full_pbp,
    roster,
    shifts
  )

  return(new_game_list)
}

#' Get Game Score Weights
#' @description Get the Game Score contribution weights
#' @note Game score vectors must be sorted in the same order as the weights. That is: `paste(names(get.st.game_score_weight()), collapse = ', ')`
#'
#' @return game score contribution weight.
#' @export
get.st.game_score_weight <- function() {
  return(c("G" = 0.75, "A1" = 0.7, "A2" = 0.55, "iSF" = 0.075, "iBLK" = 0.05, "iPENT" = -0.15, "iPEND" = 0.15, "iFOW" = 0.01, "iFOL" = -0.01, "CF" = 0.05, "CA" = -0.05, "GF" = 0.15, "GA" = -0.15))
}
st.game_score_weight <- get.st.game_score_weight()

c("SHOT", "GOAL") -> st.shot_events

c("SHOT", "GOAL", "MISS") -> st.fenwick_events

c("SHOT", "GOAL", "MISS", "BLOCK") -> st.corsi_events

as.factor(c("3v3", "5v5", "4v4", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3")) -> st.strength_states

# Meta Functions ----------------------------------------------------------

#' Combo Code
#' @description combo_code() returns the unique code produced from a list of up to three players.
#' @param p1 player 1 code
#' @param p2 player 2 code
#' @param p3 player 3 code
#'
#' @return a character string of the three player codes sorted with '-' separator
#' @export
st.combo_code <- function(p1, p2, p3) {
  sorted <- sort(c(p1, p2, p3))

  p1_abs <- sorted[1]
  p2_abs <- sorted[2]
  p3_abs <- sorted[3]

  code <- paste(p1_abs,
    p2_abs,
    p3_abs,
    sep = "-"
  )

  return(code)
}

#' Game Score
#' @description game_score() returns the game score obtained from a given vector of statistics. The vector x is expected to contain the necessary stats in proper order
#' @seealso \code{\link{get.st.game_score_weight}()}
#'
#' @param x the game score vector
#'
#' @return game score for that vector
#' @export
st.game_score <- function(x) {
  return(sum(st.game_score_weights * x))
}

#' Distance from Net
#' @description distance_from_net() returns the distance from the nearest net in feet of a location corresponding to a given set of coordinates
#'
#' @param x the x coordinate of the event
#' @param y the y coordinate of the event
#'
#' @return the distance from the nearest net in feet
#' @export
st.distance_from_net <- function(x, y) {
  return(sqrt((89 - abs(nabs(x)))^2 + nabs(y)^2))
}

#' Angle from Centre
#' @description   # angle_from_centre() returns the angle from the central line perpendicular to the goal line in degrees of a location corresponsing to a given set of coordinates
#'
#' @param x the x coordinate of the event
#' @param y the y coordinate of the event
#'
#' @return the angle from centre in degrees
#' @export
st.angle_from_centre <- function(x, y) {
  return(abs(atan(nabs(y) / (89 - abs(nabs(x)))) * (180 / pi)))
}

#' Which Zone
#' @description which_zone() returns the absolute zone of a location corresponding to a given x-coordinate
#'
#' @param x the x coordinate of the event
#'
#' @return The zone as L, N, or R as a character
#' @export
st.which_zone <- function(x) {
  factor_level <- as.factor(1 * (x <= -25) +
    2 * (abs(nabs(x)) < 25) +
    3 * (x >= 25))

  levels(factor_level) <- c(
    "L",
    "N",
    "R"
  )

  return(as.character(factor_level))
}

#' Which Circle
#' @description which_circle() returns the faceoff circle number nearest to a location corresponding to a given set of coordinates
#'
#' @param x The x coordinate of the event
#' @param y The y coordinate of the event
#'
#' @return the faceoff circle closest to the event
#' @export
st.which_circle <- function(x, y) {
  circle <- 1 * (nabs(x) <= -25 & nabs(y) > 0) +
    2 * (nabs(x) <= -25 & nabs(y) < 0) +
    3 * (nabs(x) < 0 & nabs(x) > 25 & nabs(y) > 0) +
    4 * (nabs(x) < 0 & nabs(x) > 25 & nabs(y) < 0) +
    5 * (abs(nabs(x)) < 5 & abs(nabs(y)) < 5) +
    6 * (nabs(x) > 0 & nabs(x) < 25 & nabs(y) > 0) +
    7 * (nabs(x) > 0 & nabs(x) < 25 & nabs(y) < 0) +
    8 * (nabs(x) >= 25 & nabs(y) > 0) +
    9 * (nabs(x) >= 25 & nabs(y) < 0)

  return(circle)
}

#' Enhance PBP
#' @description pbp_enhance() performs some preliminary operations on a given PBP data frame object and returns the enhanced version
#'
#' @param pbp a Corsica PBP data frame object
#' @importFrom dplyr %>%
#'
#' @return an enahnced Corsica PBP data frame object (ePBP)
#' @export
st.pbp_enhance <- function(pbp) {

  ## Description
  #

  pbp %>%
    dplyr::mutate_at(dplyr::funs(nabs), coords_x, coords_y, game_period, game_seconds) %>% ## used to be mutate_each
    data.frame() ->
  pbp

  pbp %>%
    dplyr::mutate(
      event_distance = st.distance_from_net(coords_x, coords_y),
      event_angle = st.angle_from_centre(coords_x, coords_y),
      event_rinkside = st.which_zone(coords_x),
      event_circle = st.which_circle(coords_x, coords_y)
    ) %>%
    data.frame() ->
  enhanced_pbp

  return(enhanced_pbp)
}

#' Summarize Team Stats
#' @description sum_team() summarizes all team counting stats from a ePBP data frame object. x is expected to be a grouped data frame with home_team or away_team as a grouping variable for venue = "home" and venue = "away" respectively
#'
#' @param x a Corsica PBP data frame object
#' @param venue home or away
#' @importFrom dplyr %>%
#'
#' @return player counting stats for the home or away team in the supplied pbp frame
#' @export
st.sum_team <- function(x, venue) {
  venue_ <- tolower(as.character(venue))

  if (venue_ == "home") {
    x %>%
      dplyr::rename(team = home_team) %>%
      dplyr::summarise(
        venue = "Home",
        GP = length(unique(game_id)),
        TOI = sum(event_length) / 60,
        CF = sum({
          event_type %in% st.fenwick_events & event_team == team
        } | {
          event_type == "BLOCKED_SHOT" & event_team == away_team
        }),
        CA = sum({
          event_type %in% st.fenwick_events & event_team == away_team
        } | {
          event_type == "BLOCKED_SHOT" & event_team == team
        }),
        FF = sum(event_type %in% st.fenwick_events & event_team == team),
        FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
        SF = sum(event_type %in% st.shot_events & event_team == team),
        SA = sum(event_type %in% st.shot_events & event_team == away_team),
        GF = sum(event_type == "GOAL" & event_team == team),
        GA = sum(event_type == "GOAL" & event_team == away_team),
        # xGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == team))),
        # xGA = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        # ACF = sum(stats::na.omit(adj_home_corsi * (event_type %in% st.corsi_events & event_team == team))),
        # ACA = sum(stats::na.omit(adj_away_corsi * (event_type %in% st.corsi_events & event_team == away_team))),
        # AFF = sum(stats::na.omit(adj_home_fenwick * (event_type %in% st.fenwick_events & event_team == team))),
        # AFA = sum(stats::na.omit(adj_away_fenwick * (event_type %in% st.fenwick_events & event_team == away_team))),
        # ASF = sum(stats::na.omit(adj_home_shot * (event_type %in% st.shot_events & event_team == team))),
        # ASA = sum(stats::na.omit(adj_away_shot * (event_type %in% st.shot_events & event_team == away_team))),
        # AGF = sum(stats::na.omit(adj_home_goal * (event_type == "GOAL" & event_team == team))),
        # AGA = sum(stats::na.omit(adj_away_goal * (event_type == "GOAL" & event_team == away_team))),
        # AxGF = sum(stats::na.omit(adj_home_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == team))),
        # AxGA = sum(stats::na.omit(adj_away_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
        DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
        NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
        FOW = sum(event_type == "FACEOFF" & event_team == team),
        FOL = sum(event_type == "FACEOFF" & event_team == away_team),
        PENT2 = sum(1 * (event_type == "PENALTY" & event_team == team) +
          1 * (event_type == "PENALTY" & event_team == team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PENT5 = sum(event_type == "PENALTY" & event_team == team & grepl("fighting|major", tolower(event_detail)) == TRUE),
        PENTS = sum(event_type == "PENALTY" & event_team == team & grepl("ps \\-", tolower(event_detail)) == TRUE),
        PEND2 = sum(1 * (event_type == "PENALTY" & event_team == away_team) +
          1 * (event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PEND5 = sum(event_type == "PENALTY" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
        PENDS = sum(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-", tolower(event_detail)) == TRUE),

        GVA = sum(event_type == "GIVEAWAY" & event_team == team),
        TKA = sum(event_type == "TAKEAWAY" & event_team == team),
        HF = sum(event_type == "HIT" & event_team == team),
        HA = sum(event_type == "HIT" & event_team == away_team)
      ) %>%
      data.frame() %>%
      return()
  } else if (venue_ == "away") {
    x %>%
      dplyr::rename(team = away_team) %>%
      dplyr::summarise(
        venue = "Away",
        GP = length(unique(game_id)),
        TOI = sum(event_length) / 60,
        CF = sum({
          event_type %in% st.fenwick_events & event_team == team
        } | {
          event_type == "BLOCKED_SHOT" & event_team == home_team
        }),
        CA = sum({
          event_type %in% st.fenwick_events & event_team == home_team
        } | {
          event_type == "BLOCKED_SHOT" & event_team == team
        }),
        FF = sum(event_type %in% st.fenwick_events & event_team == team),
        FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
        SF = sum(event_type %in% st.shot_events & event_team == team),
        SA = sum(event_type %in% st.shot_events & event_team == home_team),
        GF = sum(event_type == "GOAL" & event_team == team),
        GA = sum(event_type == "GOAL" & event_team == home_team),
        # xGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == team))),
        # xGA = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        # ACF = sum(stats::na.omit(adj_away_corsi * (event_type %in% st.corsi_events & event_team == team))),
        # ACA = sum(stats::na.omit(adj_home_corsi * (event_type %in% st.corsi_events & event_team == home_team))),
        # AFF = sum(stats::na.omit(adj_away_fenwick * (event_type %in% st.fenwick_events & event_team == team))),
        # AFA = sum(stats::na.omit(adj_home_fenwick * (event_type %in% st.fenwick_events & event_team == home_team))),
        # ASF = sum(stats::na.omit(adj_away_shot * (event_type %in% st.shot_events & event_team == team))),
        # ASA = sum(stats::na.omit(adj_home_shot * (event_type %in% st.shot_events & event_team == home_team))),
        # AGF = sum(stats::na.omit(adj_away_goal * (event_type == "GOAL" & event_team == team))),
        # AGA = sum(stats::na.omit(adj_home_goal * (event_type == "GOAL" & event_team == home_team))),
        # AxGF = sum(stats::na.omit(adj_away_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == team))),
        # AxGA = sum(stats::na.omit(adj_home_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
        DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
        NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
        FOW = sum(event_type == "FACEOFF" & event_team == team),
        FOL = sum(event_type == "FACEOFF" & event_team == home_team),
        PENT2 = sum(1 * (event_type == "PENALTY" & event_team == team) +
          1 * (event_type == "PENALTY" & event_team == team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PENT5 = sum(event_type == "PENALTY" & event_team == team & grepl("fighting|major", tolower(event_detail)) == TRUE),
        PENTS = sum(event_type == "PENALTY" & event_team == team & grepl("ps \\-", tolower(event_detail)) == TRUE),
        PEND2 = sum(1 * (event_type == "PENALTY" & event_team == home_team) +
          1 * (event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PEND5 = sum(event_type == "PENALTY" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
        PENDS = sum(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-", tolower(event_detail)) == TRUE),
        GVA = sum(event_type == "GIVEAWAY" & event_team == team),
        TKA = sum(event_type == "TAKEAWAY" & event_team == team),
        HF = sum(event_type == "HIT" & event_team == team),
        HA = sum(event_type == "HIT" & event_team == home_team)
      ) %>%
      data.frame() %>%
      return()
  }
}

#' Summarize Skater Stats
#' @description sum_skater() summarizes all skater counting stats from a PBP data frame object. x is expected to be a grouped data frame with home_on_x or away_on_x as a grouping variable for venue = "home" and venue = "away" respectively.
#' @note A dplyr::rename() argument must be passed before sum_skater() to convert home/away_on_x to player
#'
#' @param x a Corsica PBP data frame object
#' @param venue home or away
#' @importFrom dplyr %>%
#'
#' @return player counting stats for the home or away players in the supplied pbp frame
#' @export
st.sum_skater <- function(x, venue) {
  venue_ <- tolower(as.character(venue))

  if (venue_ == "home") {
    x %>%
      dplyr::summarise(
        venue = "Home",
        team = dplyr::first(home_team),
        GP = length(unique(game_id)),
        TOI = sum(event_length) / 60,
        CF = sum({
          event_type %in% st.fenwick_events & event_team == home_team
        } | {
          event_type == "BLOCKED_SHOT" & event_team == away_team
        }),
        CA = sum({
          event_type %in% st.fenwick_events & event_team == away_team
        } | {
          event_type == "BLOCKED_SHOT" & event_team == home_team
        }),
        FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
        FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
        SF = sum(event_type %in% st.shot_events & event_team == home_team),
        SA = sum(event_type %in% st.shot_events & event_team == away_team),
        GF = sum(event_type == "GOAL" & event_team == home_team),
        GA = sum(event_type == "GOAL" & event_team == away_team),
        # xGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        # xGA = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        # ACF = sum(stats::na.omit(adj_home_corsi * (event_type %in% st.corsi_events & event_team == home_team))),
        # ACA = sum(stats::na.omit(adj_away_corsi * (event_type %in% st.corsi_events & event_team == away_team))),
        # AFF = sum(stats::na.omit(adj_home_fenwick * (event_type %in% st.fenwick_events & event_team == home_team))),
        # AFA = sum(stats::na.omit(adj_away_fenwick * (event_type %in% st.fenwick_events & event_team == away_team))),
        # ASF = sum(stats::na.omit(adj_home_shot * (event_type %in% st.shot_events & event_team == home_team))),
        # ASA = sum(stats::na.omit(adj_away_shot * (event_type %in% st.shot_events & event_team == away_team))),
        # AGF = sum(stats::na.omit(adj_home_goal * (event_type == "GOAL" & event_team == home_team))),
        # AGA = sum(stats::na.omit(adj_away_goal * (event_type == "GOAL" & event_team == away_team))),
        # AxGF = sum(stats::na.omit(adj_home_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        # AxGA = sum(stats::na.omit(adj_away_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
        DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
        NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
        PENT2 = sum(1 * (event_type == "PENALTY" & event_team == home_team) +
          1 * (event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PENT5 = sum(event_type == "PENALTY" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
        PEND2 = sum(1 * (event_type == "PENALTY" & event_team == away_team) +
          1 * (event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PEND5 = sum(event_type == "PENALTY" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),

        iCF = sum({
          event_type %in% st.fenwick_events & event_player_1 == player
        } | {
          event_type == "BLOCKED_SHOT" & event_player_2 == player
        }),
        iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player),
        iSF = sum(event_type %in% st.shot_events & event_player_1 == player),
        ixGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_player_1 == player))),
        G = sum(event_type == "GOAL" & event_player_1 == player),
        A1 = sum(stats::na.omit(event_type == "GOAL" & event_player_2 == player)),
        A2 = sum(stats::na.omit(event_type == "GOAL" & event_player_3 == player)),
        iGVA = sum(event_type == "GIVEAWAY" & event_player_1 == player),
        iTKA = sum(event_type == "TAKEAWAY" & event_player_1 == player),
        iHF = sum(event_type == "HIT" & event_player_1 == player),
        iHA = sum(event_type == "HIT" & event_player_2 == player),
        iBLK = sum(event_type == "BLOCKED_SHOT" & event_player_1 == player),
        iFOW = sum(event_type == "FACEOFF" & event_player_1 == player),
        iFOL = sum(event_type == "FACEOFF" & event_player_2 == player),
        iPENT2 = sum(stats::na.omit(1 * (event_type == "PENALTY" & event_player_1 == player) +
          1 * (event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_player_1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE))),
        iPENT5 = sum(stats::na.omit(event_type == "PENALTY" & event_player_1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
        iPEND2 = sum(stats::na.omit(1 * (event_type == "PENALTY" & event_player_2 == player) +
          1 * (event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_player_2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE))),
        iPEND5 = sum(stats::na.omit(event_type == "PENALTY" & event_player_2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
      ) %>%
      data.frame() %>%
      return()
  } else if (venue_ == "away") {
    x %>%
      dplyr::summarise(
        venue = "Away",
        team = dplyr::first(away_team),
        GP = length(unique(game_id)),
        TOI = sum(event_length) / 60,
        CF = sum({
          event_type %in% st.fenwick_events & event_team == away_team
        } | {
          event_type == "BLOCKED_SHOT" & event_team == home_team
        }),
        CA = sum({
          event_type %in% st.fenwick_events & event_team == home_team
        } | {
          event_type == "BLOCKED_SHOT" & event_team == away_team
        }),
        FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
        FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
        SF = sum(event_type %in% st.shot_events & event_team == away_team),
        SA = sum(event_type %in% st.shot_events & event_team == home_team),
        GF = sum(event_type == "GOAL" & event_team == away_team),
        GA = sum(event_type == "GOAL" & event_team == home_team),
        # xGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        # xGA = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        # ACF = sum(stats::na.omit(adj_away_corsi * (event_type %in% st.corsi_events & event_team == away_team))),
        # ACA = sum(stats::na.omit(adj_home_corsi * (event_type %in% st.corsi_events & event_team == home_team))),
        # AFF = sum(stats::na.omit(adj_away_fenwick * (event_type %in% st.fenwick_events & event_team == away_team))),
        # AFA = sum(stats::na.omit(adj_home_fenwick * (event_type %in% st.fenwick_events & event_team == home_team))),
        # ASF = sum(stats::na.omit(adj_away_shot * (event_type %in% st.shot_events & event_team == away_team))),
        # ASA = sum(stats::na.omit(adj_home_shot * (event_type %in% st.shot_events & event_team == home_team))),
        # AGF = sum(stats::na.omit(adj_away_goal * (event_type == "GOAL" & event_team == away_team))),
        # AGA = sum(stats::na.omit(adj_home_goal * (event_type == "GOAL" & event_team == home_team))),
        # AxGF = sum(stats::na.omit(adj_away_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        # AxGA = sum(stats::na.omit(adj_home_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != away_rinkside),
        DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == away_rinkside),
        NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
        PENT2 = sum(1 * (event_type == "PENALTY" & event_team == away_team) +
          1 * (event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PENT5 = sum(event_type == "PENALTY" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
        PEND2 = sum(1 * (event_type == "PENALTY" & event_team == home_team) +
          1 * (event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PEND5 = sum(event_type == "PENALTY" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),

        iCF = sum({
          event_type %in% st.fenwick_events & event_player_1 == player
        } | {
          event_type == "BLOCKED_SHOT" & event_player_2 == player
        }),
        iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player),
        iSF = sum(event_type %in% st.shot_events & event_player_1 == player),
        ixGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_player_1 == player))),
        G = sum(event_type == "GOAL" & event_player_1 == player),
        A1 = sum(stats::na.omit(event_type == "GOAL" & event_player_2 == player)),
        A2 = sum(stats::na.omit(event_type == "GOAL" & event_player_3 == player)),
        iGVA = sum(event_type == "GIVEAWAY" & event_player_1 == player),
        iTKA = sum(event_type == "TAKEAWAY" & event_player_1 == player),
        iHF = sum(event_type == "HIT" & event_player_1 == player),
        iHA = sum(event_type == "HIT" & event_player_2 == player),
        iBLK = sum(event_type == "BLOCKED_SHOT" & event_player_1 == player),
        iFOW = sum(event_type == "FACEOFF" & event_player_1 == player),
        iFOL = sum(event_type == "FACEOFF" & event_player_2 == player),
        iPENT2 = sum(stats::na.omit(1 * (event_type == "PENALTY" & event_player_1 == player) +
          1 * (event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_player_1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE))),
        iPENT5 = sum(stats::na.omit(event_type == "PENALTY" & event_player_1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
        iPEND2 = sum(stats::na.omit(1 * (event_type == "PENALTY" & event_player_2 == player) +
          1 * (event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENALTY" & event_player_2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE))),
        iPEND5 = sum(stats::na.omit(event_type == "PENALTY" & event_player_2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
      ) %>%
      data.frame() %>%
      return()
  }
}

#' Summarize Goalie Stats
#' @description sum_goalie() summarizes all goalie counting stats from a PBP data frame object x is expected to be a grouped data frame with home_goalie or away_goalie as a grouping variable for venue = "home" and venue = "away" respectively
#'
#' @param x a Corsica PBP data frame object
#' @param venue home or away
#' @importFrom dplyr %>%
#'
#' @return player counting stats for the home or away goalie(s) in the supplied pbp frame
#' @export
st.sum_goalie <- function(x, venue) {
  venue_ <- tolower(as.character(venue))

  if (venue_ == "home") {
    x %>%
      dplyr::rename(player = home_goalie) %>%
      dplyr::summarise(
        venue = "Home",
        team = dplyr::first(home_team),
        GP = length(unique(game_id)),
        TOI = sum(nabs(event_length)) / 60,
        CA = sum(event_type %in% st.corsi_events & event_team == away_team),
        FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
        SA = sum(event_type %in% st.shot_events & event_team == away_team),
        GA = sum(event_type == "GOAL" & event_team == away_team),
        xGA = sum(stats::na.omit((prob_goal / (prob_goal + prob_save)) * (event_type %in% st.shot_events & event_team == away_team)))
      ) %>%
      data.frame() %>%
      return()
  } else if (venue_ == "away") {
    x %>%
      dplyr::rename(player = away_goalie) %>%
      dplyr::summarise(
        venue = "Away",
        team = dplyr::first(away_team),
        GP = length(unique(game_id)),
        TOI = sum(nabs(event_length)) / 60,
        CA = sum(event_type %in% st.corsi_events & event_team == home_team),
        FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
        SA = sum(event_type %in% st.shot_events & event_team == home_team),
        GA = sum(event_type == "GOAL" & event_team == home_team),
        xGA = sum(stats::na.omit((prob_goal / (prob_goal + prob_save)) * (event_type %in% st.shot_events & event_team == home_team)))
      ) %>%
      data.frame() %>%
      return()
  }
}

#' Summarize Team Stats (Old PBP Format)
#' @description old_sum_team() summarizes all team counting stats from a Corsica 1.0 PBP data frame object. x is expected to be a grouped data frame with home_team or away_team as a grouping variable for venue = "home" and venue = "away" respectively
#'
#' @param x a Corsica 1.0 PBP data frame object
#' @param venue home or away
#' @importFrom dplyr %>%
#'
#' @return player counting stats for the home or away players in the supplied pbp frame
#' @export
st.old_sum_team <- function(x, venue) {
  venue_ <- tolower(as.character(venue))

  if (venue_ == "home") {
    x %>%
      dplyr::rename(team = home_team) %>%
      dplyr::summarise(
        venue = "Home",
        GP = length(unique(game_id)),
        TOI = sum(nabs(Event.Length)) / 60,
        CF = sum(event_type %in% st.corsi_events & event_team == team),
        CA = sum(event_type %in% st.corsi_events & event_team == away_team),
        FF = sum(event_type %in% st.fenwick_events & event_team == team),
        FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
        SF = sum(event_type %in% st.shot_events & event_team == team),
        SA = sum(event_type %in% st.shot_events & event_team == away_team),
        GF = sum(event_type == "GOAL" & event_team == team),
        GA = sum(event_type == "GOAL" & event_team == away_team),
        # xGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == team))),
        # xGA = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        # ACF = sum(stats::na.omit(adj_home_corsi * (event_type %in% st.corsi_events & event_team == team))),
        # ACA = sum(stats::na.omit(adj_away_corsi * (event_type %in% st.corsi_events & event_team == away_team))),
        # AFF = sum(stats::na.omit(adj_home_fenwick * (event_type %in% st.fenwick_events & event_team == team))),
        # AFA = sum(stats::na.omit(adj_away_fenwick * (event_type %in% st.fenwick_events & event_team == away_team))),
        # ASF = sum(stats::na.omit(adj_home_shot * (event_type %in% st.shot_events & event_team == team))),
        # ASA = sum(stats::na.omit(adj_away_shot * (event_type %in% st.shot_events & event_team == away_team))),
        # AGF = sum(stats::na.omit(adj_home_goal * (event_type == "GOAL" & event_team == team))),
        # AGA = sum(stats::na.omit(adj_away_goal * (event_type == "GOAL" & event_team == away_team))),
        # AxGF = sum(stats::na.omit(adj_home_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == team))),
        # AxGA = sum(stats::na.omit(adj_away_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
        DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
        NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
        FOW = sum(event_type == "FACEOFF" & event_team == team),
        FOL = sum(event_type == "FACEOFF" & event_team == away_team),
        PENT2 = sum(1 * (event_type == "PENL" & event_team == team) +
          1 * (event_type == "PENL" & event_team == team & grepl("double minor", tolower(event_description)) == TRUE) -
          1 * (event_type == "PENL" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)),
        PENT5 = sum(event_type == "PENL" & event_team == team & grepl("fighting|major", tolower(event_description)) == TRUE),
        PENTS = sum(event_type == "PENL" & event_team == team & grepl("ps \\-", tolower(event_description)) == TRUE),
        PEND2 = sum(1 * (event_type == "PENL" & event_team == away_team) +
          1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_description)) == TRUE) -
          1 * (event_type == "PENL" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)),
        PEND5 = sum(event_type == "PENL" & event_team == away_team & grepl("fighting|major", tolower(event_description)) == TRUE),
        PENDS = sum(event_type == "PENL" & event_team == away_team & grepl("ps \\-", tolower(event_description)) == TRUE),

        GVA = sum(event_type == "GIVEAWAY" & event_team == team),
        TKA = sum(event_type == "TAKEAWAY" & event_team == team),
        HF = sum(event_type == "HIT" & event_team == team),
        HA = sum(event_type == "HIT" & event_team == away_team)
      ) %>%
      data.frame() %>%
      return()
  } else if (venue_ == "away") {
    x %>%
      dplyr::rename(team = away_team) %>%
      dplyr::summarise(
        venue = "Away",
        GP = length(unique(game_id)),
        TOI = sum(nabs(Event.Length)) / 60,
        CF = sum(event_type %in% st.corsi_events & event_team == team),
        CA = sum(event_type %in% st.corsi_events & event_team == home_team),
        FF = sum(event_type %in% st.fenwick_events & event_team == team),
        FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
        SF = sum(event_type %in% st.shot_events & event_team == team),
        SA = sum(event_type %in% st.shot_events & event_team == home_team),
        GF = sum(event_type == "GOAL" & event_team == team),
        GA = sum(event_type == "GOAL" & event_team == home_team),
        # xGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == team))),
        # xGA = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        # ACF = sum(stats::na.omit(adj_away_corsi * (event_type %in% st.corsi_events & event_team == team))),
        # ACA = sum(stats::na.omit(adj_home_corsi * (event_type %in% st.corsi_events & event_team == home_team))),
        # AFF = sum(stats::na.omit(adj_away_fenwick * (event_type %in% st.fenwick_events & event_team == team))),
        # AFA = sum(stats::na.omit(adj_home_fenwick * (event_type %in% st.fenwick_events & event_team == home_team))),
        # ASF = sum(stats::na.omit(adj_away_shot * (event_type %in% st.shot_events & event_team == team))),
        # ASA = sum(stats::na.omit(adj_home_shot * (event_type %in% st.shot_events & event_team == home_team))),
        # AGF = sum(stats::na.omit(adj_away_goal * (event_type == "GOAL" & event_team == team))),
        # AGA = sum(stats::na.omit(adj_home_goal * (event_type == "GOAL" & event_team == home_team))),
        # AxGF = sum(stats::na.omit(adj_away_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == team))),
        # AxGA = sum(stats::na.omit(adj_home_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
        DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
        NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
        FOW = sum(event_type == "FACEOFF" & event_team == team),
        FOL = sum(event_type == "FACEOFF" & event_team == home_team),
        PENT2 = sum(1 * (event_type == "PENL" & event_team == team) +
          1 * (event_type == "PENL" & event_team == team & grepl("double minor", tolower(event_description)) == TRUE) -
          1 * (event_type == "PENL" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)),
        PENT5 = sum(event_type == "PENL" & event_team == team & grepl("fighting|major", tolower(event_description)) == TRUE),
        PENTS = sum(event_type == "PENL" & event_team == team & grepl("ps \\-", tolower(event_description)) == TRUE),
        PEND2 = sum(1 * (event_type == "PENL" & event_team == home_team) +
          1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_description)) == TRUE) -
          1 * (event_type == "PENL" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)),
        PEND5 = sum(event_type == "PENL" & event_team == home_team & grepl("fighting|major", tolower(event_description)) == TRUE),
        PENDS = sum(event_type == "PENL" & event_team == home_team & grepl("ps \\-", tolower(event_description)) == TRUE),
        GVA = sum(event_type == "GIVEAWAY" & event_team == team),
        TKA = sum(event_type == "TAKEAWAY" & event_team == team),
        HF = sum(event_type == "HIT" & event_team == team),
        HA = sum(event_type == "HIT" & event_team == home_team)
      ) %>%
      data.frame() %>%
      return()
  }
}

#' Summarize Skater Stats (Old PBP Format)
#' @description old_sum_skater() summarizes all skater counting stats from a Corsica 1.0 PBP data frame object. x is expected to be a grouped data frame with home_on_x or away_on_x as a grouping variable  for venue = "home" and venue = "away" respectively.
#'
#' @note A dplyr::rename() argument must be passed before sum_skater() to convert home/away_on_x to player
#'
#' @param x a Corsica 1.0 PBP data frame object
#' @param venue home or away
#' @importFrom dplyr %>%
#'
#' @return player counting stats for the home or away players in the supplied pbp frame
#' @export
st.old_sum_skater <- function(x, venue) {
  venue_ <- tolower(as.character(venue))

  if (venue_ == "home") {
    x %>%
      dplyr::summarise(
        venue = "Home",
        team = dplyr::first(home_team),
        GP = length(unique(game_id)),
        TOI = sum(nabs(Event.Length)) / 60,
        CF = sum(event_type %in% st.corsi_events & event_team == home_team),
        CA = sum(event_type %in% st.corsi_events & event_team == away_team),
        FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
        FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
        SF = sum(event_type %in% st.shot_events & event_team == home_team),
        SA = sum(event_type %in% st.shot_events & event_team == away_team),
        GF = sum(event_type == "GOAL" & event_team == home_team),
        GA = sum(event_type == "GOAL" & event_team == away_team),
        # xGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        # xGA = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        # ACF = sum(stats::na.omit(adj_home_corsi * (event_type %in% st.corsi_events & event_team == home_team))),
        # ACA = sum(stats::na.omit(adj_away_corsi * (event_type %in% st.corsi_events & event_team == away_team))),
        # AFF = sum(stats::na.omit(adj_home_fenwick * (event_type %in% st.fenwick_events & event_team == home_team))),
        # AFA = sum(stats::na.omit(adj_away_fenwick * (event_type %in% st.fenwick_events & event_team == away_team))),
        # ASF = sum(stats::na.omit(adj_home_shot * (event_type %in% st.shot_events & event_team == home_team))),
        # ASA = sum(stats::na.omit(adj_away_shot * (event_type %in% st.shot_events & event_team == away_team))),
        # AGF = sum(stats::na.omit(adj_home_goal * (event_type == "GOAL" & event_team == home_team))),
        # AGA = sum(stats::na.omit(adj_away_goal * (event_type == "GOAL" & event_team == away_team))),
        # AxGF = sum(stats::na.omit(adj_home_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        # AxGA = sum(stats::na.omit(adj_away_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
        DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
        NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
        PENT2 = sum(1 * (event_type == "PENL" & event_team == home_team) +
          1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENL" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PENT5 = sum(event_type == "PENL" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
        PEND2 = sum(1 * (event_type == "PENL" & event_team == away_team) +
          1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENL" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PEND5 = sum(event_type == "PENL" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),

        iCF = sum(event_type %in% st.corsi_events & p1 == player),
        iFF = sum(event_type %in% st.fenwick_events & p1 == player),
        iSF = sum(event_type %in% st.shot_events & p1 == player),
        ixGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & p1 == player))),
        G = sum(event_type == "GOAL" & p1 == player),
        A1 = sum(stats::na.omit(event_type == "GOAL" & p2 == player)),
        A2 = sum(stats::na.omit(event_type == "GOAL" & p3 == player)),
        iGVA = sum(event_type == "GIVEAWAY" & p1 == player),
        iTKA = sum(event_type == "TAKEAWAY" & p1 == player),
        iHF = sum(event_type == "HIT" & p1 == player),
        iHA = sum(event_type == "HIT" & p2 == player),
        iBLK = sum(event_type == "BLOCKED_SHOT" & p2 == player),
        iFOW = sum(event_type == "FACEOFF" & p1 == player),
        iFOL = sum(event_type == "FACEOFF" & p2 == player),
        iPENT2 = sum(stats::na.omit(1 * (event_type == "PENL" & p1 == player) +
          1 * (event_type == "PENL" & p1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENL" & p1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE))),
        iPENT5 = sum(stats::na.omit(event_type == "PENL" & p1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
        iPEND2 = sum(stats::na.omit(1 * (event_type == "PENL" & p2 == player) +
          1 * (event_type == "PENL" & p2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENL" & p2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE))),
        iPEND5 = sum(stats::na.omit(event_type == "PENL" & p2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
      ) %>%
      data.frame() %>%
      return()
  } else if (venue_ == "away") {
    x %>%
      dplyr::summarise(
        venue = "Away",
        team = dplyr::first(away_team),
        GP = length(unique(game_id)),
        TOI = sum(nabs(Event.Length)) / 60,
        CF = sum(event_type %in% st.corsi_events & event_team == away_team),
        CA = sum(event_type %in% st.corsi_events & event_team == home_team),
        FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
        FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
        SF = sum(event_type %in% st.shot_events & event_team == away_team),
        SA = sum(event_type %in% st.shot_events & event_team == home_team),
        GF = sum(event_type == "GOAL" & event_team == away_team),
        GA = sum(event_type == "GOAL" & event_team == home_team),
        # xGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        # xGA = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        # ACF = sum(stats::na.omit(adj_away_corsi * (event_type %in% st.corsi_events & event_team == away_team))),
        # ACA = sum(stats::na.omit(adj_home_corsi * (event_type %in% st.corsi_events & event_team == home_team))),
        # AFF = sum(stats::na.omit(adj_away_fenwick * (event_type %in% st.fenwick_events & event_team == away_team))),
        # AFA = sum(stats::na.omit(adj_home_fenwick * (event_type %in% st.fenwick_events & event_team == home_team))),
        # ASF = sum(stats::na.omit(adj_away_shot * (event_type %in% st.shot_events & event_team == away_team))),
        # ASA = sum(stats::na.omit(adj_home_shot * (event_type %in% st.shot_events & event_team == home_team))),
        # AGF = sum(stats::na.omit(adj_away_goal * (event_type == "GOAL" & event_team == away_team))),
        # AGA = sum(stats::na.omit(adj_home_goal * (event_type == "GOAL" & event_team == home_team))),
        # AxGF = sum(stats::na.omit(adj_away_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == away_team))),
        # AxGA = sum(stats::na.omit(adj_home_goal * prob_goal * (event_type %in% st.fenwick_events & event_team == home_team))),
        OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != away_rinkside),
        DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == away_rinkside),
        NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
        PENT2 = sum(1 * (event_type == "PENL" & event_team == away_team) +
          1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENL" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PENT5 = sum(event_type == "PENL" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
        PEND2 = sum(1 * (event_type == "PENL" & event_team == home_team) +
          1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENL" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)),
        PEND5 = sum(event_type == "PENL" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),

        iCF = sum(event_type %in% st.corsi_events & p1 == player),
        iFF = sum(event_type %in% st.fenwick_events & p1 == player),
        iSF = sum(event_type %in% st.shot_events & p1 == player),
        ixGF = sum(stats::na.omit(prob_goal * (event_type %in% st.fenwick_events & p1 == player))),
        G = sum(event_type == "GOAL" & p1 == player),
        A1 = sum(stats::na.omit(event_type == "GOAL" & p2 == player)),
        A2 = sum(stats::na.omit(event_type == "GOAL" & p3 == player)),
        iGVA = sum(event_type == "GIVEAWAY" & p1 == player),
        iTKA = sum(event_type == "TAKEAWAY" & p1 == player),
        iHF = sum(event_type == "HIT" & p1 == player),
        iHA = sum(event_type == "HIT" & p2 == player),
        iBLK = sum(event_type == "BLOCKED_SHOT" & p2 == player),
        iFOW = sum(event_type == "FACEOFF" & p1 == player),
        iFOL = sum(event_type == "FACEOFF" & p2 == player),
        iPENT2 = sum(stats::na.omit(1 * (event_type == "PENL" & p1 == player) +
          1 * (event_type == "PENL" & p1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENL" & p1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE))),
        iPENT5 = sum(stats::na.omit(event_type == "PENL" & p1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
        iPEND2 = sum(stats::na.omit(1 * (event_type == "PENL" & p2 == player) +
          1 * (event_type == "PENL" & p2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
          1 * (event_type == "PENL" & p2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE))),
        iPEND5 = sum(stats::na.omit(event_type == "PENL" & p2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
      ) %>%
      data.frame() %>%
      return()
  }
}

#' Summarize Goalie Stats (Old PBP Format)
#' @description old_sum_goalie() summarizes all goalie counting stats from a Corsica 1.0 PBP data frame object. x is expected to be a grouped data frame with home_goalie or away_goalie as a grouping variable for venue = "home" and venue = "away" respectively.
#'
#' @param x a Corsica 1.0 PBP data frame object
#' @param venue home or away
#' @importFrom dplyr %>%
#'
#' @return goalie counting stats for the home or away goalie(s) in the supplied pbp frame
#' @export
st.old_sum_goalie <- function(x, venue) {
  venue_ <- tolower(as.character(venue))

  if (venue_ == "home") {
    x %>%
      dplyr::rename(player = home_goalie) %>%
      dplyr::summarise(
        venue = "Home",
        team = dplyr::first(home_team),
        GP = length(unique(game_id)),
        TOI = sum(nabs(Event.Length)) / 60,
        CA = sum(event_type %in% st.corsi_events & event_team == away_team),
        FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
        SA = sum(event_type %in% st.shot_events & event_team == away_team),
        GA = sum(event_type == "GOAL" & event_team == away_team),
        xGA = sum(stats::na.omit((prob_goal / (prob_goal + prob_save)) * (event_type %in% st.shot_events & event_team == away_team)))
      ) %>%
      data.frame() %>%
      return()
  } else if (venue_ == "away") {
    x %>%
      dplyr::rename(player = away_goalie) %>%
      dplyr::summarise(
        venue = "Away",
        team = dplyr::first(away_team),
        GP = length(unique(game_id)),
        TOI = sum(nabs(Event.Length)) / 60,
        CA = sum(event_type %in% st.corsi_events & event_team == home_team),
        FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
        SA = sum(event_type %in% st.shot_events & event_team == home_team),
        GA = sum(event_type == "GOAL" & event_team == home_team),
        xGA = sum(stats::na.omit((prob_goal / (prob_goal + prob_save)) * (event_type %in% st.shot_events & event_team == home_team)))
      ) %>%
      data.frame() %>%
      return()
  }
}

#' Numeric Absolute
#' @description nabs() returns x after first converting it to class numeric via character. Its primary use is converting objects of class factor to numeric. It also provides a more concise wrapper for standard numeric conversion.
#'
#' @param x the value to coerce to numeric via character.
#'
#' @return a numeric value from x
#' @export
nabs <- function(x) {
  return(as.numeric(as.character(x)))
}

#' Logarithmic Loss
#' @description log_loss() returns the logarithmic loss obtained from a given prediction and known result. The allow_inf parameter controls whether infinite loss is allowed (default is FALSE). Setting allow_inf to FALSE will cause large but finite penalties at the extremes.
#'
#' @param act actual result(s)
#' @param pred predicted result(s)
#' @param allow_inf whether to allow infinite penalties
#'
#' @return the log loss score for this data or series of data
#' @export
log_loss <- function(act, pred, allow_inf = FALSE) {
  eps <- as.numeric(!allow_inf) * 1e-15

  pred <- matrix(sapply(pred, function(x) max(eps, x)),
    nrow = nrow(pred)
  )
  pred <- matrix(sapply(pred, function(x) min(1 - eps, x)),
    nrow = nrow(pred)
  )

  ll <- sum(act * log(pred) + (1 - act) * log(1 - pred))
  ll <- -ll / (nrow(act))

  return(ll)
}

#' Moving Average
#' @description moving() returns a vector of averages obtained from the n elements of x preceding and including the element at each respective index
#'
#' @param x a numerical vector to calculate the moving average
#' @param n the number of elements to include in the moving average
#'
#' @return the vector of the moving average.
#' @export
moving <- function(x, n = 5) {

  ## Description
  #

  if (length(x) < n) {
    v <- NA
  } else {
    stats::filter(x,
      rep(1 / n, n),
      sides = 1
    ) ->
    v
  }

  return(as.numeric(v))
}

#' Brier Score
#' @description brier() returns the Brier score obtained from a given prediction and known result
#'
#' @param act actual result(s)
#' @param pred predicted result(s)
#'
#' @return the brier score for this data or series of data
#' @export
brier <- function(act, pred) {
  bri <- sum((act - pred)^2) / length(act)
  return(bri)
}

#' NA if NULL
#' @description na_if_null() returns an object's value if it is not NULL and NA otherwise
#' @param x the object to try return
#'
#' @return the object's value, or NA
#' @export
na_if_null <- function(x) {
  return(ifelse(is.null(x) == TRUE,
    NA,
    x
  ))
}

#' Do Call Apply
#' @description dcapply() uses do.call() to merge the products of an applied function according to specifications. The function will be applied in parallel if cores >= 1
#'
#' @param x the data to split and apply a function
#' @param fun the function to apply to the data
#' @param combine the method to recombine the data
#' @param cores the number of cores to use
#' @param ... additional parameters to lapply on each chunk
#' @importFrom foreach %dopar%
#' @importFrom dplyr %>%
#'
#' @return The data with the function applied
#' @export
dcapply <- function(x, fun, combine, cores, ...) {
  if (cores > 1) {
    cl <- parallel::makeCluster(3)
    doParallel::registerDoParallel(cl)

    doParallel::registerDoParallel(cores = cores)

    chunks <- split(x, cut(1:length(x), cores))

    foreach::foreach(i = 1:cores, .combine = c) %dopar% {
      chunks[[i]] %>%
        lapply(fun, ...)
    } -> list

    combined <- do.call(combine, list)

    parallel::stopCluster(cl)
  } else {
    list <- lapply(x, fun, ...)

    combined <- do.call(combine, list)
  }

  return(combined)
}

#' NA as String
#' @description na_as_string() returns a character vector with NA values replaced as "NA"
#'
#' @param x a character vector that may contain NA values to coerce to string "NA"
#'
#' @return a character vector with NA coerced to string "NA"
#' @export
na_as_string <- function(x) {
  x <- as.character(x)

  x[which(is.na(x) == TRUE)] <- "NA"

  return(x)
}

#' NA as Zero
#' @description na_as_zero() returns a numeric vector with NA values replaced as 0
#'
#' @param x a numeric vector that may contain NA values to coerce to 0
#'
#' @return a vector with NA values coerced to 0
#' @export
na_as_zero <- function(x) {
  x <- nabs(x)

  x[which(is.na(x) == TRUE)] <- 0

  return(x)
}

#' F Table to Data Frame
#' @description ftable2df() returns a data.frame from an ftable object
#'
#' @param mydata an ftable object to be coerced to a data.frame
#'
#' @return a data frame coerced from mydata
#' @export
ftable2df <- function(mydata) {
  ifelse(class(mydata) == "ftable",
    mydata <- mydata,
    mydata <- stats::ftable(mydata)
  )

  dfrows <- rev(expand.grid(rev(attr(mydata, "row.vars"))))

  dfcols <- as.data.frame.matrix(mydata)

  do.call(
    paste,
    c(rev(expand.grid(rev(attr(mydata, "col.vars")))),
      sep = "_"
    )
  ) -> names(dfcols)

  cbind(dfrows, dfcols)
}

#' Scrape & compile & savel all (or subset of) games in a season range. Saves a file in season dir with all unscraped games. All seasons scraping the same game
#'
#' @param seasons The season(s) to scrape. Will automatically skip 20042005 and shorten 20122013 and carry longer seasons 20172018++
#' @param games the games to scrape
#' @param data_dir The directory for storing data
#' @param pause Pause time between scrapes
#' @param try_tolerance Times to retry on fail.
#'
#' @return Integer number of games scraped
#' @export
ds.compile_all_games <- function(seasons = c("20172018"), games = ds.get_all_games(), data_dir = "./data", pause = 2, try_tolerance = 5) {
  p <- dplyr::progress_estimated(length(seasons) * length(games), min_time = 10)
  gs <- 0
  for (season in seasons) {
    if (season == "20042005") {
      for (i in 1:length(games)) {
        p$tick()
      }
      p$print()
      next
    }
    ddir <- file.path(data_dir, substring(season, 5, 8))
    if (!file.exists(ddir)) {
      dir.create(ddir, recursive = TRUE)
    }
    gms <- games
    if (as.numeric(substring(season, 5, 8)) < 2018) {
      gms[!(gms %in% 21231:21271)]
    }
    if (season == "20122013") {
      gms[!(gms %in% 20721:21271)]
    }
    for (game in gms) {
      tryCatch({
        pbp_list <- ds.compile_games(
          games = game,
          season = season,
          pause = pause,
          try_tolerance = try_tolerance,
          agents = hockeyR::ds.get_user_agents()
        )
        pbp_df <- pbp_list[[1]]
        readr::write_delim(pbp_df, file.path(ddir, as.character(game)), delim = "|")
        gs <- gs + 1
      },
      error = function(e) {
        message("Error : ", game, ", ", e)
        cat(paste0(game, ", "), file = file.path(ddir, "errors.csv"), append = TRUE)
      }
      )

      p$tick()$print()
      if ((as.integer(game) %% 100) == 0) {
        gc(verbose = FALSE)
        p$pause(60)
      }
    }
  }
  return(gs)
}

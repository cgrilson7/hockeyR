% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dryscrape.R
\name{ds.compile_all_games}
\alias{ds.compile_all_games}
\title{Scrape & compile & savel all (or subset of) games in a season range. Saves a file in season dir with all unscraped games. All seasons scraping the same game}
\usage{
ds.compile_all_games(seasons = c("20172018"),
  games = ds.get_all_games(), data_dir = "./data", pause = 2,
  try_tolerance = 5)
}
\arguments{
\item{seasons}{The season(s) to scrape. Will automatically skip 20042005 and shorten 20122013 and carry longer seasons 20172018++}

\item{games}{the games to scrape}

\item{data_dir}{The directory for storing data}

\item{pause}{Pause time between scrapes}

\item{try_tolerance}{Times to retry on fail.}
}
\value{
Integer number of games scraped
}
\description{
Scrape & compile & savel all (or subset of) games in a season range. Saves a file in season dir with all unscraped games. All seasons scraping the same game
}

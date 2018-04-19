context("test_dryscrape_ds")

test_that("attribution attributes", {
  expect_equal(ds.attribution(), "# Manny's HTM scrape - 09//15//17 \n# This code was written by Emmanuel Perry of @mannyelk on twitter and creator of corsica.hockey.\n# All credit to him for creation of the script and can found at github.com/mannyelk \n# DRYSCRAPE # Last edit: Manny (2017-07-02)\n# Description Dryscrape contains all functions and tools related to scraping data for Corsica")
})

test_that("scraper scrapes", {

  expect_warning(expect_equal(ds.compile_all_games(seasons="20172018",
                                                   data_dir = ".",
                                                   games=20001),
                              1))
  expect_true(file.exists("./2018/20001.pbp"))
  expect_true(file.exists("./2018/20001.shifts"))
  expect_true(file.exists("./2018/20001.roster"))
  expect_message(pbp<-readr::read_delim("./2018/20001.pbp", delim="|"))
  expect_message(roster<-readr::read_delim("./2018/20001.roster", delim="|"))
  expect_message(shifts<-readr::read_delim("./2018/20001.shifts", delim="|"))

  expect_equal(dim(pbp), c(967, 41))
  expect_equal(dim(roster), c(39, 15))
  expect_equal(dim(shifts), c(821, 18))


  teardown({
    file.remove("./2018/20001.pbp")
    file.remove("./2018/20001.roster")
    file.remove("./2018/20001.shifts")
    unlink("./2018", recursive = TRUE)
  })
})

context("test_dryscrape_ds")

test_that("attribution attributes", {
  expect_equal(ds.attribution(), "# Manny's HTM scrape - 09//15//17 \n# This code was written by Emmanuel Perry of @mannyelk on twitter and creator of corsica.hockey.\n# All credit to him for creation of the script and can found at github.com/mannyelk \n# DRYSCRAPE # Last edit: Manny (2017-07-02)\n# Description Dryscrape contains all functions and tools related to scraping data for Corsica")
})

test_that("A plot is returned for a storm track (AL011851)", {
  hurdat <- StormR::hurdat
  # Filter for AL011851
  hurdat <- subset(hurdat, StormID == "AL011851")
  expect_s3_class(plot_storm_track(hurdat), "gg")
})

test_that("A plot is returned for all storm tracks", {
  hurdat <- StormR::hurdat
  # Filter for AL012022
  hurdat <- subset(hurdat, StormID == "AL012022")
  expect_s3_class(plot_storm_track(hurdat), "gg")
})

test_that("A plot is returned for a storm track with only one entry (AL021851)", {
  hurdat <- StormR::hurdat
  # Filter for AL021851
  hurdat <- subset(hurdat, StormID == "AL021851")
  expect_s3_class(plot_storm_track(hurdat), "gg")
})
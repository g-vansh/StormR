test_that("A plot is returned for a storm track (AL011851)", {
  hurdat <- StormR::hurdat
  # Filter for AL011851
  hurdat <- subset(hurdat, StormID == "AL011851")
  expect_s3_class(plot_storm_track(hurdat), "gg")
})

test_that("A plot is returned for 4 storm tracks", {
  hurdat <- StormR::hurdat
  # Subset to the first 50 rows
  hurdat <- hurdat[1:50,]
  expect_s3_class(plot_storm_track(hurdat), "gg")
})

test_that("A plot is returned for a storm track with only one entry (AL021851)", {
  hurdat <- StormR::hurdat
  # Filter for AL021851
  hurdat <- subset(hurdat, StormID == "AL021851")
  expect_s3_class(plot_storm_track(hurdat), "gg")
})
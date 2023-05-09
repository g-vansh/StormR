test_that("Storm Track is Interpolated for Hurdat Storm AL011851", {
  hurdat <- StormR::hurdat
  # Filter for AL011851
  hurdat <- subset(hurdat, StormID == "AL011851")
  expect_no_error(interpolate_storm_track(hurdat))
})

test_that("Storm Track is Interpolated for Hurdat Storm AL021851, which has only one Row Entry", {
  hurdat <- StormR::hurdat
  # Filter for AL021851
  hurdat <- subset(hurdat, StormID == "AL021851")
  expect_no_error(interpolate_storm_track(hurdat))
})

test_that("Empty DataFrame is Returned for Empty DataFrame", {
  expect_no_error(interpolate_storm_track(data.frame()))
})
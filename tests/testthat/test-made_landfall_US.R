test_that("Landfall is checked correctly for a true return (AL011851)", {
  hurdat <- StormR::hurdat
  # Filter for AL011851
  hurdat <- subset(hurdat, StormID == "AL011851")
  expect_true(made_landfall_US(hurdat))
})

test_that("Landfall is checked correctly for a false return (AL112022)", {
  hurdat <- StormR::hurdat
  # Filter for AL112022
  hurdat <- subset(hurdat, StormID == "AL112022")
  expect_false(made_landfall_US(hurdat))
})
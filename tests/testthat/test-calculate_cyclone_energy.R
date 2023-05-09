test_that("Cyclone Energy is Calculated for 0 Wind", {
  # See if the cyclone energy is calculated correctly
  test_df <- data.frame(StormID = "A", Date = 18511019, Time = 1200, MaximumWind = 0)
  expect_equal(calculate_cyclone_energy(test_df), 0.0)
})

test_that("Cyclone Energy is Calculated for 100 Wind", {
  # See if the cyclone energy is calculated correctly
  test_df <- data.frame(StormID = "A", Date = 18511019, Time = 1200, MaximumWind = 100)
  expect_equal(calculate_cyclone_energy(test_df), 1.0)
})

test_that("Cyclone Energy is Calculated for 2 Row Data", {
  # See if the cyclone energy is calculated correctly
  test_df <- data.frame(StormID = c("A", "A"), Date = c(18511019, 18511019), Time = c(1200, 1900), MaximumWind = c(0, 100))
  expect_equal(calculate_cyclone_energy(test_df), 1.0)
})

test_that("Cyclone Energy is Calculated for 2 Row Data with Low Time Difference", {
  # See if the cyclone energy is calculated correctly
  test_df <- data.frame(StormID = c("A", "A"), Date = c(18511019, 18511019), Time = c(1200, 1300), MaximumWind = c(100, 100))
  expect_equal(calculate_cyclone_energy(test_df), 1.0)
})

test_that("Cyclone Energy is Calculated for 2 Row Data with Low Time Difference and Discrepancies", {
  # See if the cyclone energy is calculated correctly
  test_df <- data.frame(StormID = c("A", "A"), Date = c(18511019, 18511019), Time = c(1200, 1300), MaximumWind = c(70, 100))
  expect_equal(calculate_cyclone_energy(test_df), 1.0)
})
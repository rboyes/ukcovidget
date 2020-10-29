context("Simple basic test for ukcovidget")

library(ukcovidget)

testthat::test_that("Get the correct number of cases as 2020-04-01", {
  query_filters <- c("areaType=overview", "date=2020-04-01")
  query_structure <- list(cumulative = "cumCasesBySpecimenDate")
  df_cases <- ukcovidget::get_data(query_filters, query_structure)

  testthat::expect_equal(class(df_cases), "data.frame")
  testthat::expect_equal(nrow(df_cases), 1)
  testthat::expect_equal(ncol(df_cases), 1)
  testthat::expect_equal(colnames(df_cases)[1], "cumulative")
  testthat::expect_equal(df_cases$cumulative[1], 43388) # Hopefully this won't change!
})

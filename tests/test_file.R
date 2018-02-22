library("FSH")

testthat::test_that("Only a warning occurs on a bad file name",
          {testthat::expect_warning(FSH:::fars_read_years(14))})


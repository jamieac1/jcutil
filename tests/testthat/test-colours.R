test_that("check n>=1", {
  expect_error(make_shades("goldenrod", -1), "n must be at least 1")
  expect_error(make_shades("goldenrod", 0), "n must be at least 1")
})

test_that("lighter must be logical TRUE or FALSE", {
  expect_error(make_shades("goldenrod", 3, 8),
               "lighter must be logical TRUE or FALSE")
})

test_that("check that plot works", {
  expect_s3_class(plot_colours2("red", 5), "ggplot")
})

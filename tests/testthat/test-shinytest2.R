library(shinytest2)

test_that("Initial Shiny values are consistent", {
  app <- AppDriver$new()
  app$set_inputs(
    time = 1
  )

  expect_equal(
    unname(unlist(app$get_values(export = "user1"))),
    349
  )




})

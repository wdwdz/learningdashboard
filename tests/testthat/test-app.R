test_that("valuebox change by week", {
  testServer(server, {
    session$setInputs(time = 1)
    expect_equal(user()$y, 349)
  })
})

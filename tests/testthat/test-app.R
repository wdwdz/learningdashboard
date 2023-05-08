test_that("valuebox change by week log users", {
  testServer(server, {
    session$setInputs(time = 1)
    expect_equal(user()$y, 349)
  })
})

test_that("valuebox change by week log comments", {
  testServer(server, {
    session$setInputs(time = 13)
    expect_equal(comment(), 124)
  })
})

test_that("valuebox change by week log comments", {
  testServer(server, {
    session$setInputs(time = 7)
    expect_equal(post(), 121)
  })
})

test_that("userinfo", {
  testServer(server, {
    session$setInputs(searchText = "sn5289", searchButton = "TRUE")
    expect_equal(userinfo()$netid, "sn5289")
  })
})





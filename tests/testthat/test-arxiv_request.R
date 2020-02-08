test_that("arxiv_request() works when batches are not necessary", {
  result <-
    arxiv_request(
      query = "cat:cs.AI",
      start = 0,
      limit = 10,
      batch_size = 10
    )

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 10)
  expect_equal(nrow(distinct(result, id)), 10)
})

test_that("arxiv_request() works when batches are necessary", {
  result <-
    arxiv_request(
      query = "cat:cs.AI",
      start = 0,
      limit = 10,
      batch_size = 2,
      sleep = 1
    )

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 10)
  expect_equal(nrow(distinct(result, id)), 10)
})

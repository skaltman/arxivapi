test_that("arxiv_batches() returns a tibble with the correct number of rows", {
  limit <- 5
  result <-
    arxiv_batches(
      query = "cs.AI",
      start = 0,
      limit = limit,
      batch_size = 1,
      sleep = 2,
    )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
})

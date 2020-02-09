
#' Sleep, then retrieve batch
#'
#' @param query String with the API query
#' @param start Index to start at.
#' @param limit Maximum number of papers to return.
#' @param batch_size Batch size
#' @param batch_number Batch index
#' @param sleep Seconds to sleep between batches
#' @param timeout Timeout seconds
#' @param sep Separator
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \donttest{
#' arxiv_sleep_then_batch(
#'   query = "cat:cs.AI",
#'   start = 0,
#'   limit = 5,
#'   batch_size = 1,
#'   sleep = 1
#' )
#' }
arxiv_sleep_then_batch <- function(
  query,
  start,
  limit,
  batch_size,
  batch_number,
  sleep,
  timeout = 30,
  sep = "|"
) {

  Sys.sleep(sleep)

  result <-
    arxiv_request(query, start, limit, batch_size, timeout = timeout, sep = sep)

  while (nrow(result) != limit) {
    message("Batch not fully retrieved. Sleeping for 1 minute.")
    Sys.sleep(60)

    result <-
      bind_rows(
        result,
        arxiv_request(
          query,
          start = nrow(result),
          limit = limit,
          batch_size = batch_size,
          timeout = timeout,
          sep = sep
        )
      )
  }

  message("Retrieved batch ", batch_number)

  result
}

#' Query the arXiv API in batches
#'
#'
#' @param query String with the API query
#' @param start Index to start at.
#' @param limit Maximum number of papers to return.
#' @param batch_size batch_size
#' @param sleep Seconds to sleep between batches
#' @param timeout Timeout seconds
#' @param sep Separator
#'
#' @return A tibble of papers
#' @export
#'
#' @examples
#' \donttest{
#' arxiv_batches(
#'   query = "cat:cs.AI",
#'   start = 0,
#'   limit = 5,
#'   batch_size = 1,
#'   sleep = 1
#' )
#' }
arxiv_batches <- function(
  query, start, limit, batch_size, sleep, timeout = 30, sep = "|"
) {
  n_batch <- (limit %/% batch_size) + if_else(limit %% batch_size != 0, 1, 0)
  max_record <- start + limit - 1

  result <-
    tibble(
      start = seq(start, start + limit - 1, by = batch_size),
      limit =
        if_else(
          max_record - start + 1 < batch_size,
          max_record - start + 1,
          batch_size
        )
    ) %>%
    mutate(n = row_number()) %>%
    pmap_dfr(
      ~ arxiv_sleep_then_batch(
        query = query,
        start = ..1,
        limit = ..2,
        batch_size = batch_size,
        batch_number  = ..3,
        sleep = sleep,
        sep = sep
      )
    )

  result
}

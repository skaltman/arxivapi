#' Query the arXiv API
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
#' @return A tibble
#' @export
#'
#' @examples
#' \donttest{
#' arxiv_request(
#'   query = "cat:cs.AI",
#'   start = 0,
#'   limit = 5,
#'   batch_size = 1,
#'   sleep = 1
#' )
#' }
arxiv_request <-
  function(query, start, limit, batch_size, sleep = 0, timeout = 30, sep = "|") {

    url <- "http://export.arxiv.org/api/query"
    body <-
      list(
        search_query = query,
        start = start,
        max_results = limit,
        sortOrder = "ascending"
      )

    if (limit > batch_size) {
      arxiv_batches(
        query = query,
        start = start,
        limit = limit,
        batch_size = batch_size,
        sleep = sleep,
        timeout = timeout,
        sep = sep
      )
    } else {
      send_request(url, body, timeout) %>%
        arxiv_as_tibble(sep = sep)
    }
  }

send_request <- function(url, body, timeout) {
  search_result <-
    try(
      httr::POST(
        url,
        body = body,
        httr::timeout(timeout)
      )
    )

  httr::stop_for_status(search_result)
}

arxiv_as_tibble <- function(search_result, sep = sep) {
  aRxiv:::result2list(search_result) %>%
    aRxiv:::get_entries() %>%
    aRxiv:::listresult2df(sep = sep) %>%
    as_tibble()
}

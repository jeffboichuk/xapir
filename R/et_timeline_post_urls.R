#' Extract Post URLs from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to extract post URLs
#' from tweets in the timeline.
#'
#' @importFrom purrr map_dfr pluck map_chr
#' @importFrom dplyr select distinct
#' @template timeline
#' @return A tibble containing extracted URLs along with their tweet IDs and associated metadata.
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(
#'   username = "Tesla",
#'   max_results = 100,
#'   bearer_token = Sys.getenv("X_BEARER_TOKEN")
#' )
#' post_urls <- get_timeline_post_urls(timeline)
#' }
#' @export
get_timeline_post_urls <- function(timeline) {

  # Validate input
  if (missing(timeline) || !is.list(timeline)) {
    stop("`timeline` must be a valid list containing timeline data.")
  }

  # Extract URLs and associated tweet metadata from the timeline response
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        tibble(
          tweet_id = .x$id %||% NA_character_,
          post_url = if (!is.null(.x$id) && !is.null(.x$author_id)) {
            paste0("https://x.com/", .x$author_id, "/status/", .x$id)
          } else {
            NA_character_
          }
        )
      }
    ) |>
    distinct() ->
    post_urls_tibble

  return(post_urls_tibble)
}

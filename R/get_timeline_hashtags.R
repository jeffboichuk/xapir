#' Extract Hashtags from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to extract hashtags
#' from tweets in the timeline.
#'
#' @importFrom purrr map_dfr pluck map_chr
#' @importFrom dplyr select distinct
#' @template timeline
#' @return A tibble containing extracted hashtags.
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(
#'   username = "Tesla",
#'   max_results = 100,
#'   bearer_token = Sys.getenv("X_BEARER_TOKEN")
#' )
#' hashtags <- get_timeline_hashtags(timeline)
#' }
#' @export
get_timeline_hashtags <- function(timeline) {

  # Validate input
  if (missing(timeline) || !is.list(timeline)) {
    stop("`timeline` must be a valid list containing timeline data.")
  }

  # Extract hashtags from the timeline response
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        tibble(
          tweet_id  = .x$id %||% NA_character_,
          hashtags  = map_chr(.x$entities$hashtags, ~ .x$tag %||% NA_character_, .null = NA_character_)
        )
      }
    ) |>
    distinct() ->
    hashtags_tibble

  return(hashtags_tibble)
}

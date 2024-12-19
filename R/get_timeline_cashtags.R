#' Extract Cashtags from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to extract cashtags
#' from tweets in the timeline.
#'
#' @importFrom purrr map_dfr pluck map_chr
#' @importFrom dplyr select distinct
#' @template timeline
#' @return A tibble containing extracted cashtags.
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(
#'   username = "Tesla",
#'   max_results = 100,
#'   bearer_token = Sys.getenv("X_BEARER_TOKEN")
#' )
#' cashtags <- get_timeline_cashtags(timeline)
#' }
#' @export
get_timeline_cashtags <- function(timeline) {

  # Validate input
  if (missing(timeline) || !is.list(timeline)) {
    stop("`timeline` must be a valid list containing timeline data.")
  }

  # Extract cashtags from the timeline response
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        tibble(
          tweet_id  = .x$id %||% NA_character_,
          cashtags  = map_chr(.x$entities$cashtags, ~ .x$tag %||% NA_character_, .null = NA_character_)
        )
      }
    ) |>
    distinct() ->
    cashtags_tibble

  return(cashtags_tibble)
}

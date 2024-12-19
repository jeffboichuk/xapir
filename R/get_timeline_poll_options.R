#' Extract Poll Options from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to extract poll options
#' from tweets in the timeline.
#'
#' @importFrom purrr map_dfr pluck map_chr
#' @importFrom dplyr select distinct
#' @template timeline
#' @return A tibble containing extracted poll options.
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(
#'   username = "Tesla",
#'   max_results = 100,
#'   bearer_token = Sys.getenv("X_BEARER_TOKEN")
#' )
#' poll_options <- get_timeline_poll_options(timeline)
#' }
#' @export
get_timeline_poll_options <- function(timeline) {

  # Validate input
  if (missing(timeline) || !is.list(timeline)) {
    stop("`timeline` must be a valid list containing timeline data.")
  }

  # Extract poll options from the timeline response
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        tibble(
          tweet_id     = .x$id %||% NA_character_,
          poll_options = map_chr(.x$entities$polls$options, ~ .x$text %||% NA_character_, .null = NA_character_)
        )
      }
    ) |>
    distinct() ->
    poll_options_tibble

  return(poll_options_tibble)
}

#' Extract Entities from Post
#'
#' @description
#' Processes the timeline data retrieved from the X API to extract entities
#' such as mentions, hashtags, cashtags, and URLs from tweets in the timeline.
#'
#' @importFrom purrr map_dfr pluck map_chr
#' @importFrom dplyr select distinct
#' @template timeline
#' @return A tibble containing extracted entities, including mentions, hashtags, cashtags, and URLs.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "Tesla",
#'   max_results = 100,
#'   bearer_token = Sys.getenv("X_BEARER_TOKEN")
#' )
#' entities <- get_post_entity(timeline)
#' }
#' @export
get_post_entity <- function(timeline) {

  # Validate input
  if (missing(timeline) || !is.list(timeline)) {
    stop("`timeline` must be a valid list containing timeline data.")
  }

  # Extract entities from the timeline response
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        tibble(
          tweet_id        = .x$id %||% NA_character_,
          mentions        = map_chr(.x$entities$mentions, ~ .x$username %||% NA_character_, .null = NA_character_),
          hashtags        = map_chr(.x$entities$hashtags, ~ .x$tag %||% NA_character_, .null = NA_character_),
          cashtags        = map_chr(.x$entities$cashtags, ~ .x$tag %||% NA_character_, .null = NA_character_),
          urls            = map_chr(.x$entities$urls, ~ .x$expanded_url %||% NA_character_, .null = NA_character_)
        )
      }
    ) |>
    distinct() ->
    entities_tibble

  return(entities_tibble)
}

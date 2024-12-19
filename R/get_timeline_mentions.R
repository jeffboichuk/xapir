#' Extract Mentions from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to extract mentions
#' from tweets in the timeline, including mentions from `note_tweet.entities`.
#'
#' @importFrom purrr map_dfr pluck map_chr
#' @importFrom dplyr select distinct
#' @template timeline
#' @return A tibble containing extracted mentions.
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(
#'   username = "Tesla",
#'   max_results = 100,
#'   bearer_token = Sys.getenv("X_BEARER_TOKEN")
#' )
#' mentions <- get_timeline_mentions(timeline)
#' }
#' @export
get_timeline_mentions <- function(timeline) {

  # Validate input
  if (missing(timeline) || !is.list(timeline)) {
    stop("`timeline` must be a valid list containing timeline data.")
  }

  # Extract mentions from the timeline response
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        tibble(
          tweet_id  = .x$id %||% NA_character_,
          mentions  = c(
            map_chr(.x$entities$mentions, ~ .x$username %||% NA_character_, .null = NA_character_),
            map_chr(.x$note_tweet$entities$mentions, ~ .x$username %||% NA_character_, .null = NA_character_)
          ) |> unique()
        )
      }
    ) |>
    distinct() ->
    mentions_tibble

  return(mentions_tibble)
}

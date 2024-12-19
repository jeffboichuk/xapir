#' Extract Context Annotations from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to extract context annotations
#' for all tweets in the timeline.
#'
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing the context annotations, including domain and entity details.
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr select distinct
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(
#'   username = "Tesla",
#'   max_results = 100,
#'   bearer_token = Sys.getenv("X_BEARER_TOKEN")
#' )
#' context_annotations <- extract_context_annotations(timeline)
#' }
#' @export
extract_context_annotations <- function(timeline) {

  # Validate input
  if (missing(timeline) || !is.list(timeline)) {
    stop("`timeline` must be a valid list containing timeline data.")
  }

  # Extract context_annotations from the timeline response
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        if (!is.null(.x$context_annotations)) {
          tibble(
            tweet_id           = .x$id,
            domain_id          = map_chr(.x$context_annotations, ~ .x$domain$id %||% NA_character_),
            domain_name        = map_chr(.x$context_annotations, ~ .x$domain$name %||% NA_character_),
            domain_description = map_chr(.x$context_annotations, ~ .x$domain$description %||% NA_character_),
            entity_id          = map_chr(.x$context_annotations, ~ .x$entity$id %||% NA_character_),
            entity_name        = map_chr(.x$context_annotations, ~ .x$entity$name %||% NA_character_),
            entity_description = map_chr(.x$context_annotations, ~ .x$entity$description %||% NA_character_)
          )
        } else {
          NULL
        }
      }
    ) |>
    distinct() ->
    context_annotations_tibble

  return(context_annotations_tibble)
}

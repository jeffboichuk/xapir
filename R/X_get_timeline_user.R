#' Extract User Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to extract user metadata,
#' including profile details and public metrics.
#'
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured user data.
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr select distinct mutate any_of
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(
#'   username = "Tesla",
#'   max_results = 100,
#'   start_time = iso_8601(as_date("2024-10-10") - days(14)),
#'   end_time = iso_8601(as_date("2024-10-10") + days(14))
#' )
#' user <- x_get_timeline_user(timeline)
#' }
#' @export
x_get_timeline_user <- function(timeline) {

  # Extract user data directly
  timeline |>
    map(pluck("includes")) |>
    map(pluck("users")) |>
    unlist(recursive = FALSE) ->
    user_list

  # Define the variable order
  user_variable <- c(
    "created_at",
    "username",
    "name",
    "description",
    "followers_count",
    "following_count",
    "post_count",
    "listed_count",
    "like_count",
    "protected",
    "verified",
    "verified_type",
    "location",
    "profile_image_url",
    "link_in_bio",
    "user_id"
  )

  # Create the user tibble
  user_list |>
    map_dfr(
      ~ tibble(
        created_at        = .x$created_at,
        username          = .x$username,
        name              = .x$name,
        description       = .x$description %||% NA |> as.character(),
        followers_count   = .x$public_metrics$followers_count,
        following_count   = .x$public_metrics$following_count,
        post_count        = .x$public_metrics$tweet_count,
        listed_count      = .x$public_metrics$listed_count,
        like_count        = .x$public_metrics$like_count,
        protected         = .x$protected,
        verified          = .x$verified,
        verified_type     = .x$verified_type,
        location          = .x$location %||% NA |> as.character(),
        profile_image_url = .x$profile_image_url,
        link_in_bio       = .x$entities$url$urls |>  
                              pluck(1, "display_url", .default = NA) |> 
                              as.character(),
        user_id           = .x$id
      )
    ) |>
    mutate(created_at = ymd_hms(created_at)) |>
    distinct(user_id, .keep_all = TRUE) |>
    select(any_of(user_variable)) ->
    user

  return(user)
}

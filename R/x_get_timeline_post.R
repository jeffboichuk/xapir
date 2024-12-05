#' Extract Post Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle post data,
#' including metadata such as likes, retweets, replies, and impressions.
#'
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param include_referenced_posts Logical. Whether to include referenced posts in the output. Defaults to TRUE.
#' @return A tibble containing structured post data.
#' @importFrom purrr map map_dfr map_chr pluck
#' @importFrom dplyr mutate select any_of arrange distinct
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(
#'   username = "Tesla",
#'   max_results = 100,
#'   start_time = iso_8601(as_date("2024-10-10") - days(14)),
#'   end_time = iso_8601(as_date("2024-10-10") + days(14))
#' )
#' post <- x_get_timeline_post(timeline)
#' }
#' @export
x_get_timeline_post <- function(
    timeline,
    include_referenced_posts = TRUE
) {

  # Check if the timeline has multiple pages
  timeline_has_multiple_pages <- length(timeline) > 1

  # Determine if referenced posts exist
  if (timeline_has_multiple_pages) {
    map_lgl(
      timeline,
      ~ "includes" %in% names(.x) && "tweets" %in% names(.x$includes)
    ) |>
      any() ->
      timeline_references_posts
  } else {
    "includes" %in% names(timeline) && "tweets" %in% names(timeline$includes) ->
      timeline_references_posts
  }

  # Process timeline data
  if (timeline_has_multiple_pages) {
    timeline |>
      map(pluck("data")) |>
      unlist(recursive = FALSE) ->
      post_list

    if (timeline_references_posts && include_referenced_posts) {
      timeline |>
        map(pluck("includes")) |>
        map(pluck("tweets")) |>
        unlist(recursive = FALSE) ->
        post_list_referenced
    } else {
      post_list_referenced <- NULL
    }
  } else {
    timeline[[1]] |>
      pluck("data") ->
      post_list

    if (timeline_references_posts && include_referenced_posts) {
      timeline[[1]] |>
        pluck("includes", "tweets") ->
        post_list_referenced
    } else {
      post_list_referenced <- NULL
    }
  }

  # Combine post data
  post_list_all <- c(post_list, post_list_referenced)

  # Create the post tibble
  post_list_all |>
    map_dfr(
      ~ tibble(
        created_at          = .x$created_at,
        text                = .x$text,
        impression_count    = .x$public_metrics$impression_count,
        like_count          = .x$public_metrics$like_count,
        repost_count        = .x$public_metrics$retweet_count,
        quote_count         = .x$public_metrics$quote_count,
        reply_count         = .x$public_metrics$reply_count,
        bookmark_count      = .x$public_metrics$bookmark_count,
        reply_settings      = .x$reply_settings,
        referenced_posts    = .x$referenced_tweets,
        in_reply_to_user_id = .x$in_reply_to_user_id,
        user_id             = .x$author_id,
        conversation_id     = .x$conversation_id,
        post_id             = .x$id
      )
    ) ->
    post

  # Finalize the post data
  post_variable <- c(
    "created_at",
    "text",
    "impression_count",
    "like_count",
    "repost_count",
    "quote_count",
    "reply_count",
    "bookmark_count",
    "reply_settings",
    "reposted",
    "quoted",
    "replied_to",
    "in_reply_to_user_id",
    "user_id",
    "conversation_id",
    "post_id"
  )

  if ("referenced_posts" %in% colnames(post)) {
    post |>
      mutate(
        created_at = ymd_hms(created_at),
        ref_type   = map_chr(referenced_posts, ~ .x$type %||% NA_character_),
        ref_id     = map_chr(referenced_posts, ~ .x$id %||% NA_character_),
        reposted   = ifelse(ref_type == "retweeted", ref_id, NA_character_),
        quoted     = ifelse(ref_type == "quoted", ref_id, NA_character_),
        replied_to = ifelse(ref_type == "replied_to", ref_id, NA_character_)
      ) |>
      select(any_of(post_variable)) ->
      post
  } else {
    post |>
      mutate(created_at = ymd_hms(created_at)) |>
      select(any_of(post_variable)) ->
      post
  }

  return(post)
}

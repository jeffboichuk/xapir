#' Extract Post Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle post data,
#' including metadata such as likes, retweets, replies, and impressions.
#'
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param include_referenced_posts Logical. Whether to include referenced posts in the output. Defaults to TRUE.
#' @return A tibble containing structured post data.
#' @importFrom purrr map map_dfr map_chr pluck map_lgl
#' @importFrom dplyr mutate select any_of arrange distinct
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post <- x_get_timeline_post(timeline)
#' }
#' @export
x_get_timeline_post <- function(
    timeline,
    include_referenced_posts = TRUE
) {

  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) ->
    post_list

  map_lgl(
    timeline,
    ~ "includes" %in% names(.x) && "tweets" %in% names(.x$includes)
  ) |>
    any() ->
    timeline_has_referenced_posts

  if (timeline_has_referenced_posts && include_referenced_posts) {
    timeline |>
      map(pluck("includes")) |>
      map(pluck("tweets")) |>
      unlist(recursive = FALSE) ->
      post_list_referenced
  } else {
    post_list_referenced <- NULL
  }

  # Combine post data
  post_list_all <- c(post_list, post_list_referenced)

  post_schema <- tibble(
    created_at          = NA_POSIXct_,
    text                = NA_character_,
    impression_count    = NA_integer_,
    like_count          = NA_integer_,
    repost_count        = NA_integer_,
    quote_count         = NA_integer_,
    reply_count         = NA_integer_,
    bookmark_count      = NA_integer_,
    reply_settings      = NA_character_,
    referenced_posts    = list(NULL),
    in_reply_to_user_id = NA_character_,
    user_id             = NA_character_,
    conversation_id     = NA_character_,
    post_id             = NA_character_
  )

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

  post |>
    add_column(
      !!!post_schema[setdiff(names(post_schema), names(post))]
    ) |>
    mutate(created_at = ymd_hms(created_at)) |>
    mutate(
      ref_type = map_chr(referenced_posts, ~ .x$type %||% NA_character_),
      ref_id   = map_chr(referenced_posts, ~ .x$id %||% NA_character_)
    ) |>
    mutate(
      reposted   = ifelse(ref_type == "retweeted", ref_id, NA_character_),
      .after = reply_settings
    ) |>
    mutate(
      reposted   = ifelse(ref_type == "retweeted", ref_id, NA_character_),
      quoted     = ifelse(ref_type == "quoted", ref_id, NA_character_),
      replied_to = ifelse(ref_type == "replied_to", ref_id, NA_character_),
      .after = reply_settings
    ) |>
    select(all_of(post_variable)) ->
    post

  return(post)
}

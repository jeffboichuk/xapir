#' Extract Post Data
#'
#' @description
#' Extract a user's posts and referenced posts from a timeline object created
#' using `x_get_timeline()`.
#'
#' @importFrom purrr map pluck flatten map_dfr map_chr
#' @importFrom tidyr tibble
#' @importFrom dplyr mutate select arrange desc
#' @importFrom lubridate ymd_hms
#' @importFrom tidyselect any_of
#' @param timeline A user's timeline.
#' @param include_referenced_posts TRUE if you want referenced posts, too.
#'
#' @return A \code{tibble} containing information about a post.
#' @examples
#' \dontrun{
#' tl <- x_read_timeline_post(timeline)
#' }
#' @export
x_extract_post <- function(timeline, include_referenced_posts = TRUE) {

  # Extract the user's posts from a timeline object stemming from
  # x_get_timeline()
  timeline |>
    map(~ pluck(.x, "data")) |>
    flatten() ->
    post_list

  # If include_referenced_posts is TRUE, extract the referenced posts from a
  # timeline object stemming from x_get_timeline()
  if (include_referenced_posts) {

  timeline |>
    map(~ pluck(.x, "includes", "tweets")) |>
    flatten() ->
    related_post_list

  post_list <- c(post_list, related_post_list)

  }

  # Map the posts in to a tibble
  post_list |>
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

  # Create a character vector for selecting and ordering the variables
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

  # Wrangle the referenced_posts list column into separate columns
  # TODO: add flexibility for when referenced_posts doesn't exist in post.
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

  # Arrange the posts between and within conversations in reverse chronological
  # order
  post |>
    arrange(desc(conversation_id), desc(post_id)) ->
    post

  return(post)

}

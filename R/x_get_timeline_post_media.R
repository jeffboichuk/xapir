#' Extract Post Media from Timeline Data
#'
#' @description
#' This function processes the timeline data retrieved from the X API to extract
#' post media details, including metadata about images and videos attached to posts.
#'
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param include_referenced_posts Logical. Whether to include referenced posts in the output. Defaults to TRUE.
#' @return A tibble with post and media details, including post IDs, media type,
#'   view count, dimensions, and URLs for images and videos.
#' @importFrom purrr map map_dfr map_lgl pluck
#' @importFrom dplyr left_join mutate select rename distinct arrange relocate
#' @importFrom tidyr unnest_wider unnest
#' @examples
#' \dontrun{
#' timeline <- x_get_timeline(username = "Tesla", max_results = 100)
#' post_media <- x_get_timeline_post_media(timeline)
#' }
#' @export
x_get_timeline_post_media <- function(
    timeline,
    include_referenced_posts = TRUE
  ) {

  # Determine whether the data includes multiple pages and whether the user
  # referenced other posts
  timeline_has_multiple_pages <- length(timeline) > 1

  # Does the timeline reference posts?
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

  if (timeline_has_multiple_pages) {
    timeline |>
      map(pluck("includes")) |>
      map(pluck("media")) |>
      unlist(recursive = FALSE) ->
      post_media_list
  } else {
    timeline[[1]] |>
      pluck("includes") |>
      pluck("media") ->
      post_media_list
  }

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

  post_list_all <- (c(post_list, post_list_referenced))

  post_list_all |>
    map_dfr(
      ~ tibble(
        post_id       = .x$id,
        attachment_id = .x$attachments
      )
    ) |>
    unnest(attachment_id) |>
    unnest(attachment_id) |>
    distinct(post_id, attachment_id) ->
    post_attachment_crosswalk

  post_media_list |>
    map_dfr(
      ~ tibble(
        media_id       = .x$media_key,
        height         = .x$height,
        width          = .x$width,
        type           = .x$type,
        variants       = .x$variants,
        duration_ms    = .x$duration_ms,
        public_metrics = .x$public_metrics,
        url_photo      = .x$url,
        url_preview    = .x$preview_image_url
      )
    ) |>
    unnest_wider(variants) |>
    rename(url_video = url) |>
    mutate(url_image = ifelse(is.na(url_preview), url_photo, url_preview)) |>
    select(-url_photo) |>
    unnest(public_metrics, keep_empty = TRUE) |>
    rename(view_count = public_metrics) |>
    arrange(media_id, desc(bit_rate)) |>
    distinct(media_id, .keep_all = TRUE) |>
    select(
      media_id,
      type,
      view_count,
      height,
      width,
      duration_ms,
      bit_rate,
      url_image,
      url_video
    ) ->
    media

  media |>
    left_join(
      post_attachment_crosswalk,
      by = join_by(media_id == attachment_id)
    ) |>
    relocate(post_id, .before = media_id) |>
    arrange(post_id, media_id) ->
    post_media

  return(post_media)
}

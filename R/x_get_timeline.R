#' Get User Timeline
#'
#' @description
#' Ping the timeline endpoint. Add more details here.
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_path_append req_perform resp_body_json req_url_query
#' @importFrom purrr pluck
#' @importFrom stringr str_c
#' @template username
#' @template max_results
#' @template pagination_token
#' @template pagination_token
#' @return \code{list} that contains 4 elements that make up the API response
#' @examples
#' \dontrun{
#' tl <- x_get_timeline("Tesla")
#' }
#' @export
x_get_timeline <- function(
    username,
    max_results      = 10,
    end_time         = NULL,
    start_time       = NULL,
    until_id         = NULL,
    since_id         = NULL,
    exclude          = NULL,
    pagination_token = NULL,
    sleep_time       = 0,
    token            = Sys.getenv("X_BEARER_TOKEN"),
    post_fields      =
      c("created_at", "text", "public_metrics", "geo", "attachments",
        "context_annotations", "entities", "lang", "referenced_tweets",
        "reply_settings", "conversation_id", "in_reply_to_user_id", "author_id",
        "edit_history_tweet_ids", "id"),
    user_fields      =
      c("created_at", "description", "protected", "entities", "location",
        "profile_image_url", "public_metrics", "verified", "verified_type"),
    media_fields     =
      c("duration_ms", "height", "width", "preview_image_url", "type", "url",
        "public_metrics", "variants", "media_key"),
    poll_fields      =
      c("end_datetime", "duration_minutes", "options", "voting_status", "id"),
    place_fields     =
      c("contained_within", "country", "country_code", "full_name", "geo", "id",
        "name", "place_type"),
    expansions       =
      c("author_id", "entities.mentions.username",
        "referenced_tweets.id.author_id", "referenced_tweets.id",
        "in_reply_to_user_id", "attachments.media_keys", "attachments.poll_ids")
) {

  response <- NULL

  # Get the user_id for the specified username
  request(base_url = "https://api.x.com/2") |>
    req_url_path_append(endpoint = paste0("users/by/username/", username)) |>
    req_auth_bearer_token(token = token) |>
    req_perform() |>
    resp_body_json() |>
    pluck("data", "id") ->
    user_id

  # Join the fields with commas as the API expects
  post_fields_str  <- str_c(post_fields, collapse = ",")
  user_fields_str  <- str_c(user_fields, collapse = ",")
  media_fields_str <- str_c(media_fields, collapse = ",")
  poll_fields_str  <- str_c(poll_fields, collapse = ",")
  place_fields_str <- str_c(place_fields, collapse = ",")
  expansions_str   <- str_c(expansions, collapse = ",")

  call_i <- 1

  # Make the API request
  while (call_i == 1 | !is.null(pagination_token)) {

    request(base_url = "https://api.x.com/2") |>
      req_url_path_append(endpoint = paste0("users/", user_id, "/tweets")) |>
      req_url_query(
        max_results      = max_results,
        end_time         = end_time,
        start_time       = start_time,
        until_id         = until_id,
        since_id         = since_id,
        exclude          = exclude,
        pagination_token = pagination_token,
        tweet.fields     = post_fields_str,
        user.fields      = user_fields_str,
        media.fields     = media_fields_str,
        poll.fields      = poll_fields_str,
        place.fields     = place_fields_str,
        expansions       = expansions_str
      ) |>
      req_auth_bearer_token(token = Sys.getenv("X_BEARER_TOKEN")) |>
      req_perform() |>
      resp_body_json() ->
      this_response

    response <- c(response, list(this_response))

    this_response |>
      pluck("meta", "next_token") ->
      pagination_token

    message(paste("Finished getting posts on page ", call_i))

    call_i <- call_i + 1

    # Sleep time between API requests
    Sys.sleep(sleep_time)

  }

  # Check if the pagination token is not null
  # if not null, then capture and bind together.
  # results2 <- x_get_timeline(pagination_token = XXXXX)

  # Return the response
  return(response)
}

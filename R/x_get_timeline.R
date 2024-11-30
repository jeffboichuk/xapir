#' Get User Timeline
#'
#' @description
#' Ping the timeline endpoint.  The X API only allows fetching up to the most
#' recent 3,200 posts.
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_path_append req_perform resp_body_json req_url_query
#' @importFrom purrr pluck
#' @importFrom stringr str_c
#' @template username
#' @template max_results
#' @param end_time The latest date-time from which you want to get posts.
#'   Provide the value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
#'   `iso_8601()` function will convert a string, date, or date-time object to
#'   the required format (e.g., `iso_8601("2024-10-10")`).
#' @param start_time The earliest date-time from which you want to get posts.
#' @param until_id A post ID to limit the results to posts older than the
#'   specified ID.
#' @param since_id A post ID to limit the results to posts more recent than the
#'   specified ID.
#' @template pagination_token
#' @param exclude A comma-separated list of the types of posts to exclude from
#'   the response (e.g., "retweets", "replies", or "retweets,replies"). #'
#'   @param sleep_time A numeric value specifying the number of seconds to wait
#'   between API calls. This helps avoid hitting rate limits imposed by the X
#'   API. You can adjust this value based on your tier's rate limits, which are
#'   detailed on the [X API documentation
#'   website](https://developer.x.com/en/docs/rate-limits).
#' @template bearer_token
#' @template post_fields
#' @template user_fields
#' @return A \code{list} containing the four elements that make up the API
#'   response
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
    pagination_token = NULL,
    exclude          = NULL,
    sleep_time       = 0,
    bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
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
  while (TRUE) {
    tryCatch(
      expr = {
        request(base_url = "https://api.x.com/2") |>
          req_url_path_append(
            endpoint = paste0("users/by/username/", username)
          ) |>
          req_auth_bearer_token(token = bearer_token) |>
          req_perform() |>
          resp_body_json() |>
          pluck("data", "id") ->
          user_id
        # Exit the loop if successful
        break
      },
      error = function(e) {
        message(e$message, " Retrying in 60 seconds.")
        Sys.sleep(60)
      }
    )
  }

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

    while (TRUE) {
      tryCatch(
        expr = {
          request(base_url = "https://api.x.com/2") |>
            req_url_path_append(
              endpoint = paste0("users/", user_id, "/tweets")
            ) |>
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
            req_auth_bearer_token(token = bearer_token) |>
            req_perform() |>
            resp_body_json() ->
            this_response

          # Exit the loop if successful
          break
        },
        error = function(e) {
          message(e$message, " Retrying in 60 seconds.")
          Sys.sleep(60)
        }
      )
    }

    response <- c(response, list(this_response))

    this_response |>
      pluck("meta", "next_token") ->
      pagination_token

    message(paste("Finished getting posts on page ", call_i))

    call_i <- call_i + 1

    # Sleep time between API requests
    Sys.sleep(sleep_time)

  }

  # Return the response
  return(response)
}

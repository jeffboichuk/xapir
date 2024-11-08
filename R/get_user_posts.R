get_user_posts <- function(
    username,
    max_results      = 10,
    pagination_token = NULL,
    token            = Sys.getenv("X_BEARER_TOKEN"),
    tweet_fields =
      c("created_at", "text", "public_metrics", "geo", "attachments",
        "context_annotations", "entities", "lang", "referenced_tweets",
        "reply_settings", "conversation_id", "in_reply_to_user_id", "author_id",
        "edit_history_tweet_ids", "id"),
    user_fields =
      c("created_at", "description", "protected", "entities", "location",
        "profile_image_url", "public_metrics", "verified", "verified_type"),
    media_fields =
      c("duration_ms", "height", "width", "preview_image_url", "type", "url",
        "public_metrics", "variants", "media_key"),
    poll_fields =
      c("end_datetime", "duration_minutes", "options", "voting_status", "id"),
    expansions = c("author_id", "entities.mentions.username",
                   "referenced_tweets.id.author_id", "referenced_tweets.id",
                   "in_reply_to_user_id", "attachments.media_keys", "attachments.poll_ids")
) {

  request(base_url = "https://api.x.com/2") |>
    req_url_path_append(endpoint = paste0("users/by/username/", username)) |>
    req_auth_bearer_token(token = token) |>
    req_perform() |>
    resp_body_json() |>
    pluck("data", "id") ->
    user_id

  # Join the fields with commas as the API expects
  tweet_fields_str <- str_c(tweet_fields, collapse = ",")
  user_fields_str  <- str_c(user_fields, collapse = ",")
  media_fields_str <- str_c(media_fields, collapse = ",")
  poll_fields_str  <- str_c(poll_fields, collapse = ",")
  expansions_str   <- str_c(expansions, collapse = ",")

  # Make the API request
  response <- request(base_url = "https://api.x.com/2") |>
    req_url_path_append(endpoint = paste0("users/", user_id, "/tweets")) |>
    req_url_query(
      tweet.fields     = tweet_fields_str,
      user.fields      = user_fields_str,
      media.fields     = media_fields_str,
      poll.fields      = poll_fields_str,
      expansions       = expansions_str,
      max_results      = max_results,
      pagination_token = pagination_token
    ) |>
    req_auth_bearer_token(token = Sys.getenv("X_BEARER_TOKEN")) |>
    req_perform()

  # Return the response
  return(response)
}

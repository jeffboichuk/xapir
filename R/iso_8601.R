#' Convert a Date or Date-Time to ISO 8601 UTC Format
#'
#' @description
#' This helper function takes a date or date-time input and converts it to
#' the required ISO 8601 format (`YYYY-MM-DDTHH:mm:ssZ`) in UTC. Users can
#' supply a date, a POSIXct object, or a string. If no time zone is specified,
#' the system's default timezone is assumed.
#'
#' @importFrom lubridate as_datetime with_tz
#' @param x  A string (e.g., "2024-07-01" or "2024-07-01 12:30:00") or a date or
#'   date-time object (Date, POSIXct).
#' @param tz A string specifying the input timezone (e.g., "America/New_York").
#'   Defaults to `Sys.timezone()`, which uses the system's timezone if not
#'   already present in `x`.
#' @return A string in ISO 8601 UTC format.
#' @examples
#' \dontrun{
#' iso_8601("2024-07-01")
#' iso_8601("2024-07-01 15:00:00", tz = "America/New_York")
#' iso_8601(Sys.time())
#' }
#' @export
iso_8601 <- function(x, tz = Sys.timezone()) {

  # Parse the date or date-time input
  x |>
    as_datetime(tz = tz) |>
    with_tz(tzone = "UTC") |>
    format("%Y-%m-%dT%H:%M:%SZ") ->
    parsed_x

  return(parsed_x)

}

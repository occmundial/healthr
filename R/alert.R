#' @title Odbc
#'
#' @description
#' Creates an Odbc class
#'
#' @export
#'
Alert <- R6::R6Class(
  classname = "Alert",
  public = list(
    send = function(text,
                    url = Sys.getenv("SLACK_URL"),
                    token = Sys.getenv("SLACK_TOKEN"),
                    channel = Sys.getenv("SLACK_CHANNEL"),
                    username = Sys.getenv("SLACK_USERNAME"),
                    emoji = Sys.getenv("SLACK_EMOJI", ":bird:")) {
      checkmate::assertString(url)
      checkmate::assertString(token)
      checkmate::assertString(channel)
      checkmate::assertString(username)
      body <- list(channel = channel, username = username, text = text, icon_emoji = emoji)
      httr::POST(url = url, httr::content_type_json(),
                 httr::add_headers(Authorization = token),
                 body = yyjsonr::write_json_str(body, opts = list(auto_unbox = TRUE)))
    }
  ),
  private = list(
    .message = NULL
  )
)

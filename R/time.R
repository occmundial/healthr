#' @title Time
#'
#' @description
#' Creates a Time class
#'
#' @export
#'
Time <- R6::R6Class(
  classname = "Time",
  public = list(
    initialize = function(add = 6L * 60L * 60L) {
      private$.now <- Sys.time() - add
      private$.hour <- as.integer(format(private$.now, "%H"))
      minute <- as.integer(format(private$.now, "%M"))
      private$.minute <- list(
        hour = minute,
        day = 60L * private$.hour + minute
      )
    }
  ),
  active = list(
    now = function() private$.now,
    hour = function() private$.hour,
    minute = function() private$.minute
  ),
  private = list(
    .now = NULL,
    .hour = NULL,
    .minute = NULL
  )
)

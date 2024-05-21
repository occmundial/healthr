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
      checkmate::assertInt(add, lower = 0L)
      private$.now <- Sys.time() - add
      private$.hour <- as.integer(format(private$.now, "%H"))
      minute <- as.integer(format(private$.now, "%M"))
      private$.minute <- list(
        hour = minute,
        day = 60L * private$.hour + minute
      )
      invisible(self)
    },
    sequence = function(by = 1L, horizont = 1440L) {
      checkmate::assertInt(by, lower = 1L, upper = 60L)
      checkmate::assertInt(horizont / by, lower = 1L)
      date <- as.POSIXct(paste(format(private$.now, "%Y-%m-%d"), "00:00"))
      private$.serie <- seq.POSIXt(date, by = paste(by, "min"), length.out = horizont / by)
      invisible(self)
    }
  ),
  active = list(
    hour = function() private$.hour,
    minute = function() private$.minute,
    now = function() private$.now,
    serie = function() private$.serie
  ),
  private = list(
    .hour = NULL,
    .minute = NULL,
    .now = NULL,
    .serie = NULL
  )
)

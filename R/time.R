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
    minute = function() 60L * as.integer(format(private$.now, "%H")) + as.integer(format(private$.now, "%M")),
    now = function() format(private$.now, "%Y-%m-%d_%H:%M"),
    serie = function() private$.serie
  ),
  private = list(
    .now = NULL,
    .serie = NULL
  )
)

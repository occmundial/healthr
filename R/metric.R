#' @title Metric
#'
#' @description
#' Creates a Metric class
#'
#' @export
#'
Metric <- R6::R6Class(
  classname = "Metric",
  public = list(
    initialize = function(name) {
      private$.name <- paste("Metric", name)
      invisible(self)
    },
    count = function(dt, validator, sequence) {
      checkmate::assertDataTable(dt)
      checkmate::assertDataTable(validator)
      checkmate::assertInteger(sequence, len = nrow(validator))
      dt[, date := as.POSIXct(date)]
      data.table::setorder(dt, date)
      dt[, day := .GRP - 1L, by = .(data.table::yday(date))]
      dt[, period := 1440L * day + 60L * data.table::hour(date) + data.table::minute(date)]
      dt[, day := NULL]
      validator <- data.table::copy(validator)
      validator[dt, count := i.count, on = .(period)]
      private$.dt <- validator[, .(count = sum(count)), by = .(period = sequence)]
      data.table::setkey(private$.dt, period)
      invisible(self)
    }
  ),
  active = list(
    dt = function() private$.dt,
    name = function() private$.name
  ),
  private = list(
    .dt = NULL,
    .id = NULL,
    .name = NULL
  )
)

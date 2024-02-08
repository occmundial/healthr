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
      private$.name <- name
      invisible(self)
    },
    count = function(odbc, parameter) {
      dt <- odbc$consult(parameter$query)
      data.table::setDT(dt)
      dt[, date := as.POSIXct(date)]
      data.table::setorder(dt, date)
      dt[, day := .GRP - 1L, by = .(data.table::yday(date))]
      dt[, period := 1440L * day + 60L * data.table::hour(date) + data.table::minute(date)]
      dt[, day := NULL]
      validator <- data.table::copy(parameter$validator)
      validator[dt, count := i.count, on = .(period)]
      private$.values <- validator[, .(count = sum(count)), by = .(period = parameter$sequence)]
      data.table::setkey(private$.values, period)
      invisible(self)
    },
    save = function(redis) {
      redis$set(private$.name, yyjsonr::write_json_str(private$.values$count))
    }
  ),
  active = list(
    values = function() private$.values
  ),
  private = list(
    .name = NULL,
    .values = NULL
  )
)

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
      checkmate::assertR6(odbc, classes = "Odbc")
      checkmate::assertR6(parameter, classes = "Parameter")
      private$.parameter <- parameter
      dt <- odbc$consult(parameter$query)
      data.table::setDT(dt)
      dt[, date := as.POSIXct(date)]
      data.table::setorder(dt, date)
      dt[, day := .GRP - 1L, by = .(data.table::yday(date))]
      dt[, period := 1440L * day + 60L * data.table::hour(date) + data.table::minute(date)]
      dt[, day := NULL]
      private$.values <- data.table::copy(parameter$validator)
      private$.values[dt, count := i.count, on = .(period)]
      private$.values[, .(count = sum(count)), by = .(period = parameter$sequence)]
      data.table::setkey(private$.values, period)
      invisible(self)
    },
    save = function(redis) {
      checkmate::assertR6(redis, classes = "Redis")
      redis$set(private$.name, yyjsonr::write_json_str(private$.values$count))
      invisible(self)
    },
    read = function(redis, parameter) {
      checkmate::assertR6(redis, classes = "Redis")
      checkmate::assertR6(parameter, classes = "Parameter")
      json <- redis$get(private$.name)
      dt <- data.table::data.table(count = yyjsonr::read_json_str(json))
      private$.values <- dt[, .(count = sum(count)), by = .(period = parameter$sequence)]
      invisible(self)
    }
  ),
  active = list(
    parameter = function() private$.parameter,
    values = function() private$.values
  ),
  private = list(
    .name = NULL,
    .parameter = NULL,
    .values = NULL
  )
)

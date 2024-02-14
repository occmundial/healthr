#' @title Metric
#'
#' @description
#' Creates a Metric class
#'
#' @export
#'
Metric <- R6::R6Class(
  classname = "Metric",
  inherit = Parameter,
  public = list(
    initialize = function(name) {
      private$.name <- name
      invisible(self)
    },
    count = function(odbc) {
      checkmate::assertR6(odbc, classes = "Odbc")
      dt <- odbc$consult(private$.params$query)
      data.table::setDT(dt)
      dt[, date := as.POSIXct(date)]
      data.table::setorder(dt, date)
      dt[, day := .GRP - 1L, by = .(data.table::yday(date))]
      dt[, period := 1440L * day + 60L * data.table::hour(date) + data.table::minute(date)]
      dt[, day := NULL]
      validator <- data.table::copy(private$.params$validator)
      validator[dt, count := i.count, on = .(period)]
      private$.dt <- validator[, .(count = sum(count)), by = .(period = private$.params$sequence)]
      data.table::setkey(private$.dt, period)
      invisible(self)
    },
    save = function(redis) {
      checkmate::assertR6(redis, classes = "Redis")
      redis$set(private$.name, yyjsonr::write_json_str(private$.dt$count))
      invisible(self)
    },
    read = function(redis) {
      checkmate::assertR6(redis, classes = "Redis")
      json <- redis$get(private$.name)
      dt <- data.table::data.table(count = yyjsonr::read_json_str(json))
      private$.dt <- dt[, .(count = sum(count)), by = .(period = private$.params$sequence)]
      invisible(self)
    }
  ),
  active = list(
    dt = function() private$.dt
  ),
  private = list(
    .name = NULL,
    .dt = NULL
  )
)

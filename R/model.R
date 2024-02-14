#' @title Model
#'
#' @description
#' Creates a Model class
#'
#' @export
#'
Model <- R6::R6Class(
  classname = "Model",
  public = list(
    initialize = function(name) {
      private$.name <- name
      invisible(self)
    },
    normalize = function(metric) {
      checkmate::assertR6(metric, classes = "Metric")
      private$.parameter <- metric$parameter
      dt <- data.table::copy(metric$values)
      dt[, group := period %% private$.parameter$period]
      index <- dt[, .I[which.min(count)], by = .(group)]$V1
      dt[index, count := NA_integer_]
      dt_stats <- dt[, .(count = round(quantile(count, probs = 0.25, na.rm = TRUE))), by = .(group)]
      dt[is.na(count), count := dt_stats[.SD, count, on = .(group)]]
      index <- dt[, .I[which.max(count)], by = .(group)]$V1
      dt[index, count := NA_integer_]
      dt_stats <- dt[, .(count = round(quantile(count, probs = 0.75, na.rm = TRUE))), by = .(group)]
      dt[is.na(count), count := dt_stats[.SD, count, on = .(group)]]
      # dt[, group := NULL]
      private$.serie <- stats::ts(dt$count, start = c(1L, 1L), frequency = private$.parameter$period)
      invisible(self)
    },
    train = function() {
      fourier <- forecast::fourier(private$.serie, K = 4)
      private$.model <- forecast::tslm(private$.serie ~ fourier)
      invisible(self)
    },
    predict = function(level = 99L) {
      checkmate::assertInt(level, lower = 80L, upper = 99L)
      fourier <- data.frame(values = forecast::fourier(private$.serie, K = 4, h = private$.parameter$period))
      linear <- forecast::forecast(private$.model, newdata = fourier, level = level)
      private$.prediction <- lapply(c("lower", "mean", "upper"), function(x) {
        values <- as.integer(linear[[x]])
        values[values < 0] <- 0L
        values
      })
      invisible(self)
    },
    save = function(redis) {
      checkmate::assertR6(redis, classes = "Redis")
      redis$set(private$.name, yyjsonr::write_json_str(private$.prediction))
      invisible(self)
    },
    read = function(redis) {
      checkmate::assertR6(redis, classes = "Redis")
      json <- redis$get(private$.name)
      yyjsonr::read_json_str(json)
      invisible(self)
    }
  ),
  active = list(
    serie = function() private$.serie,
    model = function() private$.model,
    parameter = function() private$.parameter,
    prediction = function() private$.prediction
  ),
  private = list(
    .name = NULL,
    .serie = NULL,
    .model = NULL,
    .parameter = NULL,
    .prediction = NULL
  )
)

#' @title Model
#'
#' @description
#' Creates a Model class
#'
#' @export
#'
Model <- R6::R6Class(
  classname = "Model",
  inherit = Metric,
  public = list(
    initialize = function(name, query, by = 1L, horizont = 4L * 1440L) {
      private$.name <- paste("Model", name)
      self$set(query, by, horizont)
      invisible(self)
    },
    normalize = function() {
      if (checkmate::testNull(private$.dt)) stop("Count the values first")
      private$.dt[, group := period %% private$.params$period]
      index <- private$.dt[, .I[which.min(count)], by = .(group)]$V1
      private$.dt[index, count := NA_integer_]
      dt_stats <- private$.dt[, .(count = round(quantile(count, probs = 0.25, na.rm = TRUE))), by = .(group)]
      private$.dt[is.na(count), count := dt_stats[.SD, count, on = .(group)]]
      index <- private$.dt[, .I[which.max(count)], by = .(group)]$V1
      private$.dt[index, count := NA_integer_]
      dt_stats <- private$.dt[, .(count = round(quantile(count, probs = 0.75, na.rm = TRUE))), by = .(group)]
      private$.dt[is.na(count), count := dt_stats[.SD, count, on = .(group)]]
      private$.dt[, group := NULL]
      private$.serie <- stats::ts(private$.dt$count, start = c(1L, 1L), frequency = private$.params$period)
      invisible(self)
    },
    train = function() {
      if (checkmate::testNull(private$.serie)) stop("Normalize the values first")
      fourier <- forecast::fourier(private$.serie, K = 4)
      private$.model <- forecast::tslm(private$.serie ~ fourier)
      invisible(self)
    },
    predict = function(level = 99L) {
      checkmate::assertInt(level, lower = 80L, upper = 99L)
      if (checkmate::testNull(private$.serie)) stop("Normalize the values first")
      if (checkmate::testNull(private$.model)) stop("Train the model first")
      fourier <- data.frame(values = forecast::fourier(private$.serie, K = 4, h = private$.params$period))
      linear <- forecast::forecast(private$.model, newdata = fourier, level = level)
      private$.prediction <- sapply(c("lower", "mean", "upper"), function(x) {
        values <- as.integer(linear[[x]])
        values[values < 0] <- 0L
        values
      }, simplify = FALSE)
      invisible(self)
    },
    save = function(redis) {
      checkmate::assertR6(redis, classes = "Redis")
      if (checkmate::testNull(private$.prediction)) stop("Predict the values first")
      redis$set(private$.name, yyjsonr::write_json_str(private$.prediction))
      # invisible(self)
    },
    read = function(redis) {
      checkmate::assertR6(redis, classes = "Redis")
      json <- redis$get(private$.name)
      private$.prediction <- yyjsonr::read_json_str(json, opts = list(obj_of_arrs_to_df = FALSE))
      invisible(self)
    },
    pipeline = function(odbc, redis) {
      self$count(odbc)$normalize()$train()$predict()$save(redis)
    }
  ),
  active = list(
    serie = function() private$.serie,
    model = function() private$.model,
    prediction = function() private$.prediction
  ),
  private = list(
    .serie = NULL,
    .model = NULL,
    .prediction = NULL
  )
)

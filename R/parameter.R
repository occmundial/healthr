#' @title Parameter
#'
#' @description
#' Creates a Parameter class
#'
#' @export
#'
Parameter <- R6::R6Class(
  classname = "Parameter",
  public = list(
    initialize = function(query, by = 1L, horizont = 1440L) {
      self$set(query, by, horizont)
    },
    set = function(query, by = 1L, horizont = 1440L) {
      checkmate::assertInt(by, lower = 1L, upper = 60L)
      checkmate::assertInt(horizont / by, lower = 1L)
      private$.period <- 1440L / by
      private$.sequence <- rep(seq_len(horizont / by), each = by)
      if (!missing(query)) {
        checkmate::assertString(query)
        private$.query <- query
        private$.validator <- data.table::data.table(period = seq_len(horizont) - 1L, count = 0L)
      }
      invisible(self)
    }
  ),
  active = list(
    period = function() private$.period,
    query = function() private$.query,
    sequence = function() private$.sequence,
    validator = function() private$.validator
  ),
  private = list(
    .period = NULL,
    .query = NULL,
    .sequence = NULL,
    .validator = NULL
  )
)

#' @title Connection
#'
#' @description
#' Creates an Connection class
#'
#' @export
#'
Connection <- R6::R6Class(
  classname = "Connection",
  public = list(
    initialize = function(driver, ...) {
      self$open(driver, ...)
    },
    open = function(driver,
                    ...,
                    min_size = 1L,
                    max_size = 5L,
                    on_create = NULL,
                    idle_timeout = 5L * 60L,
                    validation_interval = 1L * 60L,
                    validate_query = NULL) {
      checkmate::assertList(list(...), types = "character", names = "named")
      private$.pool <- pool::dbPool(driver,
                                    ...,
                                    minSize = min_size,
                                    maxSize = max_size,
                                    onCreate = on_create,
                                    idleTimeout = idle_timeout,
                                    validationInterval = validation_interval,
                                    validateQuery = validate_query)
      invisible(self)
    },
    close = function() {
      pool::poolClose(private$.pool)
      invisible(self)
    },
    consult = function(query) {
      checkmate::assertString(query)
      private$.dt <- pool::dbGetQuery(private$.pool, query)
      data.table::setDT(private$.dt)
      invisible(self)
    }
  ),
  active = list(
    dt = function() private$.dt,
    pool = function() private$.pool
  ),
  private = list(
    .dt = NULL,
    .pool = NULL
  )
)

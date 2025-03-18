setClass("RedisConnection", slots = list(resource = "ANY"))

setMethod("onValidate", "RedisConnection", function(object) {
  message("[", format(Sys.time()), "] ", "Validating Redis connection", "\n")
  object@resource$PING()
})

#' @title Redis
#'
#' @description
#' Creates a Redis class
#'
#' @export
#'
Redis <- R6::R6Class(
  classname = "Redis",
  public = list(
    initialize = function(...) {
      self$open(...)
    },
    open = function(...,
                    min_size = 1L,
                    max_size = 5L,
                    idle_timeout = 5L * 60L,
                    validation_interval = 1L * 60L,
                    state = NULL) {
      checkmate::assertList(list(...), types = "character", names = "named")
      config <- redux::redis_config(...)
      factory <- function() {
        ok <- try(redux::hiredis(config), silent = TRUE)
        if (inherits(ok, "try-error")) {
          warning("REDIS connection failed")
          return(-1L)
        }
        new("RedisConnection", resource = ok)
      }
      private$.pool <- pool::poolCreate(factory,
                                        minSize = min_size,
                                        maxSize = max_size,
                                        idleTimeout = idle_timeout,
                                        validationInterval = validation_interval,
                                        state = state)
      invisible(self)
    },
    close = function() {
      pool::poolClose(private$.pool)
      invisible(self)
    },
    get = function(key, opts = list()) {
      if (identical(private$.pool, -1L)) stop("REDIS connection is not open")
      checkmate::assertList(opts, names = "named")
      conn <- pool::poolCheckout(private$.pool)
      on.exit(pool::poolReturn(conn))
      private$.json <- conn@resource$GET(key)
      private$.dt <- yyjsonr::read_json_str(private$.json, opts = opts)
      if (checkmate::testDataFrame(private$.dt)) data.table::setDT(private$.dt)
      invisible(self)
    },
    set = function(key, value, EX = NULL, PX = NULL, condition = NULL, opts = list()) {
      if (identical(private$.pool, -1L)) stop("REDIS connection is not open")
      checkmate::assertList(opts, names = "named")
      conn <- pool::poolCheckout(private$.pool)
      on.exit(pool::poolReturn(conn))
      conn@resource$SET(key, yyjsonr::write_json_str(value, opts = opts), EX, PX, condition)
      invisible(self)
    }
  ),
  active = list(
    dt = function() private$.dt,
    json = function() private$.json,
    pool = function() private$.pool
  ),
  private = list(
    .dt = NULL,
    .json = NULL,
    .pool = NULL
  )
)

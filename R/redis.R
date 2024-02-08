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
    initialize = function(host = Sys.getenv("REDIS_HOST"),
                          port = Sys.getenv("REDIS_PORT"),
                          db = Sys.getenv("REDIS_DB")) {
      self$connect(host, port, db)
    },
    connect = function(host, port, db) {
      config <- redux::redis_config(host = host, port = port, db = db)
      private$.connection <- redux::hiredis(config)
      invisible(self)
    },
    get = function(key) {
      private$.connection$GET(key)
    },
    set = function(key, value, EX = NULL, PX = NULL, condition = NULL) {
      private$.connection$SET(key, value, EX, PX, condition)
    }
  ),
  active = list(
    connection = function() private$.connection
  ),
  private = list(
    .connection = NULL
  )
)
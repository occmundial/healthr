#' @title Odbc
#'
#' @description
#' Creates an Odbc class
#'
#' @export
#'
Odbc <- R6::R6Class(
  classname = "Odbc",
  public = list(
    initialize = function(driver = "ODBC Driver 17 for SQL Server",
                          host = Sys.getenv("DB_HOST"),
                          name = Sys.getenv("DB_NAME"),
                          user = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD"),
                          extra = "ApplicationIntent=ReadOnly") {
      self$connect(driver, host, name, user, password, extra)
    },
    connect = function(driver = "ODBC Driver 17 for SQL Server",
                       host = Sys.getenv("DB_HOST"),
                       name = Sys.getenv("DB_NAME"),
                       user = Sys.getenv("DB_USER"),
                       password = Sys.getenv("DB_PASSWORD"),
                       extra = "ApplicationIntent=ReadOnly") {
      string <- sprintf("DRIVER={%s};server=%s;database=%s;uid=%s;pwd=%s;%s", driver, host, name, user, password, extra)
      private$.connection <- RODBC::odbcDriverConnect(string, readOnlyOptimize = TRUE)
      invisible(self)
    },
    disconnect = function() {
      close(private$.connection)
      invisible(self)
    },
    consult = function(query) {
      RODBC::sqlQuery(private$.connection, query)
    }
  ),
  active = list(
    connection = function() private$.connection
  ),
  private = list(
    .connection = NULL
  )
)

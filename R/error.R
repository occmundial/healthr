#' @title Error
#'
#' @description
#' Creates a Error class
#'
#' @export
#'
Error <- R6::R6Class(
  classname = "Error",
  public = list(
    check = function() {
      if (!checkmate::testNull(private$.error)) do.call(`return`, list(self), envir = parent.frame())
    }
  ),
  private = list(
    .error = NULL
  )
)

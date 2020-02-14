#' Add process bar to sapply.
#'
#' @param X same argument in sapply.
#' @param FUN same argument in sapply.
#' @return results same as sapply
#' @examples
#' sapply_pb(1:10, function(i) i)


#sapply function with processing bar
sapply_pb <- function(X, FUN, ...)
{
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)

  wrapper <- function(...){
    curVal <- get("counter", envir = env)
    assign("counter", curVal +1 ,envir=env)
    setTxtProgressBar(get("pb", envir=env), curVal +1)
    FUN(...)
  }
  res <- sapply(X, wrapper, ...)
  close(pb)
  res
}

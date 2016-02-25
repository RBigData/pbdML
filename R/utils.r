is.int <- function(x)
{
  if (is.numeric(x))
  {
    if (x-as.integer(x) == 0)
      return( TRUE )
    else
      return( FALSE )
  }
  else
    return( FALSE )
}



check_mvdf <- function(x)
{
  if (is.data.frame(x))
  {
    expr <- substitute(x <- as.matrix(x))
    eval(expr, parent.frame())
  }
  else if (is.numeric(x) && !is.matrix(x))
    stop("input must be a matrix, vector, or dataframe")
}



check_badvals <- function(x)
{
  if (!is.double(x))
    storage.mode(x) <- "double"
  
  .Call(R_check_badvals, x)
}



check_groupvar <- function(g)
{
  g <- submatrix(g)
  
  if (!is.integer(g))
    storage.mode(g) <- "integer"
  
  .Call(R_check_groupvar, g)
}



all.sametype <- function(...)
{
  l <- list(...)
  if (length(l) == 1)
    return(TRUE)
  
  c <- class(l[[1]])
  
  for (i in 2:length(l))
  {
    if (!identical(c, l[[i]]))
      return(FALSE)
  }
  
  return(TRUE)
}


result2df <- function(result) {

  variables <- result$head$vars
  bindings <- result$results$bindings

  if (length(bindings) == 0) {
    return(data.frame())
  }

  getcol <- function(var, bindings) {
    return(sapply(bindings, function(x) {
      if (!is.null(x[[var]])) {
        return(x[[var]]$value)
      } else {
        return('')
      }
    }))
  }

  cols <- lapply(variables, function(var) {
    return(getcol(var, bindings))
  })
  df <- data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- variables

  return(df)
}

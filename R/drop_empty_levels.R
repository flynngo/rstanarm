# Drop columns with empty interaction levels from model.matrix
drop_empty_levels <- function(x) {
  sel <- apply(x, 2L, function(x) !all(x == 1) && length(unique(x)) < 2)
  if (any(sel)) {
    # drop any column of x with < 2 unique values (empty interaction levels)
    # exception is column of 1s isn't dropped 
    warning("Dropped empty interaction levels: ",
            paste(colnames(x)[sel], collapse = ", "))
    x <- x[, !sel, drop = FALSE]
  }
  return(x)
}
# Function to filter out empty interaction cols in model.matrix. This is done
# when variables are centered prior to fitting model (with function center_x),
# but not done by model.matrix. So we either Need to change model.matrix to not
# include empty interactions or change center_x to not remove those empty cols
# Not sure stan models can be fit with an empty cols, so I think it makes sense
# to change value return by model.matrix. Ideally I'd make change within
# model.matrix, but to avoid messing too much I'm just going to create helper
# function that runs after get_x in pp_data.

#' Drop columns with empty interaction levels
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
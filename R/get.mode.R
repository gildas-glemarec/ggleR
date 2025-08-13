#' Replace NA values in a variable with the most frequent (mode) value of that
#' variable
#' @param x The variable for which to get the mode
#' @return Same dataset with updated variable x
#' @export
get.mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)][1] # Return the first mode if there are ties
}

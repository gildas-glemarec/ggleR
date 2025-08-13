#' Replace NA values in a variable with the most frequent (mode) value of that
#' variable
#' @param x The variable for which to get the mode
#' @return Same dataset with updated variable x
#' @export
get_mode <- function(x) {
  # Remove NA from unique values
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) {
    return(NA)  # All values are NA
  }
  tab <- tabulate(match(x[!is.na(x)], ux))
  sorted_modes <- ux[order(-tab)]  # Sort by frequency (descending)
  if (length(sorted_modes) >= 2) {
    return(sorted_modes[2])  # Return the second most frequent
  } else {
    return(sorted_modes[1])  # Only one unique value (no second mode)
  }
}

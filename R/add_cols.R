#' Add columns to ensure compatibility with previous EM dataset
#' @param df A data frame. Usually the output of BBimport("path_to_files")
#' @param cols A data frame. Usually the output of fix.CQ("path_to_bycatch_data")
#' @return data.frame object
#' @export
add_cols <- function(df, cols = c('mesh', 'quality', 'camera')) {
  data.table::setorderv(df, c("vessel","date","IDevent"))
  add <- cols[!cols %in% names(df)]
  df = as.data.frame(df)
  if(length(add) != 0 ) df[add] <- NA
  data.table::setDT(df)
  return(df)
}

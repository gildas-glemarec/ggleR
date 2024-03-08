#' Figures out ICES subrectangle from lon/lat
#' I stole this script from someone on the internet. Thank you stranger for your unknown contribution to my work.
#' @param lon longitude (in decimal)
#' @param lat latitude (in decimal)
#' @return Same dataset with added variables
#' @export
ices.subrect <- function (lon, lat)
{
  x <- floor(lon + 60) + 1000
  y <- floor(lat * 2) - 71 + 100
  num1 <- substr(y, 2, 3)
  lett <- LETTERS[as.numeric(substr(x, 2, 3))]
  num2 <- substr(x, 4, 4)
  res <- paste(num1, lett, num2, sep = "")
  centroid.ices.rect <- mapplots::ices.rect(res)
  num3 <- lon - centroid.ices.rect[,1]
  num4 <- lat - centroid.ices.rect[,2]
  X <- data.table::data.table(res, centroid.ices.rect, num3, num4)

  i <- function(if_stat, then) {
    if_stat <- lazyeval::expr_text(if_stat)
    then    <- lazyeval::expr_text(then)
    sprintf("ifelse(%s, %s, ", if_stat, then)
  }
  e <- function(else_ret) {
    else_ret <- lazyeval::expr_text(else_ret)
    else_ret
  }
  ie <- function(...) {
    args <- list(...)

    for (i in 1:(length(args) - 1) ) {
      if (substr(args[[i]], 1, 6) != "ifelse") {
        stop("All but the last argument, need to be i functions.", call. = FALSE)
      }
    }
    if (substr(args[[length(args)]], 1, 6) == "ifelse"){
      stop("Last argument needs to be an e function.", call. = FALSE)
    }
    args$final <- paste(rep(')', length(args) - 1), collapse = '')
    eval_string <- do.call('paste', args)
    eval(parse(text = eval_string))
  }
  X$subRect <-
    ie(
      i(num3 <= -0.08333333 & num4 >= 0.16666667, paste(res, 1, sep = "")),
      i(num3 >= 0.08333333 & num4 >= 0.16666667, paste(res, 7, sep = "")),
      i(num3 <= -0.08333333 && num4 <= -0.16666667, paste(res, 3, sep = "")),
      i(num3 >= 0.08333333 && num4 <= -0.16666667, paste(res, 9, sep = "")),
      i(dplyr::between(num3, -0.08333333, 0.08333333) && num4 <= -0.16666667, paste(res, 6, sep = "")),
      i(dplyr::between(num3, -0.08333333, 0.08333333) && num4 >= 0.16666667, paste(res, 4, sep = "")),
      i(num3 >= 0.08333333 && dplyr::between(num4, -0.16666667, 0.16666667), paste(res, 8, sep = "")),
      i(num3 <= -0.08333333 && dplyr::between(num4, -0.16666667, 0.16666667), paste(res, 2, sep = "")),
      e(paste(res, 5, sep = ""))
    )
  return(X$subRect)
}

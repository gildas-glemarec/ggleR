% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_bycatch_records.R
\name{add_bycatch_records}
\alias{add_bycatch_records}
\title{Add bycatch records from catch quantification data in Analyzer}
\usage{
add_bycatch_records(
  x = data_work,
  y = NULL,
  alt_spp_list = F,
  rm_errors = TRUE
)
}
\arguments{
\item{x}{A data frame. Usually the output of BBimport("path_to_files")}

\item{y}{A data frame. Usually the output of fix.CQ("path_to_bycatch_data")}

\item{alt_spp_list}{Experimental. Are you providing an alternative species list? Defaults to FALSE.}

\item{rm_errors}{Defaults to TRUE. Removes the problematic bycatch events (those to fix manually) and prints them}
}
\value{
data.frame object
}
\description{
Add bycatch records from catch quantification data in Analyzer
}

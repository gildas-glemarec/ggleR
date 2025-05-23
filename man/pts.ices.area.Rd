% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pts.ices.area.R
\name{pts.ices.area}
\alias{pts.ices.area}
\title{Finds the ICES area a geographic point falls into}
\usage{
pts.ices.area(
  x,
  dsn = "Q:/20-forskning/12-gis/Dynamisk/GEOdata/BasicLayers/Boundaries/Ices/ICES_areas",
  layer = "ICES_Areas_20160601_cut_dense_3857"
)
}
\arguments{
\item{x}{dataset with geographic coordinates in decimal as lon/lat}

\item{dsn}{Path to the a directory with ICES shapefiles}

\item{layer}{name of the layer to use}
}
\value{
Same dataset with added variables
}
\description{
Finds the ICES area a geographic point falls into
}

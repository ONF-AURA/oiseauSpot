% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md_crowns_metrics.R
\name{md_crowns_metrics}
\alias{md_crowns_metrics}
\title{Rasters des métriques des couronnes}
\usage{
md_crowns_metrics(
  date_mnh = util_get_date("last", "mnh"),
  path_mnh_ts = data_conf("path_mnh_ts"),
  path_crowns_ts = data_conf("path_crowns_ts"),
  path_meta = data_conf("tab_crowns"),
  path_mnt = data_conf("path_mnt"),
  buffer = data_conf("buffer"),
  mask = data_conf("shp"),
  lim_h_rege = data_conf("lim_h_rege"),
  best_day = data_conf("spot_best_day"),
  dest_dir = dc("dos_modeles"),
  spot_date = "last",
  topo = TRUE,
  dendro = TRUE,
  spot = TRUE,
  insol = TRUE,
  mnh = TRUE,
  sentinel = TRUE,
  copernicus = TRUE,
  rsp_coper = FALSE,
  force = FALSE
)
}
\arguments{
\item{date_mnh}{date du MNH à utiliser}

\item{mnh}{}

\item{crowns}{raster des couronnes issu de data_crowns}
}
\value{
spatRaster
}
\description{
Rasters des métriques des couronnes
}

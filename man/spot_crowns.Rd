% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spot_crowns.R
\name{spot_crowns}
\alias{spot_crowns}
\title{Détection des couronnes sur MNH}
\usage{
spot_crowns(
  ext = oiseauData::data_conf("shp"),
  path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
  buffer = oiseauData::data_conf("buffer"),
  lim_h_rege = oiseauData::data_conf("lim_h_rege"),
  path_mnh_mask_ts = oiseauData::data_conf("path_mnh_mask_ts"),
  an = oiseauData::data_conf("an1"),
  pente = 0.07,
  intercept = 2
)
}
\arguments{
\item{ext}{polygone sf de l'étendue à détecter}

\item{path_mnh_ts}{chemin du MNH}

\item{buffer}{tampon à appliquer à exr}

\item{lim_h_rege}{hauteur minimale des apex à détecter}

\item{path_mnh_mask_ts}{chemin spatraster 0/1, où 0 correspond aux couronnes disparues les années précédentes}

\item{an}{année du MNH utilisé}

\item{pente}{voir lidR::locate_trees}

\item{intercept}{voir lidR::locate_trees}
}
\value{
spatvector des couronnes
}
\description{
Détection des couronnes sur MNH
}

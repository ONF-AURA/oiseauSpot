#' Insolation à une date donnée
#'
#' @param dem raster modèle numérique d'élévation (MNT + MNH)
#' @param month mois
#' @param day jour
#'
#' @return raster d'insolation globale en J/m2
#' @export
#'

spot_sun <- function(dem, month = 7, day = 21){

  cgr=insol::cgrad(dem)
  demm=raster::as.matrix(dem)
  dl=raster::res(dem)[1]

  ## Isolation at 2 h interval over the length of the day
  ## RH and temp would cahnge over the dy, here we use a constant value for simplicity

  height=raster::quantile(dem, .5, na.rm = TRUE)
  visibility=30
  RH=80
  tempK=288
  tmz=0
  year=2022
  month=month
  day=day
  timeh=12
  jd=insol::JDymd(year,month,day,hour=timeh)
  Iglobal=array(0,dim=dim(demm))
  deltat=2 #heures

  coo = sf::st_point(apply(raster::bbox(dem), 1, mean)) %>%
    sf::st_sfc(crs = 2154) %>%
    sf::st_transform(4326) %>% sf::st_coordinates()

  lat=coo[2]
  lon=coo[1]

  dayl=insol::daylength(lat,lon,jd,0)

  for (srs in seq(dayl[1],dayl[2],deltat)){
    message("heure: ", srs)
    jd=insol::JDymd(year,month,day,hour=srs)
    sv=insol::sunvector(jd,lat,lon,tmz)
    hsh=insol::hillshading(cgr,sv)
    sh=insol::doshade(demm,sv,dl)
    zenith=insol::sunpos(sv)[2]
    Idirdif = insol::insolation(zenith,jd,height,visibility,RH,tempK,0.002,0.15)
    ## direct radiation modified by terrain + diffuse irradiation (skyviewfactor ignored)
    ## values in J/m^2
    Iglobal = Iglobal + (Idirdif[,1] * hsh + Idirdif[,2] )*3600*deltat
  }


  ## rasterize to plot nicely
  Iglobal=raster::raster(Iglobal,crs=raster::projection(dem))
  raster::extent(Iglobal)=raster::extent(dem)

  Iglobal

}

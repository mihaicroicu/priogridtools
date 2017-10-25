#' The row (int) of the PRIOGrid structure corresponding to a latitude (float)
#'
#' @param float A latitude provided as a float (eg. -1.25). Latitude must be expressed as WGS84, i.e. [-90..+90]
#' @return integer The corresponding PRIOGrid row id
#' @export
#' @examples
#' pg_row(27.1)
#' pg_row(-24.5)
pg_row <- function(lat) {
  if (lat>90 || lat< (-90)) return(NA)
  return ((90+floor(lat*2)/2)*2+1)
}

#' The column (int) of the PRIOGrid structure corresponding to a longitude (float)
#'
#' @param float A longitude provided as a float (eg. -1.25). Longitude must be expressed as WGS84, i.e. [-180..+180]
#' @return integer The corresponding PRIOGrid column id
#' @export
#' @examples
#' pg_col(27.1)
#' pg_col(-24.5)
pg_col <- function(lon) {
  if (lon>180 || lon< (-180)) return(NA)
  return ((180+floor(lon*2)/2)*2+1)
}

#' The PRIOGrid GID (int) of the corresponding to a pair of coordinates (latitude (float) and longitude (float)).
#' Compatible with PRIOGrid v.1.x and v.2.x
#'
#' @param float A latitude provided as a float (eg. -1.25). Latitude must be expressed as WGS84, i.e. [-90..+90]
#' @param float A longitude provided as a float (eg. -1.25). Longitude must be expressed as WGS84, i.e. [-180..+180]
#' @return integer The corresponding PRIOGrid GID
#' @export
#' @examples
#' pg_priogrid(27.1,11.2)
#' pg_priogrid(-24.5.8.21)
pg_priogrid <- function(lat,lon) {
  return ((pg_row(lat)-1)*720 + pg_col(lon))
}

.onAttach <- function(libname, pkgname) {
  #packageStartupMessage(pg_priogrid(1.55,1.25))
  stopifnot(pg_priogrid(1.55,1.25)==132123)
  stopifnot(pg_priogrid(0,0)==129961)
  stopifnot(pg_priogrid(-10.55,20.23)==114161)
  stopifnot(pg_priogrid(82,-102.99)==247835)
  stopifnot(is.na(pg_priogrid(91,109)))
}

st_azimuth <- function(x, y, ..., unit="rad") UseMethod("st_azimuth")

# fallback method
st_azimuth.default <- function(x, y, ..., unit="rad")
  paste0("st_azimuth is not implemented for " , class(x)[1])

st_azimuth.sf <- function(x, y, ..., unit="rad")
  st_azimuth(st_geometry(x), if(!missing(y)) st_geometry(y), ..., unit=unit)

st_azimuth.sfc_POINT <- function(x, y, ..., unit="rad"){
  if(!missing(y) && !inherits(y, "sfc_POINT")) stop("y is not sfc_POINT.")
  if(!requireNamespace("units")) stop("please install units package")
  ll <- st_is_longlat(x)
  if (!ll || is.na(ll)){
    z <- if(missing(y)) diff(x) else (y-x)
    z <- do.call(rbind, z)
    eq <- z[,1]==0 & z[,2]==0
    z <- units::set_units((pi/2-atan2(z[,2], z[,1])) %% (2*pi), "rad")
    #  identical points because azimuth (pi/2) is meaningless and is set to NaN
    if(any(eq)) {
      warning("found identical points!")
      z[eq] <- NaN
    }
  } else {
    if(!requireNamespace("lwgeom")) stop("please install lwgeom package")
    if(!missing(y)){
      z <- mapply(x, y, FUN=st_sfc, MoreArgs=list(crs=st_crs(x)), SIMPLIFY=F)
      z <- units::set_units(sapply(z, lwgeom::st_geod_azimuth, ...), "rad")
    } else
      z <- lwgeom::st_geod_azimuth(x, ...)
  }
  # convert units e.g. to degree. BUG : it zaps z. Report
  units(z) <- units::as_units(unit)
  z
}

st_azimuth.sfc_LINESTRING <- function(x, y, ..., unit="rad", simplify=TRUE){
  sfcp <- st_cast(st_sf(g=x, id=seq_along(x)), 'POINT')
  tapply(sfcp$g, sfcp$id, st_azimuth, unit=unit, simplify=simplify)
}

# used by st_azimuth.sfc_POINT
# to compute dx, dy of consecutive points. sets crs to NA
diff.sfc_POINT <- function(x) x[-1]-x[-length(x)]

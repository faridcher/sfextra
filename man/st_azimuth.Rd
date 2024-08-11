\name{st_azimuth}
\alias{st_azimuth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Planar and geodetic azimuth of two points
}
\description{
Computes the azimuths of two points. A set of geometries may be used.
}
\usage{
st_azimuth(x, y, ..., unit = "rad")

## S3 method for class sfc_POINT and sf
st_azimuth(x, y, ..., unit = "rad")

## S3 method for class sfc_POINT, sfc_LINESTRING and sf
st_azimuth(x, y, ..., unit="rad", simplify=TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
If y is missing, and x is sfc_POINT compute the azimuths of two consecutive points. If y is missing and x is sfc_LINESTRING, assign a unique ID to each linestring; cast the linestring to POINT and then compute azimuth of consecutive points. If x is sf, compute azimuth of its geometry column.
}
  \item{y}{
    If y is a sfc_POINT, then compute pairwise azimuth between x and y.
}
  \item{\dots}{
 Additional arguments passed to \code{st_geod_azimuth}
}
  \item{unit}{
    Whether to convert the unit of azimuth e.g. to degree.
  }
  \item{simplify}{
    For sfc_LINESTRING \code{x}, whether to simplify azimuth vector.
  }
}
\details{
If the CRS of \code{x} is lat/lon, then \code{lwgeom:st_geod_azimuth} is called and a geodetic azimuth is computed.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Farid Cheraghi \url{mailto:faridcher@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{st_geod_azimuth}} and \code{\link{atan2}}
}
\examples{
x <- st_sfc(st_point(c(0,1)), st_point(c(1,1)))
st_azimuth(x)

# planar azimuth
st_azimuth(x[1:2], x[2:1], units="degree")

# geodetic azimuth
st_crs(x) <- 4326
st_azimuth(x[1:2], x[2:1], units="degree")

# sfc_LINESTRING
pts = matrix(1:6, , 2)
ls1 = st_linestring(pts)
st_azimuth(st_sfc(ls1,ls1))

# sf
st_azimuth(st_sf(g=st_sfc(ls1,ls1)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

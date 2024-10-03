# (qgis) explode a linestring to many linear segments
# called from export/g2b/darz.r
# example:
# co <- st_coordinates(xl)
# do.call(c, by(co[,1:2], co[,3], function(x) coords2segment(dup_mid_vertices(x))))

# x is a matrix of linestring vertices
dup_mid_vertices <- function(x){
  x[c(1, rep(2:(nrow(x)-1), each=2), nrow(x)),]
}

# converts a matrix of x,y coordinates to linestring segments
# x is from dup_mid_vertices function
coords2segments <- function(x){
  x <- cbind(x, rep(seq(1, nrow(x)/2), each=2))
  lines <- by(x[, 1:2], x[, 3], function(b) st_linestring(as.matrix(b)))
  do.call(st_sfc, c(lines))
}

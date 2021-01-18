library("polyCub")

letterR <- list(
    ## R exterior ring given anticlockwise
    list(x = c(3.9, 3.8, 3.7, 3.5, 3.4, 3.5, 3.7, 3.8, 3.8, 3.7,
               3.7, 3.5, 3.3, 2, 2, 2.7, 2.7, 2.9, 3, 3.3, 3.9),
         y = c(0.7, 1.1, 1.3, 1.7, 1.8, 1.9, 2.1, 2.3, 2.5, 2.8, 3,
               3.2, 3.3, 3.3, 0.7, 0.7, 1.7, 1.7, 1.5, 0.7, 0.6)),
    ## R hole given clockwise
    list(x = c(2.6, 2.6, 3, 3.1, 3.2, 3.1, 3.1, 3),
         y = c(2.2, 2.7, 2.7, 2.6, 2.5, 2.4, 2.3, 2.2))
)

## need to reverse ring direction for sp
library("sp")
letterR_sp <- SpatialPolygons(list(Polygons(
    lapply(letterR, function(coords) Polygon(lapply(coords, rev))),
    ID = "R"
)))

## xylist.SpatialPolygons() produces original (owin) vertex order
stopifnot(identical(xylist(letterR_sp), letterR))


## CAVE: sf does not enforce a particular ring direction by default
library("sf")
letterR_sfg <- st_as_sfc(letterR_sp)[[1]]
stopifnot(inherits(letterR_sfg, "sfg"), inherits(letterR_sfg, "POLYGON"))
## unclass(letterR_sfg)  # sfg simply keeps sp vertex order! (@sf 0.9-7)

## xylist.sfg() passes via st_sfc() with check_ring_dir = TRUE
stopifnot(
    identical(xylist(letterR_sfg), letterR),
    identical(xylist(st_multipolygon(list(letterR_sfg, letterR_sfg))),
              rep(letterR, 2))
)

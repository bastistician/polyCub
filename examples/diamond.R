diamond <- list(x = c(1,2,1,0), y = c(1,2,3,2))  # anti-clockwise
diamond.sp <- sp::Polygon(lapply(diamond, rev))  # clockwise

diamond.Ps <- sp::Polygons(list(diamond.sp), ID = "my diamond")
diamond.SpPs <- sp::SpatialPolygons(list(diamond.Ps))


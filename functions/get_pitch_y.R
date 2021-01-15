library(tidyverse)
library(geosphere)


get_pitch_y <- function(p0_long, p0_lat, p1_long, p1_lat, point_long, point_lat, w){
  
  a <- distm(c(p0_long, p0_lat), c(point_long, point_lat), fun = distVincentyEllipsoid)
  b <-distm(c(point_long, point_lat), c(p1_long, p1_lat), fun = distVincentyEllipsoid)
  c <- distm(c(p0_long, p0_lat), c(p1_long,  p1_lat), fun = distVincentyEllipsoid)
  
  p0_angle <- acos((c^2+a^2 -b^2)/(2*c*a))
  
  z <- cos(p0_angle) * a
  
  y <- z/c * w
  
  return(y)
}

get_pitch_y <- Vectorize(get_pitch_y)
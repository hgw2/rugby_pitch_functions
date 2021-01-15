library(tidyverse)
library(geosphere)

get_pitch_x <- function(p0_long, p0_lat, p2_long, p2_lat, point_long, point_lat, h){
  
  a <- distm(c(p0_long, p0_lat), c(point_long, point_lat), fun = distVincentyEllipsoid)
  b <-distm(c(point_long, point_lat), c(p2_long, p2_lat), fun = distVincentyEllipsoid)
  c <- distm(c(p0_long, p0_lat), c(p2_long,  p2_lat), fun = distVincentyEllipsoid)
  
  p0_angle <- acos((c^2+a^2 -b^2)/(2*c*a))
  
  z <- cos(p0_angle) * a
  
  x <- z/c * h
  
  return(x)
}

get_pitch_x <- Vectorize(get_pitch_x)
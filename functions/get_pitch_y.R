get_pitch_y <- function(p0_long, p0_lat, p1_long, p1_lat, point_long, point_lat, w){
  
  a <- have_dist(long1 = p0_long, lat1 = p0_lat, long2 = point_long, lat2 = point_lat)
  b <- have_dist(long1 = point_long, lat1 = point_lat, long2 = p1_long, lat2 = p1_lat)
  c <- have_dist(long1 = p0_long, lat1 = p0_lat, long2 = p1_long, lat2 = p1_lat)
  
  p0_angle <- acos((c^2+a^2 -b^2)/(2*c*a))
  
  z <- cos(p0_angle) * a
  
  y <- z/c * w
  
  return(y)
}

get_pitch_y <- Vectorize(get_pitch_y)
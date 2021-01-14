get_pitch_x <- function(p0_long, p0_lat, p2_long, p2_lat, point_long, point_lat, h){
  
  a <- have_dist(long1 = p0_long, lat1 = p0_lat, long2 = point_long, lat2 = point_lat)
  b <-have_dist(long1 = point_long, lat1 = point_lat, long2 = p2_long, lat2 = p2_lat)
  c <- have_dist(long1 = p0_long, lat1 = p0_lat, long2 = p2_long, lat2 = p2_lat)
  
  p0_angle <- acos((c^2+a^2 -b^2)/(2*c*a))
  
  z <- cos(p0_angle) * a
  
  x <- z/c * h
  
  return(x)
}

get_pitch_x <- Vectorize(get_pitch_x)
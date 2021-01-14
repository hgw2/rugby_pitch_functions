have_dist <- function(long1, lat1, long2, lat2) {
  
  R <- 6371
  diff_long <- (long2 - long1)
  diff_lat <- (lat2 - lat1)
  a <- sin(diff_lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff_long/2)^2
  b <- 2 * asin(pmin(1, sqrt(a))) 
  d = R * b
  return(d)
}
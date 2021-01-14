create_rugby_pitch <- function(grass_colour ="#ffffff", 
                               line_colour =  "#A9A9A9", 
                               background_colour = "#ffffff", 
                               goal_colour ="#000000", 
                               pitch_width = 70, 
                               pitch_length = 140,
                               lines = read_csv("pitch_lines.csv")){
  
  lines <- lines %>% 
    mutate(across(matches("x"), ~ ((.x/140) * pitch_length))) %>% 
    mutate(across(matches("y"), ~ ((.x/70) * pitch_width)))
  
  lines_1 <- lines %>% 
    filter(dash == F)
  
  lines_2 <-  lines %>% 
    
    filter(dash == T)
  
  theme_blankPitch = function(size=12) { 
    theme(
      #axis.line=element_blank(), 
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      #axis.ticks.margin=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      #       panel.border=element_blank(), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ymin <- 0 # minimum width
  ymax <- pitch_width # maximum width
  xmin <- 0 # minimum length
  xmax <- pitch_length # maximum length
  
  glxst <- (20/140) * pitch_length
  glyst <- (32.5/70) * pitch_width
  glxed <- (120/140) * pitch_length
  glyed <- (37.5/70) * pitch_width
  
  ## initiate the plot, set some boundries to the plot
  pitch <- ggplot() + 
    xlim(c(xmin -5 ,xmax + 5)) + 
    ylim(c(ymin - 5,ymax +5)) +
    theme_blankPitch() +
    #add lines etc
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) + 
    geom_segment(data = lines_1, aes(x = x, y = y, xend = xend, yend = yend), colour = line_colour,size = 0.5) +
    geom_segment(data = lines_2, aes(x = x, y = y, xend = xend, yend = yend), linetype="dashed", colour = line_colour,size = 0.5) +
    geom_segment(aes(x = glxst, y =  glyst, xend = glxst, yend = glyed),colour = goal_colour, size = 1) +
    geom_segment(aes(x =glxed, y =  glyst, xend =glxed, yend = glyed),colour = goal_colour, size = 1)
  
  return(pitch)
}
create_rugby_pitch_lines <- function(plot, grass_colour = "#ffffff",
                                             line_colour = "#A9A9A9",
                                             background_colour = "#ffffff",
                                             goal_colour = "#000000",
                                             pitch_width = 70,
                                             pitch_length = 100, 
                                     in_goal = 20) {
  
  ymin <- 0 # minimum width
  ymax <- pitch_width # maximum width
  xmin <- 0 # minimum length
  xmax <- pitch_length + (in_goal * 2) # maximum length
  
  glxst <- in_goal 
  glyst <- (32.5 / 70) * pitch_width
  glxed <- in_goal + (100 / 100) * pitch_length
  glyed <- (37.5 / 70) * pitch_width
  
  # Pitch Outline
p <-  plot + 
  # Try line 1
  annotate("segment",
           x = in_goal ,
           y = 0 ,
           xend = in_goal,
           yend =  pitch_width,
           colour = line_colour,
           size = 0.5
  ) +
  # 22 1
  annotate("segment",
           x = in_goal + (22 / 100 * pitch_length),
           y = (0 / 70 * pitch_width),
           xend = in_goal + (22  / 100 * pitch_length),
           yend = (70 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  # T10m 1
  annotate("segment",
           x = in_goal + (40 / 100 * pitch_length),
           y = (0 / 70 * pitch_width),
           xend = in_goal + (40 / 100 * pitch_length),
           yend = (70 / 70 * pitch_width),
           colour = line_colour,
           linetype = "dashed",
           size = 0.5
  ) +
  # halfway
  annotate("segment",
           x = in_goal + (50/ 100 * pitch_length),
           y = (0 / 70 * pitch_width),
           xend = in_goal + (50/ 100 * pitch_length),
           yend = (70 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  # T10m 2
  annotate("segment",
           x = in_goal + (60/ 100 * pitch_length),
           y = (0 / 70 * pitch_width),
           xend = in_goal + (60 / 100 * pitch_length),
           yend = (70 / 70 * pitch_width),
           colour = line_colour,
           linetype = "dashed",
           size = 0.5
  ) +
  # 22 2
  annotate("segment",
           x = in_goal + (78/ 100 * pitch_length),
           y = (0 / 70 * pitch_width),
           xend = in_goal + (78/ 100 * pitch_length),
           yend = (70 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  # try line 2
  annotate("segment",
           x = in_goal + (100 / 100 * pitch_length),
           y = (0 / 70 * pitch_width),
           xend = in_goal + (100/ 100 * pitch_length),
           yend = (70 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  # 5m channel
  annotate("segment",
           x = in_goal,
           y = (5 / 70 * pitch_width),
           xend = in_goal + (100 / 100 * pitch_length),
           yend = (5 / 70 * pitch_width),
           colour = line_colour,
           linetype = "dashed",
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (5 / 100 * pitch_length),
           y = (2 / 70 * pitch_width),
           xend = in_goal + (5/ 100 * pitch_length),
           yend = (8 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  
  # 5m channel
  annotate("segment",
           x = in_goal,
           y = (65 / 70 * pitch_width),
           xend = in_goal + (100/ 100 * pitch_length),
           yend = (65 / 70 * pitch_width),
           colour = line_colour,
           linetype = "dashed",
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (95/ 100 * pitch_length),
           y = (2 / 70 * pitch_width),
           xend = in_goal + (95/ 100 * pitch_length),
           yend = (8 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  # 5m lines
  annotate("segment",
           x = in_goal + (5 / 100 * pitch_length),
           y = (12 / 70 * pitch_width),
           xend = in_goal + (5 / 100 * pitch_length),
           yend = (18 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (95 / 100 * pitch_length),
           y = (12 / 70 * pitch_width),
           xend = in_goal + (95 / 100 * pitch_length),
           yend = (18 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  # 15m line
  annotate("segment",
           x = in_goal,
           y = (15 / 70 * pitch_width),
           xend = in_goal + (5 / 100 * pitch_length),
           yend = (15 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (19/ 100 * pitch_length),
           y = (15 / 70 * pitch_width),
           xend = in_goal + (25/ 100 * pitch_length),
           yend = (15 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (37 / 100 * pitch_length),
           y = (15 / 70 * pitch_width),
           xend = in_goal + (43 / 100 * pitch_length),
           yend = (15 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (47 / 100 * pitch_length),
           y = (15 / 70 * pitch_width),
           xend = in_goal + (53 / 100 * pitch_length),
           yend = (15 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (57 / 100 * pitch_length),
           y = (15 / 70 * pitch_width),
           xend = in_goal + (63 / 100 * pitch_length),
           yend = (15 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (75 / 100 * pitch_length),
           y = (15 / 70 * pitch_width),
           xend = in_goal + (81 / 100 * pitch_length),
           yend = (15 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (100 / 100 * pitch_length),
           y = (15 / 70 * pitch_width),
           xend = in_goal + (95 / 100 * pitch_length),
           yend = (15 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (5 / 100 * pitch_length),
           y = (62 / 70 * pitch_width),
           xend = in_goal +( 5 / 100 * pitch_length),
           yend = (68 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (95 / 100 * pitch_length),
           y = (62 / 70 * pitch_width),
           xend = in_goal + (95 / 100 * pitch_length),
           yend = (68 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  # 15m channel
  annotate("segment",
           x = in_goal + (5 / 100 * pitch_length),
           y = (52 / 70 * pitch_width),
           xend = in_goal + (5 / 100 * pitch_length),
           yend = (58 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (95 / 100 * pitch_length),
           y = (52 / 70 * pitch_width),
           xend = in_goal + (95 / 100 * pitch_length),
           yend = (58 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal,
           y = (55 / 70 * pitch_width),
           xend = ((in_goal + 5)/ 100 * pitch_length),
           yend = (55 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (19 / 100 * pitch_length),
           y = (55 / 70 * pitch_width),
           xend = in_goal + (25 / 100 * pitch_length),
           yend = (55 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (37  / 100 * pitch_length),
           y = (55 / 70 * pitch_width),
           xend = in_goal + (43 / 100 * pitch_length),
           yend = (55 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal +( 47 / 100 * pitch_length),
           y = (55 / 70 * pitch_width),
           xend = in_goal + (53 / 100 * pitch_length),
           yend = (55 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5)+
  
  annotate("segment",
           x = in_goal + (57/ 100 * pitch_length),
           y = (55 / 70 * pitch_width),
           xend = in_goal + (63/ 100 * pitch_length),
           yend = (55 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (75 / 100 * pitch_length),
           y = (55 / 70 * pitch_width),
           xend = in_goal + (81/ 100 * pitch_length),
           yend = (55 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  annotate("segment",
           x = in_goal + (100/ 100 * pitch_length),
           y = (55 / 70 * pitch_width),
           xend = in_goal + (95  / 100 * pitch_length),
           yend = (55 / 70 * pitch_width),
           colour = line_colour,
           size = 0.5
  ) +
  
  
  
  geom_segment(aes(x = glxst, y =  glyst, xend = glxst, yend = glyed),colour = goal_colour, size = 1) +
  geom_segment(aes(x =glxed, y =  glyst, xend =glxed, yend = glyed),colour = goal_colour, size = 1)


return(p)
}
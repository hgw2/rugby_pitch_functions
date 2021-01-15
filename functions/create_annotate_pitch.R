create_annotate_pitch <- function(grass_colour = "#ffffff",
                                  line_colour = "#A9A9A9",
                                  background_colour = "#ffffff",
                                  goal_colour = "#000000",
                                  pitch_width = 70,
                                  pitch_length = 100, 
                                  in_goal = 20) {
  theme_blankPitch <- function(size = 12) {
    theme(
      # axis.line=element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      # axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length = unit(0, "lines"),
      # axis.ticks.margin=unit(0, "lines"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.background = element_rect(fill = background_colour, colour = NA),
      legend.key = element_rect(colour = background_colour, fill = background_colour),
      legend.key.size = unit(1.2, "lines"),
      legend.text = element_text(size = size),
      legend.title = element_text(size = size, face = "bold", hjust = 0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background = element_rect(fill = background_colour, colour = background_colour),
      #       panel.border=element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = element_blank(),
      plot.background = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      plot.title = element_text(size = size * 1.2),
      strip.text.y = element_text(colour = background_colour, size = size, angle = 270),
      strip.text.x = element_text(size = size * 1)
    )
  }

  ymin <- 0 # minimum width
  ymax <- pitch_width # maximum width
  xmin <- 0 # minimum length
  xmax <- pitch_length + (in_goal * 2) # maximum length

  glxst <- (in_goal / 100) * pitch_length
  glyst <- (32.5 / 70) * pitch_width
  glxed <- ((in_goal + 100) / 100) * pitch_length
  glyed <- (37.5 / 70) * pitch_width

  ## initiate the plot, set some boundaries to the plot
  pitch <- ggplot() +
    xlim(c(xmin - 5, xmax + 5)) +
    ylim(c(ymin - 5, ymax + 5)) +
    theme_blankPitch() +
    # add lines
    # Pitch Outline
    annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = grass_colour, colour = line_colour) +
    
    # Try line 1
    annotate("segment",
             x = (in_goal / 100 * pitch_length),
             y = (0 / 70 * pitch_width),
             xend = (in_goal / 100 * pitch_length),
             yend = (70 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    # 22 1
    annotate("segment",
             x = ((in_goal + 22) / 100 * pitch_length),
             y = (0 / 70 * pitch_width),
             xend = ((in_goal + 22)  / 100 * pitch_length),
             yend = (70 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    # T10m 1
    annotate("segment",
             x = ((in_goal + 40) / 100 * pitch_length),
             y = (0 / 70 * pitch_width),
             xend = ((in_goal + 40) / 100 * pitch_length),
             yend = (70 / 70 * pitch_width),
             colour = line_colour,
             linetype = "dashed",
             size = 0.5
    ) +
    # halfway
    annotate("segment",
             x = ((in_goal + 50) / 100 * pitch_length),
             y = (0 / 70 * pitch_width),
             xend = ((in_goal + 50)/ 100 * pitch_length),
             yend = (70 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    # T10m 2
    annotate("segment",
             x = ((in_goal + 60)/ 100 * pitch_length),
             y = (0 / 70 * pitch_width),
             xend = ((in_goal + 60) / 100 * pitch_length),
             yend = (70 / 70 * pitch_width),
             colour = line_colour,
             linetype = "dashed",
             size = 0.5
    ) +
    # 22 2
    annotate("segment",
             x = ((in_goal + 78) / 100 * pitch_length),
             y = (0 / 70 * pitch_width),
             xend = ((in_goal + 78)/ 100 * pitch_length),
             yend = (70 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    # try line 2
    annotate("segment",
             x = ((in_goal + 100) / 100 * pitch_length),
             y = (0 / 70 * pitch_width),
             xend = ((in_goal + 100)/ 100 * pitch_length),
             yend = (70 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    # 5m channel
    annotate("segment",
             x = ((in_goal)/ 100 * pitch_length),
             y = (5 / 70 * pitch_width),
             xend = ((in_goal + 100) / 100 * pitch_length),
             yend = (5 / 70 * pitch_width),
             colour = line_colour,
             linetype = "dashed",
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 5) / 100 * pitch_length),
             y = (2 / 70 * pitch_width),
             xend = ((in_goal + 5)/ 100 * pitch_length),
             yend = (8 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    
    # 5m channel
    annotate("segment",
             x = ((in_goal)/ 100 * pitch_length),
             y = (65 / 70 * pitch_width),
             xend = ((in_goal + 100)/ 100 * pitch_length),
             yend = (65 / 70 * pitch_width),
             colour = line_colour,
             linetype = "dashed",
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 95)/ 100 * pitch_length),
             y = (2 / 70 * pitch_width),
             xend = ((in_goal + 95)/ 100 * pitch_length),
             yend = (8 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    # 5m lines
    annotate("segment",
             x = ((in_goal + 5) / 100 * pitch_length),
             y = (12 / 70 * pitch_width),
             xend = ((in_goal + 5)/ 100 * pitch_length),
             yend = (18 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 95) / 100 * pitch_length),
             y = (12 / 70 * pitch_width),
             xend = ((in_goal + 95)/ 100 * pitch_length),
             yend = (18 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    # 15m line
    annotate("segment",
             x = ((in_goal) / 100 * pitch_length),
             y = (15 / 70 * pitch_width),
             xend = ((in_goal + 5) / 100 * pitch_length),
             yend = (15 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 19)/ 100 * pitch_length),
             y = (15 / 70 * pitch_width),
             xend = ((in_goal + 25)/ 100 * pitch_length),
             yend = (15 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 37) / 100 * pitch_length),
             y = (15 / 70 * pitch_width),
             xend = ((in_goal + 43) / 100 * pitch_length),
             yend = (15 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 47) / 100 * pitch_length),
             y = (15 / 70 * pitch_width),
             xend = ((in_goal + 53) / 100 * pitch_length),
             yend = (15 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 57) / 100 * pitch_length),
             y = (15 / 70 * pitch_width),
             xend = ((in_goal + 63) / 100 * pitch_length),
             yend = (15 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 75) / 100 * pitch_length),
             y = (15 / 70 * pitch_width),
             xend = ((in_goal + 81) / 100 * pitch_length),
             yend = (15 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 100) / 100 * pitch_length),
             y = (15 / 70 * pitch_width),
             xend = ((in_goal + 95) / 100 * pitch_length),
             yend = (15 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 5) / 100 * pitch_length),
             y = (62 / 70 * pitch_width),
             xend = ((in_goal + 5)/ 100 * pitch_length),
             yend = (68 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 95) / 100 * pitch_length),
             y = (62 / 70 * pitch_width),
             xend = ((in_goal + 95)/ 100 * pitch_length),
             yend = (68 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    # 15m channel
    annotate("segment",
             x = ((in_goal + 5) / 100 * pitch_length),
             y = (52 / 70 * pitch_width),
             xend = ((in_goal + 5)/ 100 * pitch_length),
             yend = (58 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 95) / 100 * pitch_length),
             y = (52 / 70 * pitch_width),
             xend = ((in_goal + 95) / 100 * pitch_length),
             yend = (58 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal)/ 100 * pitch_length),
             y = (55 / 70 * pitch_width),
             xend = ((in_goal + 5)/ 100 * pitch_length),
             yend = (55 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 19) / 100 * pitch_length),
             y = (55 / 70 * pitch_width),
             xend = ((in_goal + 25) / 100 * pitch_length),
             yend = (55 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 37) / 100 * pitch_length),
             y = (55 / 70 * pitch_width),
             xend = ((in_goal + 43) / 100 * pitch_length),
             yend = (55 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 47) / 100 * pitch_length),
             y = (55 / 70 * pitch_width),
             xend = ((in_goal + 53) / 100 * pitch_length),
             yend = (55 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5)+
    
    annotate("segment",
             x = ((in_goal + 57)/ 100 * pitch_length),
             y = (55 / 70 * pitch_width),
             xend = ((in_goal + 63)/ 100 * pitch_length),
             yend = (55 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 75) / 100 * pitch_length),
             y = (55 / 70 * pitch_width),
             xend = ((in_goal + 81)/ 100 * pitch_length),
             yend = (55 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
    annotate("segment",
             x = ((in_goal + 100)/ 100 * pitch_length),
             y = (55 / 70 * pitch_width),
             xend = ((in_goal + 95) / 100 * pitch_length),
             yend = (55 / 70 * pitch_width),
             colour = line_colour,
             size = 0.5
    ) +
  
    
    
    geom_segment(aes(x = glxst, y =  glyst, xend = glxst, yend = glyed),colour = goal_colour, size = 1) +
    geom_segment(aes(x =glxed, y =  glyst, xend =glxed, yend = glyed),colour = goal_colour, size = 1)

  return(pitch)
}

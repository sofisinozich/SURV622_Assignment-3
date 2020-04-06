library(ggplot2)

# Set the default theme for plots
  if ('extrafont' %in% installed.packages()) {  
    if (Sys.info()["sysname"] == "Windows") {
      extrafont::loadfonts(device = 'win')
    }
    .font_to_use <- 'Arial Narrow'
  } else {
    .font_to_use <- "sans"
  }

  theme_set(
    theme_minimal(base_family = .font_to_use, base_size = 12) + 
      theme(plot.title.position = 'plot',
            plot.title = element_text(family = .font_to_use,
                                      face = "bold", size = 14),
            plot.subtitle = element_text(family = .font_to_use,
                                      face = "italic", size = 12),
            axis.title = element_text(family = .font_to_use,
                                      face = 'bold', size = 12,
                                      angle = 0),
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.text = element_text(size = 10),
            strip.text = element_text(family = .font_to_use,
                                        size = 12, angle = 0),
            strip.text.y.left = element_text(family = .font_to_use,
                                             size = 12, angle = 0),
            strip.text.x = element_text(family = .font_to_use,
                                        size=12, angle=0, face= "bold"),
            strip.placement = 'outside',
            legend.position = "top",
            legend.justification = c(0, 0),
            legend.text = ggplot2::element_text(size = (0.8)*12, colour = "#545454",
                                                family = .font_to_use),
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_rect(size = 0.5, colour = NA),
            )
  )
  
  rm(.font_to_use)
  
# Update the default color of geoms to match Maryland colors
  .default_color <- "gray60"

  ggplot2::update_geom_defaults('bar', list(colour = .default_color,
                                            fill = .default_color))
  ggplot2::update_geom_defaults('col', list(colour = .default_color,
                                            fill = .default_color))
  ggplot2::update_geom_defaults('point', list(colour = .default_color,
                                              fill = .default_color))
  ggplot2::update_geom_defaults('line', list(colour = .default_color, size = 1.5))

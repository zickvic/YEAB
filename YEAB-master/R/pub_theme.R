pub_theme <- function() {
  theme_gray() +
    theme(
      axis.line = element_line(colour = "black", size = 0.5),
      axis.ticks = element_line(colour = "black", size = 0.5),
      legend.key = element_blank(),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.key.size = unit(0.1, "cm"),
      legend.background = element_blank(),
      panel.spacing.x = unit(0.3, "lines"),
      panel.spacing.y = unit(0.3, "lines"),
      strip.background = element_blank(), # element_rect(fill = NA, color = '#ABABAB'),
      axis.title.x = element_text(size = 10),
      axis.text.x = element_text(size = 10, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
      axis.title.y = element_text(size = 10),
      axis.text.y = element_text(size = 10, margin = unit(c(t = 0.1, r = 2.5, b = 0, l = 0), "mm")),
      axis.ticks.length = unit(-1, "mm"),
      plot.margin = margin(0.2, .5, 0, 0.1, "cm"),
      panel.border = element_rect(fill = NA),
      panel.background = element_blank(),
      axis.text = element_text(colour = "black"),
      legend.position = "none"
    )
}

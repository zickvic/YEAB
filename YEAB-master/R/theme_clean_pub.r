theme_clean_pub <- function(base_size = 12,
                            base_family = "sans") {
  (
    theme_grey(
      base_size = base_size,
      base_family = base_family
    ) +
      theme(
        axis.line.x = element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        axis.line.y = element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        axis.ticks.length = unit(-1.4, "mm"),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(0.6, "lines"),
        axis.text = element_text(
          size = ceiling(base_size * 0.7),
          colour = "black"
        ),
        axis.text.x = element_text(margin = unit(c(t = 2, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 2, b = 0, l = 0), "mm")),
        axis.title = element_text(size = ceiling(base_size * 0.8)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(
          colour = "gray",
          linetype = "dotted"
        ),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(), # strip.background = element_rect(linetype = 0),
        strip.text = element_text(),
        strip.text.x = element_text(vjust = 0.5),
        strip.text.y = element_text(angle = -90),
        legend.text = element_text(
          size = ceiling(base_size * 0.9),
          margin = margin(l = -5, t = -5, b = -5, unit = "pt"),
          family = "sans"
        ),
        legend.title = element_text(
          size = base_size,
          face = "plain",
          family = "sans"
        ),
        legend.position = "right",
        legend.key = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin = margin(0.2, .5, 0, 0.1, "cm"),
        plot.title = element_text(
          size = ceiling(base_size * 1.1),
          face = "plain"
        ),
        plot.subtitle = element_text(size = ceiling(base_size * 1.05))
      )
  )
}

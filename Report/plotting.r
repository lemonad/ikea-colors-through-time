library(ggplot2)

plot_theme <- function(img, color_theme) {
  p1 <- grobTree(
    rectGrob(gp = gpar(fill = "#EBDDC3", col = "#EBDDC3")),
    rasterGrob(img, interpolate = TRUE)
  )
  p2 <- qplot(
    x = seq_len(nrow(color_theme)),
    y = 1,
    fill = factor(seq_len(nrow(color_theme))),
    geom = "tile"
  ) +
    scale_fill_manual(values = rgb(color_theme)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "#EBDDC3", color = "#EBDDC3")
    )
  grid.arrange(p1, p2, nrow = 1)
}

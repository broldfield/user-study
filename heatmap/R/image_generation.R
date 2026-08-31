save_heatmap_image <- function(plot, filename = "heatmaps.png", dir = "img", width = 14, height = 8, dpi = 300) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggsave(filename = file.path(dir, filename), plot = plot, width = width, height = height, dpi = dpi)
}

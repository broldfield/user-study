library(magick)

image_paths <- c(
  #"img/heatmap_all.png",  # add more paths below, e.g.:
  "img/heatmap_gbr_yes.png",
  "img/heatmap_gbr_no.png"
  #"img/heatmap_indus_research.png",
  #"img/heatmap_indus_non_research.png"
)
output_path <- "img/heatmap_gbr_both.png"

images <- image_read(image_paths)
stacked <- image_append(images, stack = TRUE)
image_write(stacked, path = output_path)

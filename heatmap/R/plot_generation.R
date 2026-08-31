library(patchwork)

create_heatmap <- function(Question_results,
                           question,
                           year,
                           global_max,
                           show_y_label = FALSE) {
  Qdf <- cbind.data.frame(row = 1:5, Question_results[[question]])
  Qlng <- pivot_longer(Qdf, cols = 2:6) |>
    mutate(col = str_sub(name, 5, 5)) |>
    mutate(value = value / global_max)
  heatmap <- ggplot(Qlng, aes(x = row, y = col, fill = value)) +
    geom_raster() +
    scale_fill_gradient(
      low = "lightgrey",
      high = "darkred",
      limits = c(0, 1),
      name = "Agreement"
    ) +
    ggtitle(year) +
    xlab(NULL) +
    ylab(if (show_y_label)
      "Preference for Univariate"
      else
        NULL) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.text = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(face = "bold", size = 16),
          )
}

stitch_heatmaps <- function(heatmaps, title) {
  wrap_plots(heatmaps, nrow = 1) +
    plot_layout(guides = "collect") +
    plot_annotation(title, caption = "Preference for Bivariate by Year") &
    theme(
      plot.title = element_text(face = "bold", size = 20),
      legend.position = "right",
      plot.caption = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
      legend.key.size = unit(1.2, "cm"),
      legend.title = element_text(size = 16, face = "bold", margin = margin(b= 10)),
      legend.text = element_text(size = 16, face="bold")
    )
}

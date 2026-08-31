source("R/data.R")
source("R/image_generation.R")
source("R/plot_generation.R")

config <- list(
  experiment = "both",
  version = "sev",
  filter = c("gbr_no"),
  # Add from filter map to change output.
  title = "Not familiar with the Great Barrier Reef"
)

filter_map <- list(
  gbr_yes = list(col = "Are you familiar with the Great Barrier Reef catchments?", value = "Yes"),
  gbr_no  = list(col = "Are you familiar with the Great Barrier Reef catchments?", value = "No"),
  indus_research = list(
    col = "What is the primary industry you work in?\r\n",
    value = c("Academia / Research", "Government (Research)")
  ),
  indus_research_no = list(
    col = "What is the primary industry you work in?\r\n",
    value = c("Academia / Research", "Government (Research)"),
    exclude = TRUE
  )
)

print_col12 <- function(df)
  dput(unique(df)[12])

s_main <- function(cfg) {
  df_original <- import_data_s(data = cfg$experiment)
  if (length(cfg$filter) > 0) {
    for (f in cfg$filter) {
      spec <- filter_map[[f]]
      df_original <- df_original |>
        filter(if (isTRUE(spec$exclude))
          ! (.data[[spec$col]] %in% spec$value)
          else
            .data[[spec$col]] %in% spec$value)
    }
  }
  stim_answers <- get_stim_answers(cfg, df_original)
  Question_results <- convert_to_pairings(stim_answers$df_lng2)
  heatmaps <- list(
    map1 = NULL,
    map2 = NULL,
    map3 = NULL,
    map4 = NULL,
    map5 = NULL
  )
  years <- c("1992/1993",
             "1999/2000",
             "2001/2002",
             "2002/2003",
             "2005/2006")

  global_max <- max(sapply(Question_results, max))

  for (x in 1:5) {
    x_name <- paste0("Preference for ", years[x], " Bivariate")
    y_name <- paste0("Preference for ", years[x], " Univariate")
    heatmaps[[x]] <- create_heatmap(
      Question_results,
      question = x,
      year = years[x],
      global_max,
      show_y_label = (x == 1)
    )
  }
  combined <- stitch_heatmaps(heatmaps, cfg$title)
  print(combined)
  save_heatmap_image(combined, width = 22, height = 5)

}

p_main <- function(cfg) {

}

main <- function(cfg) {
  if (cfg$version == "petra") {
    p_main(cfg)
  }
  if (cfg$version == "sev") {
    s_main(cfg)
  }
}

main(config)

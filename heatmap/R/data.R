library(readxl)
library(tidyverse)

import_data_s <- function(data = "both") {
  datoriA <- read_excel("data/responses/Vizumap User Study Experiment A.xlsx")
  expA <- c("Eg",
            "Q4",
            "Q5B",
            "Q2",
            "Q5",
            "Q3",
            "Q3B",
            "Q2B",
            "Q1",
            "Q4B",
            "Q1B")
  colnames(datoriA)[14:24] <- expA


  datoriB <- read_excel("data/responses/Vizumap User Study Experiment B.xlsx")
  expB <- c("Eg",
            "Q1B",
            "Q3B",
            "Q4",
            "Q4B",
            "Q2B",
            "Q3",
            "Q1",
            "Q5B",
            "Q5",
            "Q2")
  colnames(datoriB)[14:24] <- expB
  if (data == "a") {
    return(datoriA)
  }
  if (data == "b") {
    return(datoriB)
  }

  datori <- bind_rows(datoriA, datoriB)
  return(datori)
}

get_stim_answers <- function(cfg, df) {
  dat_ans <- df[, 14:24]
  df_ans2 <- dat_ans %>%
    mutate(Example = str_sub(Eg, 1, 1))
  df_ans3 <- dat_ans[, -1]
  df_ans4 <- df_ans3 %>%
    mutate(across(everything(), ~ str_sub(., 1, -2)))
  df_lng <- df_ans4 |>
    mutate(id = 1:NROW(df_ans4)) |>
    relocate(id) |>
    pivot_longer(cols = -1)
  df_lng2 <- df_lng |>
    separate_wider_delim(
      value,
      delim = ';',
      names = c("Pref1", "Pref2", "Pref3", "Pref4", "Pref5"),
      too_few = "align_start"
    )  |>
    drop_na() |>
    rename(Question = name) |>
    pivot_longer(cols = 3:7) |>
    mutate(response = str_sub(value, 10))
  return(
    list(
      dat_ans = dat_ans,
      df_ans2 = df_ans2,
      df_ans3 = df_ans3,
      df_ans4 = df_ans4,
      df_lng = df_lng,
      df_lng2 = df_lng2
    )
  )
}

convert_to_pairings <- function(df_lng2) {
  Qnames <- data.frame(
    QA = c("Q1", "Q2", "Q3", "Q4", "Q5"),
    QB = c("Q1B", "Q2B", "Q3B", "Q4B", "Q5B")
  )

  max_persons <- max(df_lng2$id)
  Question_results <- list()
  for (question in 1:5) {
    QA <- Qnames[question, 1]
    QB <- Qnames[question, 2]

    Q_results <- matrix(0, nrow = 5, ncol = 5)
    for (thisid in 1:max_persons) {
      QAdf <- df_lng2 |> filter(id == thisid & Question == QA)  |>
        rename(responseA = response) |>
        select(-Question, -value)
      QBdf <- df_lng2 |> filter(id == thisid & Question == QB)  |>
        rename(responseB = response) |>
        select(-Question, -value)

      QABdf <- full_join(QAdf, QBdf)

      # check if the columns are not NA
      if (sum(is.na(QABdf$responseA)) == 0 &
          sum(is.na(QABdf$responseB)) == 0) {
        result_matrix <- create_permutation_matrix(QABdf$responseA, QABdf$responseB)
      }


      Q_results <- Q_results + result_matrix
    }
    Question_results[[question]] <- Q_results
  }
  return(Question_results)

}

create_permutation_matrix <- function(responseA, responseB) {
  # Get unique items (assuming same items in both columns)
  items <- unique(c(responseA, responseB))
  n <- length(items)

  # Initialize n x n matrix with zeros
  perm_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(perm_matrix) <- items
  colnames(perm_matrix) <- items

  # For each pair of responses
  for (i in 1:length(responseA)) {
    from_item <- responseA[i]
    to_item <- responseB[i]

    # Find positions in the sorted order
    from_pos <- which(items == from_item)
    to_pos <- which(items == to_item)

    # Increment the matrix entry
    perm_matrix[from_pos, to_pos] <- perm_matrix[from_pos, to_pos] + 1
  }

  colnames(perm_matrix) <- rownames(perm_matrix) <- c("Pref1", "Pref2", "Pref3", "Pref4", "Pref5")
  return(perm_matrix)
}

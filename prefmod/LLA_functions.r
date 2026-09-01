#----------------------------------------------------------------------------------------
# Functions required for wrangling data and plotting
#
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------#
# ranking_to_ranks : Extracting rank values
#----------------------------------------------------------------------------------------#
ranking_to_ranks <- function(df, col) {
    # df  = data frame housing the ranking preferences
    # col = column

  parse_ranks <- function(s) {
    s <- gsub("\u00a0", " ", s)  # handle non-breaking spaces
    letters_vec <- regmatches(s, gregexpr("(?<=Region - )[A-Z]", s, perl = TRUE))[[1]]
    n <- length(letters_vec)
    # Position 1 = rank 1, position 2 = rank 2, etc.
    ranks <- seq_len(n)
    setNames(ranks, letters_vec)
  }

  rank_matrix <- do.call(rbind, lapply(df[[col]], function(s) {
    r <- parse_ranks(s)
    r[c("A", "B", "C", "D", "E")]  # enforce column order
  }))

  rank_df <- as.data.frame(rank_matrix)
  bind_cols(df, rank_df)
}

#----------------------------------------------------------------------------------------#
# LRtest_prefmod: Liklihood ratio test between two models
#----------------------------------------------------------------------------------------#

LRtest_prefmod <- function(mod0, mod1){
    
    # mod0 = prefmod model object 0
    # mod1 = prefmod model object 1
    
    LR  <- 2 * (mod1$ll - mod0$ll)
    df  <- length(mod1$coefficients) - length(mod0$coefficients)
    pval <- pchisq(LR, df, lower.tail = FALSE)

    list(LR = LR, df = df, pval = pval)

}

#----------------------------------------------------------------------------------------#
# extract_pref_data: Extract data for log-linear analysis using prefmod R package
#----------------------------------------------------------------------------------------#

extract_pref_data <- function(i, df_ans6, Questions1, Questions2, GBR = FALSE){

    # i          = which map year
    # df_ans6    = data frame of preferences
    # Questions1 = questions from survey A
    # Questions2 = questions from survey B


    df_ans7 <- df_ans6 |>
       select(Age,
         Gender,
         Education,
         Occupation,
         Industry,
         GBF_Familiarity,
         exp,
         Questions1[i]) |>
       mutate(Q = !!sym(Questions1[i]), Uncert = "2") |>
       select(-Questions1[i])

    df_ans8 <- df_ans6 |>
       select(Age,
         Gender,
         Education,
         Occupation,
         Industry,
         GBF_Familiarity,
         exp,
         Questions2[i]) |>
       mutate(Q = !!sym(Questions2[i]), Uncert = "1") |>
       select(-Questions2[i])

    df_ans <- bind_rows(df_ans7, df_ans8)

    out <- ranking_to_ranks(df_ans, "Q")
  
  

    if(GBR){
     out2 <- out |>
       mutate(GBF = ifelse(GBF_Familiarity =="Yes", 2, 1)) |>
          select(A, B, C, D, E, Uncert, GBF)
       out2$GBF <- as.factor(out2$GBF)
    }
  else{
      out2 <- out|>
       select(A, B, C, D, E, Uncert)
      out2$Uncert <- as.factor(out2$Uncert)
    }
    
  
  out2
}


#----------------------------------------------------------------------------------------#
# plot_wmat: plotting worth estimates (R base package)
#            grabbed from the prefmod package because some aesetics were hard wired in
#----------------------------------------------------------------------------------------#
plot_wmat <- function (x, main = "Preferences", ylab = "Estimate", psymb = NULL, 
    pcol = NULL, ylim = range(worthmat), log = "", pcex = 1, tcex = 1, ...) 
{
   # x     = worth estimates (df/tibble)
   # main  = title
   # ylab  = label for y-axis
   # psymb = point symbol
   # pcol  = colours for points
   # ylim  = limits for y-axis
   # log   = plot on log-scale
   # pcex  = size of points
   # tcex  = size of text
   # ...   = other arguments passed to base plotting function

    worthmat <- x
    coeff <- unclass(worthmat)
    objnames <- rownames(coeff)
    grnames <- colnames(coeff)
    if (length(grep(":", grnames))) {
        gr.lines <- unlist(strsplit(grnames[1], ":"))
        ngrlin <- length(gr.lines) - 0.5
        grnames <- gsub(":", "\n", grnames)
    }
    else ngrlin <- 1
    nobj <- dim(coeff)[1]
    ngroups <- dim(coeff)[2]
    if (ngroups == 1) 
        colnames(coeff) <- ""
    if (is.null(psymb)) 
        psymb <- rep(16, nobj)
    if (is.null(pcol)) 
        farbe <- rainbow_hcl(nobj)
    else if (length(pcol) > 1) 
        farbe <- pcol
    else if (pcol == "black") 
        farbe <- "black"
    else if (pcol %in% c("heat", "terrain")) 
        farbe <- eval(call(paste(pcol, "_hcl", sep = "", collapse = ""), 
            nobj))
    else if (pcol == "gray") 
        farbe <- eval(call(paste(pcol, ".colors", sep = "", collapse = ""), 
            nobj))
    else farbe <- rainbow_hcl(nobj)
    pomi <- c(0.2, 0.2, 0.5, 0.2)
        par(omi = pomi, mar = c(3, 6, 0.1, 0))
    plot(c(0.5, ngroups + 0.5), c(min(coeff), max(coeff)), type = "n", 
        axes = FALSE, xlab = "", ylab = ylab, ylim = ylim, log = log, 
        ...)
    title(main, outer = TRUE)
    box()
    axis(2)
    axis(1, at = 1:ngroups, labels = grnames, mgp = c(3, ngrlin, 
        0))
    adj <- 1/nchar(as.character(objnames))
    adj <- strwidth(as.character(objnames))/2
    d <- strwidth("d")
    adj <- adj + d
    for (i in 1:ngroups) {
        pm <- rep(c(1, -1), nobj)
        pm <- pm[1:nobj]
        o <- order(coeff[1:nobj, i])
        sadj <- adj[o]
        sobjnames <- objnames[o]
        spsymb <- psymb[o]
        sfarbe <- farbe[o]
        scoeff <- sort(coeff[, i])
        x <- rep(i, nobj)
        xy <- xy.coords(x, scoeff)
        lines(c(i, i), range(scoeff), ...)
        points(xy, pch = spsymb, col = sfarbe, cex = pcex)
        text(x + sadj * pm, scoeff, sobjnames, cex = tcex)
    }
}


#----------------------------------------------------------------------------------------#
# plot_wmat_gg: plotting worth estimates (ggplot package)
#            This function plots worth estimates differently to base version, and connecxts
#            the worth estimates from each factor plotted.
#----------------------------------------------------------------------------------------#
plot_wmat_gg <- function(x, main = "Preferences", ylab = "Estimate",
                          pcol = NULL, pcex = 3, tcex = 3.5, lwd = 0.8,
                          log_y = FALSE, label_pos = c("right", "left", "both"),
                          nudge = 0.15,
                          x_pad = 0.25,        
                          y_range = NULL,      
                          axis_title_size = 14,   
                          axis_text_size  = 12,   
                          main_size       = 16)  {
  
   # x               = worth estimates (df/tibble)
   # main            = title
   # ylab            = label for y-axis
   # pcol            = colours for points
   # pcex            = size of points
   # tcex            = size of text
   # lwd             = line width
   # log_y           = plot on log-scale (TRUE/FALSE)
   # label_pos       = label position
   # nudge           = affects placement of labels
   # x_pad           = controls left/right white space
   # y_range         = limits for y-axis
   # axis_title_size = size of y-axis title
   # axis_text_size  = size of text
   # main_size       = size of plot title  
  

  label_pos <- match.arg(label_pos)

  coeff <- unclass(x)
  objnames <- rownames(coeff)
  grnames  <- colnames(coeff)
  ngroups  <- length(grnames)

  df <- as.data.frame(coeff) %>%
    rownames_to_column("object") %>%
    pivot_longer(-object, names_to = "factor", values_to = "estimate") %>%
    mutate(
      object    = factor(object, levels = objnames),
      factor    = factor(factor, levels = grnames),
      x_numeric = as.numeric(factor)
    )

  if (is.null(pcol)) pcol <- scales::hue_pal()(length(objnames))
  names(pcol) <- objnames

  label_df <- bind_rows(
    if (label_pos %in% c("right", "both"))
      df %>% filter(x_numeric == ngroups) %>% mutate(hj = 0, x_lab = x_numeric + nudge),
    if (label_pos %in% c("left", "both"))
      df %>% filter(x_numeric == 1) %>% mutate(hj = 1, x_lab = x_numeric - nudge)
  )

  
  p <- ggplot(df, aes(x = x_numeric, y = estimate, group = object, colour = object)) +
    geom_line(linewidth = lwd) +
    geom_point(size = pcex) +
    geom_text(
      data = label_df,
      aes(x = x_lab, label = object, hjust = hj),
      size = tcex, show.legend = FALSE, fontface = "bold"
    ) +
    scale_colour_manual(values = pcol, guide = "none") +
    scale_x_continuous(
      breaks = 1:ngroups, labels = grnames,
      limits = c(1 - nudge - x_pad, ngroups + nudge + x_pad)
    ) +
    labs(title = main, x = NULL, y = ylab) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(hjust = 0.5, size = main_size, fontface = "bold"),
      panel.grid.minor = element_blank(),
      axis.title.y     = element_text(size = axis_title_size, margin = margin(r = 20)),
      axis.text.x      = element_text(size = axis_text_size),
      axis.text.y      = element_text(size = axis_text_size)
    )

  if (log_y) p <- p + scale_y_log10()

  p <- p + coord_cartesian(ylim = y_range) 
  p
}

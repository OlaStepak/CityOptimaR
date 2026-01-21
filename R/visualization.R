#' Plot Fuzzy VIKOR Results
#'
#' @description Visualizes the VIKOR results using a bubble chart.
#' The X-axis represents Group Utility (S), the Y-axis represents Individual Regret (R).
#' Bubble size is inversely proportional to the Compromise Solution (Q) - smaller Q (better rank) = larger bubble.
#'
#' @param x An object of class `fuzzy_vikor_res` returned by the `fuzzy_vikor()` function.
#' @param ... Additional arguments.
#' @return A ggplot object.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.fuzzy_vikor_res <- function(x, ...) {

  df <- x$results

  # Prepare data for plotting
  df$Label <- paste("Alt", df$Alternative)

  # Categorize by ranking: Top 3 vs Others
  df$Group <- ifelse(df$Ranking <= 3, "Top 3", "Others")

  # Size Logic: We want Better Rank (Lower Q) to look significant.
  # Let's use inverted ranking for size or normalized inverted Q.
  # Simple visual approach: Size = (MaxRank + 1) - Rank
  max_rank <- max(df$Ranking)
  df$PlotSize <- (max_rank + 1) - df$Ranking

  s_mean <- mean(df$Def_S, na.rm = TRUE)
  r_mean <- mean(df$Def_R, na.rm = TRUE)

  # Calculate Plot Limits with padding
  x_range <- range(df$Def_S)
  y_range <- range(df$Def_R)

  p <- ggplot(df, aes(x = Def_S, y = Def_R)) +
    # Quadrant lines
    geom_vline(xintercept = s_mean, linetype = "dashed", color = "grey60") +
    geom_hline(yintercept = r_mean, linetype = "dashed", color = "grey60") +

    # Bubbles
    geom_point(aes(size = PlotSize, fill = Group),
               shape = 21,
               color = "black",
               stroke = 0.8,
               alpha = 0.85) +

    # Text Labels
    geom_text_repel(aes(label = Label),
                    size = 3.5,
                    box.padding = 0.5,
                    point.padding = 0.3,
                    max.overlaps = 20) +

    scale_fill_manual(values = c("Top 3" = "#4CAF50", "Others" = "#E0E0E0")) +
    scale_size_continuous(range = c(4, 12), name = "Rank Strength") +

    # VIKOR Logic: Ideal is (0,0) or bottom-left.
    # Usually we leave axes as is, but users should know Lower is Better.

    labs(
      title = "Fuzzy VIKOR Compromise Analysis",
      subtitle = "X: Group Utility (S), Y: Individual Regret (R). Lower values are better.\nGreen bubbles indicate top-ranked alternatives.",
      x = "Group Utility (Defuzzified S)",
      y = "Individual Regret (Defuzzified R)",
      fill = "Rank Group"
    ) +

    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )

  return(p)
}

# Fix for R CMD check global variable warnings
utils::globalVariables(c("Def_S", "Def_R", "PlotSize", "Group", "Label", "Alternative"))

# ============================================================
# MULTIMOORA & PROMETHEE PLOTS
# ============================================================

#' Mapa Strategiczna MULTIMOORA
#' @export
plot.rozmyty_multimoora_wynik <- function(x, ...) {
  df <- x$results
  df$Strength <- (max(df$Final_Ranking) - df$Final_Ranking + 1)^2

  ggplot2::ggplot(df, ggplot2::aes(x = RS_Score, y = RP_Score)) +
    ggplot2::annotate("rect",
                      xmin = median(df$RS_Score),
                      xmax = Inf,
                      ymin = -Inf,
                      ymax = median(df$RP_Score),
                      fill = "#E8F5E9",
                      alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(size = Strength,
                                     fill = as.factor(Final_Ranking)),
                        shape = 21,
                        color = "black") +
    ggrepel::geom_text_repel(ggplot2::aes(label = paste0("Alt ", Alternative))) +
    ggplot2::labs(title = "Mapa MULTIMOORA",
                  x = "System Ilorazowy (Max)",
                  y = "Punkt Odniesienia (Min)",
                  fill = "Ranking",
                  size = "Siła") +
    ggplot2::theme_minimal()
}
#' Wykres Przepływów PROMETHEE II
#' @export
plot.rozmyty_promethee_wynik <- function(x, ...) {
  df <- x$results
  df <- df[order(df$Phi_Net), ]
  df$Alt <- factor(paste0("Alt ", df$Alternative),
                   levels = paste0("Alt ", df$Alternative))

  ggplot2::ggplot(df, ggplot2::aes(x = Alt, y = Phi_Net)) +
    ggplot2::geom_segment(ggplot2::aes(xend = Alt, y = 0, yend = Phi_Net),
                          color = "grey") +
    ggplot2::geom_point(ggplot2::aes(fill = Phi_Net),
                        size = 5,
                        shape = 21) +
    ggplot2::coord_flip()
    ggplot2::scale_fill_gradient2(low = "red",
                                  mid = "white",
                                  high = "green",
                                  midpoint = 0) +
    ggplot2::labs(title = "PROMETHEE II Ranking",
                  y = "Przepływ Netto (Phi)",
                  x = "Alternatywa",
                  fill = "Phi Net") +
    ggplot2::theme_minimal()
}

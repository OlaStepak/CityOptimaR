#' Plot Fuzzy VIKOR Results
#'
#' @description Wizualizuje wyniki metody Fuzzy VIKOR jako wykres babelkowy.
#' Os X reprezentuje uzytecznosc grupowa (S), os Y indywidualny zal (R).
#' Rozmiar babla jest odwrotnie proporcjonalny do wskaznika kompromisu Q -
#' im mniejsze Q (lepszy ranking), tym wiekszy babel.
#' @param x Obiekt klasy fuzzy_vikor_res zwrocony przez funkcje fuzzy_vikor().
#' @param ... Dodatkowe argumenty.
#' @return Obiekt ggplot.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.fuzzy_vikor_res <- function(x, ...) {
  df <- x$results
  df$Label <- paste('Alt', df$Alternative)
  df$Group <- ifelse(df$Ranking <= 3, 'Top 3', 'Others')
  max_rank <- max(df$Ranking)
  df$PlotSize <- (max_rank + 1) - df$Ranking
  s_mean <- mean(df$Def_S, na.rm = TRUE)
  r_mean <- mean(df$Def_R, na.rm = TRUE)

  ggplot(df, aes(x = Def_S, y = Def_R)) +
    geom_vline(xintercept = s_mean, linetype = 'dashed', color = 'grey60') +
    geom_hline(yintercept = r_mean, linetype = 'dashed', color = 'grey60') +
    geom_point(aes(size = PlotSize, fill = Group),
               shape = 21, color = 'black', stroke = 0.8, alpha = 0.85) +
    geom_text_repel(aes(label = Label),
                    size = 3.5, box.padding = 0.5,
                    point.padding = 0.3, max.overlaps = 20) +
    scale_fill_manual(values = c('Top 3' = '#4CAF50', 'Others' = '#E0E0E0')) +
    scale_size_continuous(range = c(4, 12), name = 'Rank Strength') +
    labs(
      title = 'Fuzzy VIKOR Compromise Analysis',
      subtitle = 'X: Group Utility (S), Y: Individual Regret (R). Nizsze wartosci sa lepsze.',
      x = 'Group Utility (Defuzzified S)',
      y = 'Individual Regret (Defuzzified R)',
      fill = 'Rank Group'
    ) +
    theme_minimal() +
    theme(
      legend.position = 'right',
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = 'black'),
      plot.title = element_text(face = 'bold', size = 14),
      plot.subtitle = element_text(size = 10, color = 'grey30')
    )
}

utils::globalVariables(c('Def_S', 'Def_R', 'PlotSize', 'Group', 'Label', 'Alternative'))

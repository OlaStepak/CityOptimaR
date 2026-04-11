#' @importFrom stats cor
# ============================================================
# META-RANKING (3 METODY: TOPSIS, VIKOR, WASPAS)
# ============================================================

#' @title Teoria Dominacji dla Rankingu
#' @keywords internal
calculate_dominance_ranking <- function(rank_mat) {
  n <- nrow(rank_mat)
  final_rank <- rep(0, n)
  available <- rep(TRUE, n)

  for (current_position in 1:n) {
    current_mat <- rank_mat
    current_mat[!available, ] <- Inf
    candidates <- apply(current_mat, 2, which.min)
    freq_table <- table(candidates)
    max_votes <- max(freq_table)
    winners <- as.numeric(names(freq_table)[freq_table == max_votes])
    if (length(winners) == 1) {
      winner_idx <- winners
    } else {
      sums <- rowSums(rank_mat[winners, , drop = FALSE])
      winner_idx <- winners[which.min(sums)]
    }
    final_rank[winner_idx] <- current_position
    available[winner_idx] <- FALSE
  }
  return(final_rank)
}

#' @title Rozmyty Meta-Ranking
#' @description Agreguje wyniki trzech metod MCDA: Fuzzy TOPSIS (metoda referencyjna),
#' Fuzzy VIKOR oraz Fuzzy WASPAS. TOPSIS stanowi glowna metode rankingowa pakietu
#' CityOptimaR ze wzgledu na intuicyjnosc i odpornosc w problemach wyboru miasta.
#' @param decision_mat Rozmyta macierz decyzyjna (m x 3n).
#' @param criteria_types Wektor typow kryteriow ("min" lub "max").
#' @param weights Wektor wag (opcjonalny).
#' @param bwm_best Wektor BWM dla najlepszego kryterium.
#' @param bwm_worst Wektor BWM dla najgorszego kryterium.
#' @param lambda Parametr WASPAS (domyslnie 0.5).
#' @param v Parametr VIKOR (domyslnie 0.5).
#' @return Lista z comparison, topsis_ranking i correlations.
#' @importFrom stats cor aggregate
#' @export
fuzzy_meta_ranking <- function(decision_mat,
                               criteria_types,
                               weights = NULL,
                               bwm_best = NULL,
                               bwm_worst = NULL,
                               lambda = 0.5,
                               v = 0.5) {

  if (is.null(weights) && (is.null(bwm_best) || is.null(bwm_worst))) {
    message("Brak wag. Obliczam wagi metoda Entropii Shannona...")
    weights <- .calculate_entropy_weights(decision_mat)
  }

  args_base <- list(
    decision_mat = decision_mat,
    criteria_types = criteria_types
  )
  if (!is.null(weights)) args_base$weights <- weights
  if (!is.null(bwm_best)) {
    args_base$bwm_best <- bwm_best
    args_base$bwm_worst <- bwm_worst
  }

  res_topsis <- do.call(fuzzy_topsis, args_base)
  res_vikor  <- do.call(fuzzy_vikor,  c(args_base, list(v = v)))
  res_waspas <- do.call(fuzzy_waspas, c(args_base, list(lambda = lambda)))

  rank_matrix <- cbind(
    TOPSIS = res_topsis$Ranking,
    VIKOR  = res_vikor$results$Ranking,
    WASPAS = res_waspas$Ranking
  )

  rank_sum <- rank(rowSums(rank_matrix), ties.method = "first")
  rank_dom <- calculate_dominance_ranking(rank_matrix)

  comp_df <- data.frame(
    Alternative    = rownames(decision_mat),
    R_TOPSIS       = rank_matrix[, "TOPSIS"],
    R_VIKOR        = rank_matrix[, "VIKOR"],
    R_WASPAS       = rank_matrix[, "WASPAS"],
    Meta_Suma      = rank_sum,
    Meta_Dominacja = rank_dom
  )

  return(list(
    comparison     = comp_df,
    topsis_ranking = res_topsis,
    correlations   = cor(rank_matrix, method = "spearman")
  ))
}


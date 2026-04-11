#' @title Fuzzy VIKOR Method
#' @description Fuzzy VIKOR (VIseKriterijumska Optimizacija I Kompromisno Resenje).
#' Metoda koncentruje sie na wyborze rozwiazania kompromisowego, ktore maksymalizuje
#' uzytecznosc grupowa przy jednoczesnym minimalizowaniu indywidualnego zalu decydenta.
#' W pakiecie CityOptimaR pelni role metody weryfikujacej wyniki Fuzzy TOPSIS.
#' @param decision_mat Matrix of TFN values (n x 3m format)
#' @param criteria_types Vector of 'max'/'min' indicating benefit/cost criteria
#' @param v Weight of the strategy of maximum group utility (default 0.5)
#' @param weights Optional numeric vector of fuzzy weights (length = 3 * number of criteria)
#' @param bwm_criteria Optional criteria names for BWM weight calculation
#' @param bwm_best Optional Best-to-Others vector for BWM
#' @param bwm_worst Optional Others-to-Worst vector for BWM
#' @return Obiekt klasy fuzzy_vikor_res z wynikami rankingu.
#' @export
fuzzy_vikor <- function(decision_mat,
                        criteria_types,
                        v = 0.5,
                        weights,
                        bwm_criteria,
                        bwm_best,
                        bwm_worst) {

  if (!is.matrix(decision_mat))
    stop("'decision_mat' must be a matrix.")

  final_w <- .get_final_weights(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst)

  n_cols <- ncol(decision_mat)
  fuzzy_cb <- rep(criteria_types, each = 3)

  pos <- ifelse(fuzzy_cb == 'max',
                apply(decision_mat, 2, max),
                apply(decision_mat, 2, min))

  neg <- ifelse(fuzzy_cb == 'min',
                apply(decision_mat, 2, max),
                apply(decision_mat, 2, min))

  D <- abs(decision_mat - matrix(pos, nrow(decision_mat), n_cols, TRUE)) /
    abs(matrix(pos - neg, nrow(decision_mat), n_cols, TRUE))

  WD <- D %*% diag(final_w)

  S <- cbind(
    rowSums(WD[, seq(1, n_cols, 3)]),
    rowSums(WD[, seq(2, n_cols, 3)]),
    rowSums(WD[, seq(3, n_cols, 3)])
  )

  R <- cbind(
    apply(WD[, seq(1, n_cols, 3)], 1, max),
    apply(WD[, seq(2, n_cols, 3)], 1, max),
    apply(WD[, seq(3, n_cols, 3)], 1, max)
  )

  S_star <- min(S[,1]); S_minus <- max(S[,3])
  R_star <- min(R[,1]); R_minus <- max(R[,3])

  Q <- v * (S - S_star)/(S_minus - S_star) +
    (1 - v) * (R - R_star)/(R_minus - R_star)

  results_df <- data.frame(
    Alternative = seq_len(nrow(decision_mat)),
    Def_S = rowMeans(S),
    Def_R = rowMeans(R),
    Q = rowMeans(Q),
    Ranking = rank(rowMeans(Q), ties.method = 'first')
  )

  result <- list(results = results_df, method = 'VIKOR')
  class(result) <- 'fuzzy_vikor_res'
  return(result)
}

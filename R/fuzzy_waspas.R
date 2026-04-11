#' @title Fuzzy WASPAS Method
#' @description Fuzzy WASPAS (Weighted Aggregated Sum Product Assessment).
#' Metoda laczy dwa podejscia rankingowe: sumowanie i mnozenie wazonych ocen.
#' W pakiecie CityOptimaR pelni role metody weryfikujacej wyniki Fuzzy TOPSIS.
#' @param decision_mat Matrix of TFN values (n x 3m)
#' @param criteria_types Vector of 'max'/'min'
#' @param weights Optional numeric vector of weights (length = number of criteria)
#' @param lambda Weighting coefficient (default 0.5)
#' @param bwm_criteria Optional criteria names for BWM
#' @param bwm_best Optional Best-to-Others vector
#' @param bwm_worst Optional Others-to-Worst vector
#' @return Data frame with WASPAS scores and ranking
#' @export
fuzzy_waspas <- function(decision_mat,
                         criteria_types,
                         weights = NULL,
                         lambda = 0.5,
                         bwm_criteria = NULL,
                         bwm_best = NULL,
                         bwm_worst = NULL) {

  if (!is.matrix(decision_mat))
    stop("'decision_mat' must be a matrix.")

  n_alt <- nrow(decision_mat)
  n_crit <- length(criteria_types)

  defuzz <- matrix(NA, n_alt, n_crit)
  for (j in seq_len(n_crit)) {
    cols <- ((j - 1) * 3 + 1):(j * 3)
    defuzz[, j] <- rowMeans(decision_mat[, cols])
  }

  if (is.null(weights)) {
    if (!is.null(bwm_criteria) && !is.null(bwm_best) && !is.null(bwm_worst)) {
      bwm <- calculate_bwm_weights(
        criteria_names = bwm_criteria,
        best_to_others = bwm_best,
        others_to_worst = bwm_worst
      )
      w <- bwm$criteriaWeights
    } else {
      m <- nrow(defuzz)
      P <- defuzz / rowSums(defuzz)
      P[P == 0] <- 1e-12
      e <- -colSums(P * log(P)) / log(m)
      d <- 1 - e
      w <- d / sum(d)
    }
  } else {
    w <- weights
  }

  w <- w / sum(w)

  norm <- matrix(NA, n_alt, n_crit)
  for (j in seq_len(n_crit)) {
    if (criteria_types[j] == 'max') {
      norm[, j] <- defuzz[, j] / max(defuzz[, j])
    } else {
      norm[, j] <- min(defuzz[, j]) / defuzz[, j]
    }
  }

  wsm <- norm %*% w
  wpm <- apply(norm, 1, function(x) prod(x ^ w))
  Q <- lambda * wsm + (1 - lambda) * wpm

  data.frame(
    Alternative = seq_len(n_alt),
    Score = as.numeric(Q),
    Ranking = rank(-Q, ties.method = 'min')
  )
}

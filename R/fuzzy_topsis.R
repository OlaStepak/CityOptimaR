#' @title Fuzzy TOPSIS Method with Acceptance Threshold
#' @description Fuzzy TOPSIS (Technique for Order Preference by Similarity to Ideal Solution).
#' Glowna metoda rankingowa pakietu CityOptimaR. W porownaniu do klasycznej implementacji,
#' metoda zostala rozszerzona o mechanizm progow akceptowalnosci (acceptance thresholds).
#' Miasta ktore nie spelniaja minimalnego progu w krytycznym kryterium (np. bezpieczenstwo)
#' sa automatycznie wykluczone z rankingu i oznaczone jako 'unacceptable'.
#' Rozszerzenie to jest szczegolnie uzasadnione w kontekscie wyboru miasta do zamieszkania,
#' gdzie pewne kryteria maja charakter warunkow koniecznych, a nie tylko pozadanych.
#' @param decision_mat Matrix of TFN values (n x 3m format)
#' @param criteria_types Vector of 'max'/'min' indicating benefit/cost criteria
#' @param weights Optional numeric vector of fuzzy weights (length = 3 * number of criteria)
#' @param bwm_criteria Optional criteria names for BWM weight calculation
#' @param bwm_best Optional Best-to-Others vector for BWM
#' @param bwm_worst Optional Others-to-Worst vector for BWM
#' @param thresholds Opcjonalny nazwany wektor minimalnych progow akceptowalnosci
#'   dla kryteriow (w skali Saaty 1-9). Np. c(Safety = 3, Healthcare = 2).
#'   Miasta ponizej progu otrzymuja ranking NA i status 'unacceptable'.
#' @return Data frame with columns: Alternative, Score, Ranking, Status
#' @export
fuzzy_topsis <- function(decision_mat,
                         criteria_types,
                         weights,
                         bwm_criteria,
                         bwm_best,
                         bwm_worst,
                         thresholds = NULL) {

  if (!is.matrix(decision_mat))
    stop("'decision_mat' must be a matrix.")

  final_w <- .get_final_weights(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst)

  n_cols <- ncol(decision_mat)
  n_crit <- n_cols / 3
  fuzzy_cb <- rep(criteria_types, each = 3)

  acceptable <- rep(TRUE, nrow(decision_mat))
  if (!is.null(thresholds) && !is.null(names(thresholds))) {
    crit_names <- attr(decision_mat, 'criterion_names')
    for (crit in names(thresholds)) {
      idx <- which(crit_names == crit)
      if (length(idx) == 1) {
        col_m <- (idx - 1) * 3 + 2
        acceptable <- acceptable & (decision_mat[, col_m] >= thresholds[crit])
      }
    }
  }

  denoms <- sqrt(colSums(decision_mat^2))
  norm_mat <- decision_mat / matrix(denoms, nrow(decision_mat), n_cols, TRUE)
  weighted_mat <- norm_mat %*% diag(final_w)

  pos_ideal <- ifelse(fuzzy_cb == 'max',
                      apply(weighted_mat, 2, max),
                      apply(weighted_mat, 2, min))

  neg_ideal <- ifelse(fuzzy_cb == 'min',
                      apply(weighted_mat, 2, max),
                      apply(weighted_mat, 2, min))

  d_pos <- (weighted_mat - matrix(pos_ideal, nrow(decision_mat), n_cols, TRUE))^2
  d_neg <- (weighted_mat - matrix(neg_ideal, nrow(decision_mat), n_cols, TRUE))^2

  d_pos_f <- cbind(
    sqrt(rowSums(d_pos[, seq(1, n_cols, 3)])),
    sqrt(rowSums(d_pos[, seq(2, n_cols, 3)])),
    sqrt(rowSums(d_pos[, seq(3, n_cols, 3)]))
  )

  d_neg_f <- cbind(
    sqrt(rowSums(d_neg[, seq(1, n_cols, 3)])),
    sqrt(rowSums(d_neg[, seq(2, n_cols, 3)])),
    sqrt(rowSums(d_neg[, seq(3, n_cols, 3)]))
  )

  denom <- d_pos_f + d_neg_f

  R_f <- cbind(
    d_neg_f[,1] / denom[,3],
    d_neg_f[,2] / denom[,2],
    d_neg_f[,3] / denom[,1]
  )

  score <- (R_f[,1] + 4 * R_f[,2] + R_f[,3]) / 6
  score[!acceptable] <- NA

  ranking <- rep(NA_integer_, length(score))
  ranking[acceptable] <- rank(-score[acceptable], ties.method = 'first')

  data.frame(
    Alternative = seq_len(nrow(decision_mat)),
    Score = score,
    Ranking = ranking,
    Status = ifelse(acceptable, 'acceptable', 'unacceptable')
  )
}

#' @title Entropy-based Objective Weights (Fuzzy)
#' @description Calculates objective fuzzy weights using Shannon entropy.
#' @keywords internal
.calculate_entropy_weights <- function(decision_mat) {
  n_crit <- ncol(decision_mat) / 3
  m <- nrow(decision_mat)
  crisp_mat <- matrix(0, m, n_crit)
  k <- 1
  for (j in seq(1, ncol(decision_mat), 3)) {
    crisp_mat[, k] <- rowMeans(decision_mat[, j:(j + 2)])
    k <- k + 1
  }
  P <- crisp_mat / rowSums(crisp_mat)
  P[P == 0] <- 1e-12
  entropy <- -colSums(P * log(P)) / log(m)
  d <- 1 - entropy
  w <- d / sum(d)
  rep(w, each = 3)
}

#' @title Process Weights for Fuzzy MCDM
#' @description Priority: explicit weights -> BWM -> entropy
#' @keywords internal
.get_final_weights <- function(decision_mat,
                               weights,
                               bwm_criteria,
                               bwm_best,
                               bwm_worst) {
  n_crit <- ncol(decision_mat) / 3
  if (!missing(weights)) {
    if (length(weights) != ncol(decision_mat))
      stop("Length of 'weights' must be equal to 3 * number of criteria.")
    return(weights)
  }
  if (!missing(bwm_criteria) && !missing(bwm_best) && !missing(bwm_worst)) {
    bwm_res <- calculate_bwm_weights(bwm_criteria, bwm_best, bwm_worst)
    crisp_w <- bwm_res$criteriaWeights
    if (length(crisp_w) != n_crit)
      stop("BWM weights do not match number of criteria.")
    return(rep(crisp_w, each = 3))
  }
  .calculate_entropy_weights(decision_mat)
}

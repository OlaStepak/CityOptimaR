# ============================================================
# Internal helpers
# ============================================================

#' @title Entropy-based Objective Weights (Fuzzy)
#' @description Calculates objective fuzzy weights using Shannon entropy.
#' @keywords internal
.calculate_entropy_weights <- function(decision_mat) {

  n_crit <- ncol(decision_mat) / 3
  m <- nrow(decision_mat)

  # Defuzzification (mean of TFN)
  crisp_mat <- matrix(0, m, n_crit)
  k <- 1
  for (j in seq(1, ncol(decision_mat), 3)) {
    crisp_mat[, k] <- rowMeans(decision_mat[, j:(j + 2)])
    k <- k + 1
  }

  # Normalization
  P <- crisp_mat / rowSums(crisp_mat)
  P[P == 0] <- 1e-12

  # Shannon entropy
  entropy <- -colSums(P * log(P)) / log(m)
  d <- 1 - entropy
  w <- d / sum(d)

  # Convert to fuzzy weights
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

  # 1 Explicit fuzzy weights
  if (!missing(weights)) {
    if (length(weights) != ncol(decision_mat)) {
      stop("Length of 'weights' must be equal to 3 * number of criteria.")
    }
    return(weights)
  }

  # 2 BWM
  if (!missing(bwm_criteria) &&
      !missing(bwm_best) &&
      !missing(bwm_worst)) {

    message("Weights not provided. Calculating using BWM...")
    bwm_res <- calculate_bwm_weights(
      bwm_criteria,
      bwm_best,
      bwm_worst
    )

    crisp_w <- bwm_res$criteriaWeights

    if (length(crisp_w) != n_crit) {
      stop("BWM weights do not match number of criteria.")
    }

    return(rep(crisp_w, each = 3))
  }

  # 3 Entropy fallback
  message("Weights not provided. Calculating objective weights using Shannon entropy...")
  .calculate_entropy_weights(decision_mat)
}

# ============================================================
# FUZZY TOPSIS
# ============================================================

#' @title Fuzzy TOPSIS Method
#' @description Fuzzy TOPSIS (Technique for Order Preference by Similarity to Ideal Solution)
#' @param decision_mat Matrix of TFN values (n x 3m format)
#' @param criteria_types Vector of "max"/"min" indicating benefit/cost criteria
#' @param weights Optional numeric vector of fuzzy weights (length = 3 * number of criteria)
#' @param bwm_criteria Optional criteria names for BWM weight calculation
#' @param bwm_best Optional Best-to-Others vector for BWM
#' @param bwm_worst Optional Others-to-Worst vector for BWM
#' @return Data frame with columns: Alternative, Score, Ranking
#' @export
fuzzy_topsis <- function(decision_mat,
                         criteria_types,
                         weights,
                         bwm_criteria,
                         bwm_best,
                         bwm_worst) {


  if (!is.matrix(decision_mat))
    stop("'decision_mat' must be a matrix.")

  final_w <- .get_final_weights(
    decision_mat,
    weights,
    bwm_criteria,
    bwm_best,
    bwm_worst
  )

  n_cols <- ncol(decision_mat)

  fuzzy_cb <- rep(criteria_types, each = 3)

  # Vector normalization
  denoms <- sqrt(colSums(decision_mat^2))
  norm_mat <- decision_mat / matrix(denoms, nrow(decision_mat), n_cols, TRUE)

  weighted_mat <- norm_mat %*% diag(final_w)

  pos_ideal <- ifelse(
    fuzzy_cb == "max",
    apply(weighted_mat, 2, max),
    apply(weighted_mat, 2, min)
  )

  neg_ideal <- ifelse(
    fuzzy_cb == "min",
    apply(weighted_mat, 2, max),
    apply(weighted_mat, 2, min)
  )

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

  data.frame(
    Alternative = seq_len(nrow(decision_mat)),
    Score = score,
    Ranking = rank(-score, ties.method = "first")
  )
}

# ============================================================
# FUZZY VIKOR
# ============================================================

#' @title Fuzzy VIKOR Method
#' @description Fuzzy VIKOR (VIseKriterijumska Optimizacija I Kompromisno Resenje)
#' @param decision_mat Matrix of TFN values (n x 3m format)
#' @param criteria_types Vector of "max"/"min" indicating benefit/cost criteria
#' @param v Weight of the strategy of maximum group utility (default 0.5)
#' @param weights Optional numeric vector of fuzzy weights (length = 3 * number of criteria)
#' @param bwm_criteria Optional criteria names for BWM weight calculation
#' @param bwm_best Optional Best-to-Others vector for BWM
#' @param bwm_worst Optional Others-to-Worst vector for BWM
#' @return Data frame with columns: Alternative, Q, Ranking
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

  final_w <- .get_final_weights(
    decision_mat,
    weights,
    bwm_criteria,
    bwm_best,
    bwm_worst
  )

  n_cols <- ncol(decision_mat)
  fuzzy_cb <- rep(criteria_types, each = 3)

  pos <- ifelse(fuzzy_cb == "max",
                apply(decision_mat, 2, max),
                apply(decision_mat, 2, min))

  neg <- ifelse(fuzzy_cb == "min",
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

  def_Q <- rowMeans(Q)

  # Defuzzyfikacja S i R (dodajemy do wyniku)
  def_S <- rowMeans(S)
  def_R <- rowMeans(R)

  # Tworzymy ramkę danych z wynikami
  results_df <- data.frame(
    Alternative = seq_len(nrow(decision_mat)),
    Def_S = def_S,
    Def_R = def_R,
    Q = def_Q,
    Ranking = rank(def_Q, ties.method = "first")
  )

  # Tworzymy obiekt z klasą
  result <- list(
    results = results_df,
    method = "VIKOR"
  )

  class(result) <- "fuzzy_vikor_res"
  return(result)
}

# ============================================================
# FUZZY WASPAS
# ============================================================

#' @title Fuzzy WASPAS Method
#' @description Fuzzy WASPAS (Weighted Aggregated Sum Product Assessment)
#' @param decision_mat Matrix of TFN values (n x 3m)
#' @param criteria_types Vector of "max"/"min"
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

  if (!is.matrix(decision_mat)) stop("'decision_mat' must be a matrix.")

  n_alt <- nrow(decision_mat)
  n_crit <- length(criteria_types)

  # --- Defuzzification ---
  defuzz <- matrix(NA, n_alt, n_crit)
  for (j in seq_len(n_crit)) {
    cols <- ((j - 1) * 3 + 1):(j * 3)
    defuzz[, j] <- rowMeans(decision_mat[, cols])
  }

  # --- Weight handling ---
  if (is.null(weights)) {
    if (!is.null(bwm_criteria) &&
        !is.null(bwm_best) &&
        !is.null(bwm_worst)) {

      message("Weights not provided. Calculating using BWM...")
      bwm <- calculate_bwm_weights(
        criteria_names = bwm_criteria,
        best_to_others = bwm_best,
        others_to_worst = bwm_worst
      )
      w <- bwm$criteriaWeights

    } else {
      message("Weights not provided. Calculating objective weights using Shannon entropy...")
      w <- {
        n <- ncol(defuzz)
        m <- nrow(defuzz)
        P <- defuzz / rowSums(defuzz)
        P[P == 0] <- 1e-12
        e <- -colSums(P * log(P)) / log(m)
        d <- 1 - e
        d / sum(d)
      }
    }
  } else {
    w <- weights
  }

  w <- w / sum(w)

  # --- Normalization ---
  norm <- matrix(NA, n_alt, n_crit)
  for (j in seq_len(n_crit)) {
    if (criteria_types[j] == "max") {
      norm[, j] <- defuzz[, j] / max(defuzz[, j])
    } else {
      norm[, j] <- min(defuzz[, j]) / defuzz[, j]
    }
  }

  # --- Weighted Sum Model (WSM) ---
  wsm <- norm %*% w

  # --- Weighted Product Model (WPM) ---
  wpm <- apply(norm, 1, function(x) prod(x ^ w))

  # --- WASPAS score ---
  Q <- lambda * wsm + (1 - lambda) * wpm
  ranking <- rank(-Q, ties.method = "min")

  data.frame(
    Alternative = seq_len(n_alt),
    Score = as.numeric(Q),
    Ranking = ranking
  )
}

# ============================================================
# FUZZY MULTIMOORA
# ============================================================

#' @title Internal MULTIMOORA Dominance Aggregation
#' @description Agreguje trzy wewnętrzne rankingi metody MULTIMOORA (RS, RP, MF).
#' @keywords internal
.teoria_dominacji_multimoora <- function(r1, r2, r3) {
  n <- length(r1)
  finalny_ranking <- rep(0, n)
  macierz_rang <- cbind(r1, r2, r3)
  dostepne <- rep(TRUE, n)

  for (poz in 1:n) {
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf

    # Znajdź kandydatów z najlepszą (najniższą) rangą w każdej podmetodzie
    c1 <- which.min(obecna_macierz[, 1])
    c2 <- which.min(obecna_macierz[, 2])
    c3 <- which.min(obecna_macierz[, 3])
    kandydaci <- c(c1, c2, c3)

    # Mechanizm głosowania (Voting Mechanism)
    czestosc <- table(kandydaci)
    zwyciezca <- as.numeric(names(czestosc)[which.max(czestosc)])

    # Rozstrzyganie remisu
    if (length(czestosc) == 3) {
      sumy <- rowSums(macierz_rang[kandydaci, ])
      zwyciezca <- kandydaci[which.min(sumy)]
    }

    finalny_ranking[zwyciezca] <- poz
    dostepne[zwyciezca] <- FALSE
  }

  return(finalny_ranking)
}

#' Rozmyta Metoda MULTIMOORA
#'
#' @description Implementacja metody Fuzzy MULTIMOORA. Składa się z:
#' 1. Ratio System (RS)
#' 2. Reference Point (RP)
#' 3. Full Multiplicative Form (FMF)
#' Finalny ranking powstaje przez agregację Teorią Dominacji.
#'
#' @param decision_mat Matrix of TFN values (n x 3m format)
#' @param criteria_types Vector of "max"/"min" indicating benefit/cost criteria
#' @param weights Optional numeric vector of fuzzy weights (length = 3 * number of criteria)
#' @param bwm_criteria Optional criteria names for BWM weight calculation
#' @param bwm_best Optional Best-to-Others vector for BWM
#' @param bwm_worst Optional Others-to-Worst vector for BWM
#' @return Obiekt klasy `rozmyty_multimoora_wynik`.
#' @export
rozmyty_multimoora <- function(decision_mat,
                               criteria_types,
                               weights = NULL,
                               bwm_criteria,
                               bwm_best,
                               bwm_worst) {

  if (!is.matrix(decision_mat))
    stop("'decision_mat' must be a matrix.")

  # 1. Obliczenie wag
  final_w <- .get_final_weights(
    decision_mat,
    weights,
    bwm_criteria,
    bwm_best,
    bwm_worst
  )

  n_rows <- nrow(decision_mat)
  n_cols <- ncol(decision_mat)

  # Rozszerzenie typów kryteriów
  fuzzy_cb <- rep(criteria_types, each = 3)
  # 2. Normalizacja Wektorowa
  norm_mat <- matrix(0, nrow = n_rows, ncol = n_cols)
  for (i in seq(1, n_cols, 3)) {
    denom <- sqrt(sum(decision_mat[,i]^2 +
                        decision_mat[,i+1]^2 +
                        decision_mat[,i+2]^2))
    if (denom == 0) denom <- 1
    norm_mat[,i] <- decision_mat[,i] / denom
    norm_mat[,i+1] <- decision_mat[,i+1] / denom
    norm_mat[,i+2] <- decision_mat[,i+2] / denom
  }

  # --- CZĘŚĆ A: System Ilorazowy (RS) ---
  rs_weighted <- norm_mat
  for (j in 1:n_cols) {
    rs_weighted[, j] <- norm_mat[, j] * final_w[j]
  }

  rs_fuzzy <- matrix(0, nrow = n_rows, ncol = 3)
  for (j in seq(1, n_cols, 3)) {
    if (fuzzy_cb[j] == 'max') {
      rs_fuzzy[,1] <- rs_fuzzy[,1] + rs_weighted[, j]
      rs_fuzzy[,2] <- rs_fuzzy[,2] + rs_weighted[, j+1]
      rs_fuzzy[,3] <- rs_fuzzy[,3] + rs_weighted[, j+2]
    } else {
      rs_fuzzy[,1] <- rs_fuzzy[,1] - rs_weighted[, j+2]
      rs_fuzzy[,2] <- rs_fuzzy[,2] - rs_weighted[, j+1]
      rs_fuzzy[,3] <- rs_fuzzy[,3] - rs_weighted[, j]
    }
  }

  def_rs <- rowMeans(rs_fuzzy)
  rank_rs <- rank(-def_rs, ties.method = "first")

  # --- CZĘŚĆ B: Punkt Odniesienia (RP) ---
  ref_point <- numeric(n_cols)
  for (j in 1:n_cols) {
    if (fuzzy_cb[j] == 'max')
      ref_point[j] <- max(rs_weighted[, j])
    else
      ref_point[j] <- min(rs_weighted[, j])
  }

  distances <- matrix(0, nrow = n_rows, ncol = n_cols/3)
  k <- 1
  for (j in seq(1, n_cols, 3)) {
    d_l <- (rs_weighted[, j] - ref_point[j])^2
    d_m <- (rs_weighted[, j+1] - ref_point[j+1])^2
    d_u <- (rs_weighted[, j+2] - ref_point[j+2])^2
    distances[, k] <- sqrt(d_l + d_m + d_u)
    k <- k + 1
  }

  def_rp <- apply(distances, 1, max)
  rank_rp <- rank(def_rp, ties.method = "first")
  # --- CZĘŚĆ C: Forma Multiplikatywna (FMF) ---
  prod_benefit <- matrix(1, nrow = n_rows, ncol = 3)
  prod_cost <- matrix(1, nrow = n_rows, ncol = 3)

  for (j in seq(1, n_cols, 3)) {
    w <- final_w[j+1]
    triplet <- norm_mat[, j:(j+2)]

    if (fuzzy_cb[j] == 'max') {
      prod_benefit[,1] <- prod_benefit[,1] * (triplet[,1]^w)
      prod_benefit[,2] <- prod_benefit[,2] * (triplet[,2]^w)
      prod_benefit[,3] <- prod_benefit[,3] * (triplet[,3]^w)
    } else {
      prod_cost[,1] <- prod_cost[,1] * (triplet[,1]^w)
      prod_cost[,2] <- prod_cost[,2] * (triplet[,2]^w)
      prod_cost[,3] <- prod_cost[,3] * (triplet[,3]^w)
    }
  }

  prod_cost[prod_cost == 0] <- 1e-9

  fmf_fuzzy <- matrix(0, nrow = n_rows, ncol = 3)
  fmf_fuzzy[,1] <- prod_benefit[,1] / prod_cost[,3]
  fmf_fuzzy[,2] <- prod_benefit[,2] / prod_cost[,2]
  fmf_fuzzy[,3] <- prod_benefit[,3] / prod_cost[,1]

  def_fmf <- rowMeans(fmf_fuzzy)
  rank_fmf <- rank(-def_fmf, ties.method = "first")
  # Agregacja przez Teorię Dominacji
  final_ranking <- .teoria_dominacji_multimoora(rank_rs, rank_rp, rank_fmf)

  results_df <- data.frame(
    Alternative = 1:n_rows,
    RS_Score = def_rs,
    RS_Ranking = rank_rs,
    RP_Score = def_rp,
    RP_Ranking = rank_rp,
    FMF_Score = def_fmf,
    FMF_Ranking = rank_fmf,
    Final_Ranking = final_ranking
  )

  output <- list(
    results = results_df,
    method = "MULTIMOORA"
  )

  class(output) <- "rozmyty_multimoora_wynik"
  return(output)
}

# ============================================================
# META-RANKING (5 METHODS)
# ============================================================

#' @title Teoria Dominacji dla Rankingu
#' @description Funkcja pomocnicza do konsensusu.
#' @keywords internal
calculate_dominance_ranking <- function(rank_mat) {
  n <- nrow(rank_mat)
  final_rank <- rep(0, n)
  available <- rep(TRUE, n)

  for (current_position in 1:n) {
    current_mat <- rank_mat
    current_mat[!available, ] <- Inf

    # Kto ma najlepszą rangę w każdej metodzie?
    candidates <- apply(current_mat, 2, which.min)
    freq_table <- table(candidates)

    max_votes <- max(freq_table)
    winners <- as.numeric(names(freq_table)[freq_table == max_votes])

    if (length(winners) == 1) {
      winner_idx <- winners
    } else {
      # Remis: wybierz tego z najmniejszą sumą rang
      sums <- rowSums(rank_mat[winners, , drop = FALSE])
      winner_idx <- winners[which.min(sums)]
    }

    final_rank[winner_idx] <- current_position
    available[winner_idx] <- FALSE
  }

  return(final_rank)
}

#' @title Rozmyty Meta-Ranking (5 Metod)
#' @description Agreguje wyniki: VIKOR, TOPSIS, WASPAS, MULTIMOORA, PROMETHEE.
#'
#' @param decision_mat Rozmyta macierz decyzyjna.
#' @param criteria_types Wektor typów kryteriów ("min", "max").
#' @param weights Wektor wag (opcjonalny).
#' @param preference_params Ramka parametrów dla PROMETHEE (opcjonalna).
#' @param bwm_best,bwm_worst Parametry BWM.
#' @param lambda Parametr WASPAS.
#' @param v Parametr VIKOR.
#'
#' @export
fuzzy_meta_ranking <- function(decision_mat,
                               criteria_types,
                               weights = NULL,
                               preference_params = NULL,
                               bwm_best = NULL,
                               bwm_worst = NULL,
                               lambda = 0.5,
                               v = 0.5) {

  # 1. Wagi (jeśli brak -> Entropia)
  if (is.null(weights) && (is.null(bwm_best) || is.null(bwm_worst))) {
    message("Brak wag. Obliczam Entropię...")
    weights_raw <- .calculate_entropy_weights(decision_mat)
    weights <- rep(weights_raw, each = 3)
  }

  # Domyślne parametry PROMETHEE (Linear, q=0, p=2)
  if (is.null(preference_params)) {
    n_crit <- ncol(decision_mat) / 3
    preference_params <- data.frame(
      Type = rep("linear", n_crit),
      q = rep(0, n_crit),
      p = rep(2, n_crit),
      s = rep(NA, n_crit),
      Role = rep("max", n_crit)
    )
    for (j in 1:n_crit) {
      preference_params$Role[j] <- criteria_types[(j - 1) * 3 + 1]
    }
  }
  # 2. Uruchomienie Metod
  args_base <- list(
    decision_mat = decision_mat,
    criteria_types = criteria_types
  )

  if (!is.null(weights)) args_base$weights <- weights

  if (!is.null(bwm_best)) {
    args_base$bwm_best <- bwm_best
    args_base$bwm_worst <- bwm_worst
  }

  # VIKOR, TOPSIS, WASPAS
  res_vikor <- do.call(fuzzy_vikor, c(args_base, list(v = v)))
  res_topsis <- do.call(fuzzy_topsis, args_base)
  res_waspas <- do.call(fuzzy_waspas, c(args_base, list(lambda = lambda)))

  # MULTIMOORA
  res_mm <- do.call(rozmyty_multimoora, args_base)

  # PROMETHEE (wymaga innej listy argumentów)
  args_prom <- args_base
  args_prom$criteria_types <- NULL  # PROMETHEE bierze role z preference_params
  args_prom$preference_params <- preference_params
  res_prom <- do.call(rozmyty_promethee, args_prom)
  # 3. Zestawienie Wyników
  rank_matrix <- cbind(
    res_vikor$results$Ranking,
    res_topsis$Ranking,
    res_waspas$Ranking,
    res_mm$results$Final_Ranking,
    res_prom$results$Ranking
  )

  colnames(rank_matrix) <- c("VIKOR", "TOPSIS", "WASPAS", "MMOORA", "PROMETHEE")

  # 4. Agregacja
  # A. Suma
  rank_sum <- rank(rowSums(rank_matrix), ties.method = "first")

  # B. Dominacja
  rank_dom <- calculate_dominance_ranking(rank_matrix)

  # C. RankAggreg
  ra_input <- t(apply(rank_matrix, 2, order))  # Konwersja na listę indeksów
  n_alt <- nrow(decision_mat)

  if (n_alt <= 10) {
    ra <- RankAggreg::BruteAggreg(ra_input, n_alt, distance = "Spearman")
  } else {
    ra <- RankAggreg::RankAggreg(ra_input, n_alt, method = "GA",
                                 distance = "Spearman", verbose = FALSE)
  }

  # Konwersja wyniku RA na wektor rang
  rank_ra <- numeric(n_alt)
  top_list <- ra$top.list
  if (is.numeric(top_list)) {
    for (i in 1:n_alt) rank_ra[top_list[i]] <- i
  } else {
    for (i in 1:n_alt) rank_ra[top_list[i]] <- i
  }
  # 5. Wynik końcowy
  comp_df <- data.frame(
    Alternative = rownames(decision_mat),
    R_VIKOR = rank_matrix[, 1],
    R_TOPSIS = rank_matrix[, 2],
    R_WASPAS = rank_matrix[, 3],
    R_MMOORA = rank_matrix[, 4],
    R_PROMETHEE = rank_matrix[, 5],
    Meta_Sum = rank_sum,
    Meta_Dominance = rank_dom,
    Meta_Aggreg = rank_ra
  )

  return(list(
    comparison = comp_df,
    correlations = cor(comp_df[, -1], method = "spearman")
  ))
}

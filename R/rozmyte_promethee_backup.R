# ============================================================
# FUZZY PROMETHEE II
# ============================================================

#' @title Internal Preference Calculator
#' @description Oblicza wartości preferencji dla metody PROMETHEE.
#' @keywords internal
.oblicz_preferencje_promethee <- function(d, typ, q, p, s) {
  P <- matrix(0, nrow(d), ncol(d))

  if (typ == "usual") {
    P <- ifelse(d > 0, 1, 0)

  } else if (typ == "u-shape") {
    P <- ifelse(d > q, 1, 0)

  } else if (typ == "v-shape") {
    P <- ifelse(d > p, 1, ifelse(d <= 0, 0, d / p))

  } else if (typ == "level") {
    P <- ifelse(d > p, 1, ifelse(d > q, 0.5, 0))

  } else if (typ == "linear") {
    P <- ifelse(d > p, 1, ifelse(d <= q, 0, (d - q) / (p - q)))

  } else if (typ == "gaussian") {
    P <- ifelse(d <= 0, 0, 1 - exp(-(d^2) / (2 * s^2)))
  }

  return(P)
}
#' Rozmyta Metoda PROMETHEE II
#'
#' @description Implementacja metody Fuzzy PROMETHEE. Oblicza przepływy netto (Phi).
#'
#' @param decision_mat Rozmyta macierz danych (n x 3m format)
#' @param preference_params Ramka danych z kolumnami: Type, q, p, s, Role ("min"/"max")
#' @param weights Wektor wag (opcjonalny)
#' @param bwm_criteria Optional criteria names for BWM weight calculation
#' @param bwm_best Optional Best-to-Others vector for BWM
#' @param bwm_worst Optional Others-to-Worst vector for BWM
#' @return Obiekt klasy `rozmyty_promethee_wynik`.
#' @export
rozmyty_promethee <- function(decision_mat,
                              preference_params,
                              weights = NULL,
                              bwm_criteria,
                              bwm_best,
                              bwm_worst) {

  # Obliczenie wag
  final_w <- .get_final_weights(
    decision_mat,
    weights,
    bwm_criteria,
    bwm_best,
    bwm_worst
  )

  # PROMETHEE wymaga ostrych (crisp), znormalizowanych wag
  n_crit <- ncol(decision_mat) / 3
  crisp_w <- numeric(n_crit)
  for (j in 1:n_crit) {
    idx <- (j - 1) * 3 + 1
    crisp_w[j] <- mean(final_w[idx:(idx + 2)])
  }
  crisp_w <- crisp_w / sum(crisp_w)

  n_alt <- nrow(decision_mat)
  Pi_total <- matrix(0, n_alt, n_alt)  # Zagregowana preferencja
  # Pętla po kryteriach
  for (j in 1:n_crit) {
    typ <- as.character(preference_params[j, "Type"])
    q <- as.numeric(preference_params[j, "q"])
    p <- as.numeric(preference_params[j, "p"])
    s <- as.numeric(preference_params[j, "s"])
    rola <- as.character(preference_params[j, "Role"])

    # Dla uproszczenia: używamy środków trójek (crisp inputs)
    idx_m <- (j - 1) * 3 + 2
    vals <- decision_mat[, idx_m]

    if (rola == "max") {
      d <- outer(vals, vals, "-")
    } else {
      d <- outer(vals, vals, "-") * -1
    }

    P_crit <- .oblicz_preferencje_promethee(d, typ, q, p, s)
    Pi_total <- Pi_total + (P_crit * crisp_w[j])
  }

  diag(Pi_total) <- 0
  # Obliczenie przepływów
  Phi_plus <- rowSums(Pi_total) / (n_alt - 1)
  Phi_minus <- colSums(Pi_total) / (n_alt - 1)
  Phi_net <- Phi_plus - Phi_minus

  # Wyniki
  results_df <- data.frame(
    Alternative = 1:n_alt,
    Phi_Plus = Phi_plus,
    Phi_Minus = Phi_minus,
    Phi_Net = Phi_net,
    Ranking = rank(-Phi_net, ties.method = "first")
  )

  output <- list(
    results = results_df,
    method = "PROMETHEE II"
  )

  class(output) <- "rozmyty_promethee_wynik"
  return(output)
}

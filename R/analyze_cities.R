#' @title Analyze Cities
#' @description Glowna funkcja pakietu CityOptimaR.
#' Przeprowadza pelna analize wielokryterialna dla miast
#' na podstawie kryteriow jakosci zycia.
#' @param data Ramka danych z danymi o miastach (musi zawierac kolumne City).
#' @param criteria_types Wektor typow kryteriow ('max' lub 'min').
#' @param weights Opcjonalny wektor wag. Jesli NULL, stosowana jest Entropia Shannona.
#' @param thresholds Opcjonalny nazwany wektor minimalnych progow akceptowalnosci
#'   dla kryteriow (w skali Saaty 1-9). Np. c(Safety = 4, Healthcare = 3).
#' @param profile Opcjonalny profil uzytkownika: 'student', 'family' lub 'retiree'.
#'   Jesli podany, automatycznie ustawia wagi i progi akceptowalnosci.
#'   Nadpisuje argumenty weights i thresholds.
#' @return Lista z rankingiem miast i wynikami wszystkich metod.
#' @export
analyze_cities <- function(data,
                           criteria_types = NULL,
                           weights = NULL,
                           thresholds = NULL,
                           profile = NULL) {

  if (!is.data.frame(data))
    stop('Argument data musi byc ramka danych (data.frame).')
  if (!'City' %in% names(data))
    stop('Dane musza zawierac kolumne City z nazwami miast.')

  if (!is.null(profile)) {
    prof <- .get_life_profile(profile)
    weights    <- prof$weights
    thresholds <- prof$thresholds
  }

  nazwy_miast <- data$City
  kol_kryteriow <- setdiff(names(data), c('City', 'Country'))
  syntax_auto <- paste(paste(kol_kryteriow, '=~', kol_kryteriow), collapse = '; ')
  macierz <- prepare_mcda_data(data, syntax_auto, alternative_column = 'City')
  n_kryteriow <- ncol(macierz) / 3
  nazwy_kryteriow <- attr(macierz, 'criterion_names')

  if (is.null(criteria_types))
    criteria_types <- rep('max', n_kryteriow)

  if (is.null(weights)) {
    wagi_fuzzy <- .calculate_entropy_weights(macierz)
    wagi_crisp <- wagi_fuzzy[seq(2, length(wagi_fuzzy), 3)]
    wagi_crisp <- wagi_crisp / sum(wagi_crisp)
  } else {
    wagi_crisp <- weights / sum(weights)
    wagi_fuzzy <- rep(wagi_crisp, each = 3)
  }
  names(wagi_crisp) <- nazwy_kryteriow

  wynik_topsis <- fuzzy_topsis(macierz, criteria_types, weights = wagi_fuzzy, thresholds = thresholds)
  wynik_vikor  <- fuzzy_vikor(macierz, criteria_types, weights = wagi_fuzzy)
  wynik_waspas <- fuzzy_waspas(macierz, criteria_types, weights = wagi_crisp)

  rank_matrix <- cbind(
    TOPSIS = wynik_topsis$Ranking,
    VIKOR  = wynik_vikor$results$Ranking,
    WASPAS = wynik_waspas$Ranking
  )

  meta_konsensus <- calculate_dominance_ranking(rank_matrix)

  ranking_finalny <- data.frame(
    Rank          = meta_konsensus,
    City          = nazwy_miast,
    TOPSIS_Score  = round(wynik_topsis$Score, 4),
    TOPSIS_Rank   = wynik_topsis$Ranking,
    TOPSIS_Status = wynik_topsis$Status,
    VIKOR_Rank    = wynik_vikor$results$Ranking,
    WASPAS_Rank   = wynik_waspas$Ranking,
    Consensus_Rank = rank(rowSums(rank_matrix, na.rm = TRUE), ties.method = 'first')
  )
  ranking_finalny <- ranking_finalny[order(ranking_finalny$Rank), ]

  return(invisible(list(
    ranking = ranking_finalny,
    topsis  = wynik_topsis,
    vikor   = wynik_vikor,
    waspas  = wynik_waspas,
    weights = wagi_crisp,
    profile = profile
  )))
}

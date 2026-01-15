#' @title Teoria Dominacji dla Rankingu
#' @description
#' Funkcja pomocnicza. Wyznacza ranking konsensusu na podstawie reguły większości.
#' Iteracyjnie sprawdza, która alternatywa najczęściej wygrywa na danej pozycji.
#'
#' @param r1 Wektor numeryczny rang metody 1.
#' @param r2 Wektor numeryczny rang metody 2.
#' @param r3 Wektor numeryczny rang metody 3.
#' @return Wektor numeryczny z finalnym rankingiem.
#' @keywords internal
.oblicz_ranking_dominacji <- function(r1, r2, r3) {
  n <- length(r1)
  finalny_ranking <- rep(0, n)

  # Macierz rang (wiersze = alternatywy, kolumny = metody)
  macierz_rang <- cbind(r1, r2, r3)

  # Maska dostepnych alternatyw (na początku wszystkie są dostępne)
  dostepne <- rep(TRUE, n)

  for (obecna_pozycja in 1:n) {
    # Pobieramy rangi tylko dla dostepnych alternatyw (reszte zamieniamy na Inf)
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf

    # Kto ma najlepszą (najniższą) rangę w każdej metodzie?
    najlepszy_r1 <- which.min(obecna_macierz[, 1])
    najlepszy_r2 <- which.min(obecna_macierz[, 2])
    najlepszy_r3 <- which.min(obecna_macierz[, 3])

    kandydaci <- c(najlepszy_r1, najlepszy_r2, najlepszy_r3)

    # Głosowanie większościowe (kto pojawia się najczęściej?)
    tabela_czestosci <- table(kandydaci)
    zwyciezca_idx <- as.numeric(names(tabela_czestosci)[which.max(tabela_czestosci)])

    # Obsługa remisu (gdy każdy kandydat jest inny: 3 różne wskazania)
    if (length(tabela_czestosci) == 3) {
      c1 <- najlepszy_r1; c2 <- najlepszy_r2; c3 <- najlepszy_r3

      # Sprawdzamy "kto lepszy od kogo" w parach (Head-to-Head)
      c1_wygrane <- sum(macierz_rang[c1, ] < macierz_rang[c2, ]) +
        sum(macierz_rang[c1, ] < macierz_rang[c3, ])
      c2_wygrane <- sum(macierz_rang[c2, ] < macierz_rang[c1, ]) +
        sum(macierz_rang[c2, ] < macierz_rang[c3, ])
      c3_wygrane <- sum(macierz_rang[c3, ] < macierz_rang[c1, ]) +
        sum(macierz_rang[c3, ] < macierz_rang[c2, ])

      wygrane <- c(c1_wygrane, c2_wygrane, c3_wygrane)
      if (which.max(wygrane) == 1) zwyciezca_idx <- c1
      else if (which.max(wygrane) == 2) zwyciezca_idx <- c2
      else zwyciezca_idx <- c3
    }

    # Przypisz pozycje i oznacz jako niedostepnego
    finalny_ranking[zwyciezca_idx] <- obecna_pozycja
    dostepne[zwyciezca_idx] <- FALSE
  }

  return(finalny_ranking)
}

#' @title Rozmyty Meta-Ranking
#' @description
#' Agreguje wyniki z metod Fuzzy VIKOR, TOPSIS i WASPAS, aby stworzyć
#' jeden, robustny ranking konsensusu.
#'
#' @param macierz_decyzyjna Rozmyta macierz danych.
#' @param typy_kryteriow Wektor typów ("min", "max").
#' @param wagi (Opcjonalnie) Wagi kryteriów.
#' @param bwm_najlepsze (Opcjonalnie) Wektor BWM Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor BWM Others-to-Worst.
#' @param lambda Parametr dla WASPAS (domyślnie 0.5).
#' @param v Parametr dla VIKOR (domyślnie 0.5).
#'
#' @return Lista zawierająca ramkę danych z porównaniem rankingów oraz macierz korelacji.
#' @importFrom RankAggreg BruteAggreg RankAggreg
#' @importFrom stats cor
#' @export
rozmyty_meta_ranking <- function(macierz_decyzyjna,
                                 typy_kryteriow,
                                 wagi = NULL,
                                 bwm_najlepsze = NULL,
                                 bwm_najgorsze = NULL,
                                 lambda = 0.5,
                                 v = 0.5) {

  # 1. Sprawdzenie wag (jesli brak BWM i brak wag recznych -> licz Entropie)
  if (is.null(wagi) && (is.null(bwm_najlepsze) || is.null(bwm_najgorsze))) {
    message("Brak wag i parametrów BWM. Obliczam wagi metodą Entropii...")
    wagi_surowe <- .calculate_entropy_weights(macierz_decyzyjna)
    wagi <- wagi_surowe
  }

  # 2. Uruchomienie poszczególnych metod
  # Przygotowujemy liste argumentow wspolnych
  args_baza <- list(decision_mat = macierz_decyzyjna, criteria_types = typy_kryteriow)

  if (!is.null(wagi)) args_baza$weights <- wagi
  if (!is.null(bwm_najlepsze)) {
    args_baza$bwm_best <- bwm_najlepsze
    args_baza$bwm_worst <- bwm_najgorsze
    # Pobieramy nazwy kryteriow z atrybutu macierzy, zeby BWM zadzialal
    args_baza$bwm_criteria <- attr(macierz_decyzyjna, "nazwy_kryteriow")
  }

  # VIKOR
  args_vikor <- c(args_baza, list(v = v))
  res_vikor <- do.call(fuzzy_vikor, args_vikor)

  # TOPSIS
  res_topsis <- do.call(fuzzy_topsis, args_baza)

  # WASPAS
  args_waspas <- c(args_baza, list(lambda = lambda))
  res_waspas <- do.call(fuzzy_waspas, args_waspas)

  # 3. Ekstrakcja Rankingów (same wektory liczb całkowitych)
  r_vikor <- res_vikor$results$Ranking
  r_topsis <- res_topsis$Ranking
  r_waspas <- res_waspas$Ranking
  # 4. Agregacja Rankingów
  # A. Suma Rang (Im mniej tym lepiej)
  suma_pkt <- r_vikor + r_topsis + r_waspas
  ranking_suma <- rank(suma_pkt, ties.method = "first")

  # B. Teoria Dominacji
  ranking_dominacja <- .oblicz_ranking_dominacji(r_vikor, r_topsis, r_waspas)

  # C. RankAggreg (Algorytm Brute Force)
  # RankAggreg wymaga listy uporządkowanych indeksów, a nie wektora rang!
  macierz_dla_ra <- rbind(
    order(r_vikor),
    order(r_topsis),
    order(r_waspas)
  )

  n_alt <- nrow(macierz_decyzyjna)

  # Jeśli mało alternatyw (<10), używamy Brute Force (dokładny).
  # Jeśli dużo, używamy Algorytmu Aggreg (przybliżony, ale szybszy).
  if (n_alt <= 10) {
    ra_wynik <- RankAggreg::BruteAggreg(macierz_dla_ra, n_alt, distance = "Spearman", verbose = FALSE)
  } else {
    ra_wynik <- RankAggreg::RankAggreg(macierz_dla_ra, n_alt, method = "GA", distance = "Spearman", verbose = FALSE)
  }

  # Konwersja wyniku RankAggreg (lista indeksów) na wektor rang
  top_lista <- ra_wynik$top.list
  wektor_ra <- numeric(n_alt)

  # Mapowanie: top_lista[1] to indeks zwyciezcy -> dostaje range 1
  for(pozycja in 1:n_alt) {
    indeks_alternatywy <- as.numeric(top_lista[pozycja])
    wektor_ra[indeks_alternatywy] <- pozycja
  }

  # 5. Zestawienie wyników
  porownanie_df <- data.frame(
    Alternatywa = 1:n_alt,
    R_VIKOR = r_vikor,
    R_TOPSIS = r_topsis,
    R_WASPAS = r_waspas,
    Meta_Suma = ranking_suma,
    Meta_Dominacja = ranking_dominacja,
    Meta_Agregacja = wektor_ra
  )

  # Macierz korelacji Spearmana (czy metody są zgodne?)
  macierz_kor <- cor(porownanie_df[,-1], method = "spearman")

  wynik <- list(
    porownanie = porownanie_df,
    korelacje = macierz_kor
  )

  return(wynik)
}

#' @title Generowanie Tabeli APA
#' @description
#' Funkcja przekształca wyniki analizy MCDA (TOPSIS, VIKOR, WASPAS, Meta-Ranking)
#' w sformatowaną tabelę zgodną ze standardem APA, gotową do publikacji w Wordzie.
#'
#' @param x Obiekt wynikowy z funkcji pakietu (np. `fuzzy_vikor_res`).
#' @param tytul Opcjonalny tytuł tabeli.
#' @return Obiekt klasy `flextable` gotowy do druku lub zapisu do Worda.
#' @importFrom rempsyc nice_table
#' @importFrom flextable autofit save_as_docx
#' @export
tabela_apa <- function(x, tytul = NULL) {
  UseMethod("tabela_apa")
}

#' @export
tabela_apa.fuzzy_vikor_res <- function(x, tytul = "Wyniki metody Fuzzy VIKOR") {
  df <- x$results

  # Formatowanie nazw kolumn dla czytelnika
  names(df) <- c("Alternatywa", "S (Grupa)", "R (Żal)", "Q (Kompromis)", "Ranking")

  # Zaokrąglenia
  df$`S (Grupa)` <- round(df$`S (Grupa)`, 3)
  df$`R (Żal)` <- round(df$`R (Żal)`, 3)
  df$`Q (Kompromis)` <- round(df$`Q (Kompromis)`, 4)

  # Tworzenie tabeli
  rempsyc::nice_table(
    df,
    title = c("Tabela 1", tytul),
    note = c("Uwaga. S: użyteczność grupy, R: indywidualny żal, Q: indeks kompromisu (im mniej tym lepiej).")
  )
}

#' @export
tabela_apa.data.frame <- function(x, tytul = "Wyniki MCDA") {
  # Obsługa zwykłych data.frame (TOPSIS, WASPAS)

  # Sprawdź czy to TOPSIS (ma kolumnę Score)
  if ("Score" %in% names(x)) {
    names(x) <- c("Alternatywa", "Wynik (CC)", "Ranking")
    x$`Wynik (CC)` <- round(x$`Wynik (CC)`, 4)

    tabela <- rempsyc::nice_table(
      x,
      title = c("Tabela 2", tytul),
      note = c("Uwaga. CC - Coefficient of Closeness. Im wyższa wartość, tym lepsza alternatywa.")
    )
  } else {
    # Domyślne formatowanie dla innych tabel
    tabela <- rempsyc::nice_table(
      x,
      title = c("Tabela", tytul)
    )
  }

  return(tabela)
}

#' @export
tabela_apa.list <- function(x, tytul = "Meta-Ranking (Konsensus)") {
  # Obsługa Meta-Rankingu
  if(is.null(x$porownanie)) stop("To nie jest obiekt meta-rankingu.")

  df <- x$porownanie

  # Usuwamy "podłogi" z nazw kolumn (np. Meta_Suma -> Meta Suma)
  names(df) <- gsub("_", " ", names(df))

  rempsyc::nice_table(
    df,
    title = c("Tabela 3", tytul),
    note = c("Zestawienie rang uzyskanych różnymi metodami oraz rankingi konsensusu.")
  )
}
#' @title Zapisz tabelę do Worda
#' @description Pomocnicza funkcja do zapisu tabeli flextable do pliku .docx
#' @param tabela Obiekt flextable (wynik funkcji tabela_apa)
#' @param sciezka Ścieżka do pliku wyjściowego (np. "wyniki.docx")
#' @export
zapisz_tabele <- function(tabela, sciezka) {
  flextable::save_as_docx(tabela, path = sciezka)
  message("Tabela zapisana w: ", sciezka)
}
# ============================================================
# MULTIMOORA & PROMETHEE APA TABLES
# ============================================================

#' @export
tabela_apa.rozmyty_multimoora_wynik <- function(x, tytul = "Wyniki MULTIMOORA") {
  df <- x$results[, c("Alternative", "RS_Ranking", "RP_Ranking", "FMF_Ranking", "Final_Ranking")]
  names(df) <- c("Alternatywa", "Rank Ratio", "Rank Ref.Point", "Rank Mult.Form", "MULTIMOORA")

  rempsyc::nice_table(df, title = c("Tabela", tytul))
}

#' @export
tabela_apa.rozmyty_promethee_wynik <- function(x, tytul = "Wyniki PROMETHEE II") {
  df <- x$results
  df$Phi_Net <- round(df$Phi_Net, 3)
  names(df) <- c("Alternatywa", "Phi+ (Leaving)", "Phi- (Entering)", "Phi Net", "Ranking")

  rempsyc::nice_table(df, title = c("Tabela", tytul))
}

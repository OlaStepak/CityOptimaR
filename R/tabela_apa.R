#' @title Generowanie Tabeli APA
#' @description Funkcja przeksztalca wyniki analizy MCDA w tabele zgodna ze standardem APA.
#' @param x Obiekt wynikowy z funkcji pakietu.
#' @param tytul Opcjonalny tytul tabeli.
#' @importFrom rempsyc nice_table
#' @importFrom flextable autofit save_as_docx
#' @export
tabela_apa <- function(x, tytul = NULL) {
  UseMethod('tabela_apa')
}

#' @export
tabela_apa.fuzzy_vikor_res <- function(x, tytul = 'Wyniki metody Fuzzy VIKOR') {
  df <- x$results
  names(df) <- c('Alternatywa', 'S (Grupa)', 'R (Zal)', 'Q (Kompromis)', 'Ranking')
  df[['S (Grupa)']] <- round(df[['S (Grupa)']], 3)
  df[['R (Zal)']] <- round(df[['R (Zal)']], 3)
  df[['Q (Kompromis)']] <- round(df[['Q (Kompromis)']], 4)
  rempsyc::nice_table(df,
    title = c('Tabela 1', tytul),
    note = 'S: uzytecznosc grupy, R: indywidualny zal, Q: indeks kompromisu (im mniej tym lepiej).'
  )
}

#' @export
tabela_apa.data.frame <- function(x, tytul = 'Wyniki Fuzzy TOPSIS') {
  if ('Score' %in% names(x)) {
    names(x) <- c('Alternatywa', 'Wynik (CC)', 'Ranking')
    x[['Wynik (CC)']] <- round(x[['Wynik (CC)']], 4)
    rempsyc::nice_table(x,
      title = c('Tabela 2', tytul),
      note = 'CC - Wspolczynnik Bliskosci. Im wyzsza wartosc, tym miasto blizsze idealu.'
    )
  } else {
    rempsyc::nice_table(x, title = c('Tabela', tytul))
  }
}

#' @export
tabela_apa.list <- function(x, tytul = 'Meta-Ranking (Konsensus)') {
  if (is.null(x$comparison)) stop('To nie jest obiekt meta-rankingu.')
  df <- x$comparison
  names(df) <- gsub('_', ' ', names(df))
  rempsyc::nice_table(df,
    title = c('Tabela 3', tytul),
    note = 'Zestawienie rang metod TOPSIS, VIKOR, WASPAS oraz meta-rankingu konsensusu.'
  )
}

#' @title Zapisz tabele do Worda
#' @description Zapisuje tabele flextable do pliku .docx
#' @param tabela Obiekt flextable
#' @param sciezka Sciezka do pliku wyjsciowego
#' @export
zapisz_tabele <- function(tabela, sciezka) {
  flextable::save_as_docx(tabela, path = sciezka)
  message('Tabela zapisana w: ', sciezka)
}

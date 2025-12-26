# 0.1: Instalacja i ładowanie pakietów
# Będziemy potrzebować 'tidyverse' do ogólnej pracy na danych
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# Będziemy potrzebować 'remotes' do instalacji konkretnej wersji pakietu
if (!require(remotes)) install.packages("remotes")

# 0.2: Instalacja pakietu FuzzyMCDM (wersja 1.1)
# Użyjemy konkretnej, zarchiwizowanej wersji z CRAN, aby mieć pewność,
# że funkcje odpowiadają dokładnie tym, które analizujemy.
# Wybierz JEDNĄ z poniższych metod instalacji:

# Metoda 1: Używając `remotes` (zalecane)
remotes::install_version(
  "FuzzyMCDM",
  version = "1.1",
  repos   = "https://cran.r-project.org"
)

# Metoda 2: Bezpośrednio z pliku źródłowego (jeśli Metoda 1 zawiedzie)
# install.packages(
#    "https://cran.r-project.org/src/contrib/Archive/FuzzyMCDM/FuzzyMCDM_1.1.tar.gz",
#    repos = NULL,
#    type  = "source"
# )

# 0.3: Załadowanie biblioteki
library(FuzzyMCDM)

# Definicje TFN
BN <- c(0, 0, 0.2)
N  <- c(0, 0.2, 0.4)
S  <- c(0.2, 0.4, 0.6)
W  <- c(0.4, 0.6, 0.8)
BW <- c(0.8, 1, 1)

# Budowanie macierzy wierszami (transpozycja na końcu)
#          C1(l,m,u), C2(l,m,u), C3(l,m,u)
#odpowiedź pojedynczego eksperta
A1 <- c(   W,         S,         N   )
A2 <- c(   N,         W,         W   )
A3 <- c(   S,         S,         BW  )

# Łączymy wiersze w macierz i transponujemy
decision_matrix <- rbind(A1, A2, A3)

# Sprawdźmy wymiary: 3 alternatywy (wiersze), 3 kryteria * 3 = 9 kolumn
print(dim(decision_matrix))
print(decision_matrix)

cb_vector <- c('min', 'max', 'max')
print(cb_vector)

# Wagi dla C1, C2, C3
w_C1 <- c(1/3, 1/3, 1/3)
w_C2 <- c(1/3, 1/3, 1/3)
w_C3 <- c(1/3, 1/3, 1/3)

weights_vector <- c(w_C1, w_C2, w_C3)

# Sprawdźmy długość: 3 kryteria * 3 = 9
print(length(weights_vector))
print(weights_vector)


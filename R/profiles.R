#' @title City Life Profiles
#' @description Predefiniowane profile uzytkownikow do analizy miast.
#' Kazdy profil zawiera wagi kryteriow i progi akceptowalnosci dopasowane
#' do potrzeb konkretnej grupy uzytkownikow.
#' @keywords internal
.get_life_profile <- function(profile) {
  profiles <- list(

    student = list(
      weights = c(
        Cost_Living   = 0.25,
        Job_Market    = 0.25,
        Safety        = 0.10,
        Healthcare    = 0.05,
        Housing_Price = 0.20,
        Transport     = 0.10,
        Air_Quality   = 0.03,
        Green_Space   = 0.02
      ),
      thresholds = c(Safety = 3, Healthcare = 2)
    ),

    family = list(
      weights = c(
        Cost_Living   = 0.08,
        Job_Market    = 0.10,
        Safety        = 0.25,
        Healthcare    = 0.22,
        Housing_Price = 0.10,
        Transport     = 0.05,
        Air_Quality   = 0.12,
        Green_Space   = 0.08
      ),
      thresholds = c(Safety = 5, Healthcare = 4, Air_Quality = 3)
    ),

    retiree = list(
      weights = c(
        Cost_Living   = 0.20,
        Job_Market    = 0.02,
        Safety        = 0.15,
        Healthcare    = 0.30,
        Housing_Price = 0.10,
        Transport     = 0.08,
        Air_Quality   = 0.10,
        Green_Space   = 0.05
      ),
      thresholds = c(Healthcare = 5, Air_Quality = 4, Safety = 4)
    )
  )

  if (!profile %in% names(profiles))
    stop(paste('Nieznany profil. Dostepne profile:', paste(names(profiles), collapse = ', ')))

  profiles[[profile]]
}

#' Clean and convert data columns to numeric
#'
#' This function cleans the input data frame by converting specific columns to numeric type.
#' It ensures that the selected columns are in the correct format for further analysis.
#'
#' @import dplyr
#'
#' @param db A data frame containing the dataset to be cleaned, including columns like `Dossard`, `Place`, `Resultats`, `Valeur`, `Pts_course`, `Temps_pur`, `Penalites`, `ID_competition`, `ID_course`, and `annee`.
#'
#' @return A data frame with the specified columns converted to numeric type.
#' @export
#'
clean_data <- function(db){

  colonnes_a_convertir <- c("Dossard", "Place", "Resultats", "Valeur",
                            "Pts_course", "Temps_pur", "Penalites", "ID_competition",
                            "ID_course", "annee")

  db[colonnes_a_convertir] <- lapply(db[colonnes_a_convertir], as.numeric)

  return(db)
}




# db <- read.csv('../Data/db.csv') %>%
#   filter(ID_course == 20120445,
#          Embarcation != 'INV')
#
# db_cleaned <- clean_data(db)

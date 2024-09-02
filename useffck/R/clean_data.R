clean_data <- function(db){

  colonnes_a_convertir <- c("Dossard", "Place", "Resultats", "Valeur",
                            "Pts_course", "Temps_pur", "Penalites", "ID_competition",
                            "ID_course", "annee")

  db[colonnes_a_convertir] <- lapply(db[colonnes_a_convertir], as.numeric)

  return(db)
}




db <- read.csv('../Data/db.csv') %>%
  filter(ID_course == 20120445,
         Embarcation != 'INV')

db_cleaned <- clean_data(db)

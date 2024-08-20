df_2024 <- read.csv("./Data/2024/df_2024.csv")

df_2024_propre <- df_2024 %>%
  filter(Dossard == as.integer(Dossard),
         Place == as.integer(Place),
         Nom == as.character(Nom),
         Club == as.character(Club),
         Cat == as.character(Cat),
         Resultats != as.integer(Resultats))
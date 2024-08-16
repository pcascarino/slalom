load('./Data/df_final1.RData')


df_pas_ok <- df_final %>%
  filter(is.na(as.numeric(Dossard)))

id_pas_ok <- unique(df_pas_ok$ID_course)

print(id_pas_ok)

# 20120329 est ok juste 0 à la fin
# 20120324 est ok juste 0 à la fin
# 20120418 est ok juste 0 à la fin


# 20120447 pas ok et important (N1)
# 20120445
# 20120429 
# 20120427
# 20120401 
# 20120399 

# 20120465 même pb


df_tidy <- df_final %>%
  filter(!ID_course %in% c(
    20120376,  # Pas le même nbr de portes pour les c2
    20120447,  # pas ok et important (N1)
    20120445,
    20120429,
    20120427,
    20120401,
    20120399,
    20120465   # même pb
  ))
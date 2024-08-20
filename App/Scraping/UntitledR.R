source('./Scraping/Scrap.R')

years_tab <- sort(give_years(), decreasing = TRUE)

ID_compet <- give_id_race(years_tab) %>%
  filter(annee == 2024)

ID_compet <- ID_compet %>%
  filter(! ID_course  %in% c(20120470, 20120376, 20120465))

ID_compet_N1 <- ID_compet %>%
  filter(
    ID_course %in% c(20120427)
  )

df_N1 <- scrap_course(ID_compet_N1, isN1=TRUE, link_rdata='./Data/2024/20120427.RData')
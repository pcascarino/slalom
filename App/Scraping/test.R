source('./Scraping/Scrap.R')

years_tab <- sort(give_years(), decreasing = TRUE)

ID_compet <- give_id_race(years_tab) %>%
  filter(annee == 2024)

ID_compet <- ID_compet %>%
  filter(! ID_course  %in% c(20120470, 20120376, 20120465, 20120386, 20120464))

N1_list <- c(
  20120447,  # pas ok et important (N1)
  20120445,
  20120429,
  #20120427,
  20120401,
  20120399
)

# ID_compet_N1 <- ID_compet %>%
#   filter(
#     ID_course %in% N1_list
#   )
#  
# df_N1 <- scrap_course(ID_compet_N1, isN1=TRUE, link_rdata='./Data/2024/N1.RData')

ID_compet_noN1 <- ID_compet %>%
  filter(
    ! ID_course %in% N1_list
  )

df_noN1 <- scrap_course(ID_compet_noN1, isN1=FALSE, link_rdata='./Data/2024/2024_noN1.RData')


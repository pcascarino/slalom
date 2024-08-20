source('./Scraping/Scrap.R')

years_tab <- sort(give_years(), decreasing = TRUE)

ID_compet <- give_id_race(years_tab) %>%
  filter(annee == 2022)

# ID_compet <- ID_compet %>%
#   filter(! ID_course  %in% c(20120470, 20120376, 20120465, 20120386, 20120464))
# 
# N1_list <- c(
#   20120002,
#   20120115,
#   20120113,
#   20120004
# )
# 
# ID_compet_N1 <- ID_compet %>%
#   filter(
#     ID_course %in% N1_list
#   )
# 
# df_N1 <- scrap_course(ID_compet_N1, isN1=TRUE, link_rdata='./Data/2023/N1.RData')

ID_compet_noN1 <- ID_compet

# %>%
#   filter(
#     ! ID_course %in% N1_list
#   )

df_noN1 <- scrap_course(ID_compet_noN1, isN1=FALSE, link_rdata='./Data/2022/2022_all.RData')


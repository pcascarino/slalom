library(ggplot2)
library(dplyr)



db$Resultats <- as.integer(db$Resultats)

cut_time <- min(db$Resultats) +50

db_reduce <- db %>%
  filter(Resultats < cut_time)

db_bad <- db %>%
  filter(Resultats >cut_time)%>%
  group_by(Embarcation) %>%
  summarise(count=n())

count_bad <- data.frame(cut_time = cut_time, counts = nrow(db_bad))


athlete_name <- 'Quentin Maillefer'
athlete_df <- db %>%
  filter(Nom == athlete_name)



ggplot(db_reduce, aes(x = Resultats)) +
  geom_histogram(aes(fill = Embarcation),
                 binwidth = 1,
                 position = "identity") +
  geom_label(data = db_bad,
             aes(x = cut_time - 10, y = 0,
                 label = paste(count, "athlètes \n(+", cut_time, "sec)")),
             vjust = -0.5,
             alpha = 0.2) +
  facet_grid(Embarcation ~ .) +
  geom_vline(data = athlete_df,
             aes(xintercept = Resultats),
             size=2,
             linetype = "dashed",
             color = "red")

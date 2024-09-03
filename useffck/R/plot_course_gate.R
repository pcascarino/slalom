library(ggplot2)
library(dplyr)

View(db)

ggplot(db,
       aes(x=Temps_pur, y=Penalites)) +
  geom_point()

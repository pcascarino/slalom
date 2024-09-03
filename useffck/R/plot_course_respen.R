library(ggplot2)
library(dplyr)

porte_df <- create_gate_df(db)

# Reshape data to long format
porte_long <- porte_df %>%
  select(Porte, count_2, count_50, Embarcation) %>%
  pivot_longer(cols = starts_with("count"), names_to = "Type", values_to = "Count") %>%
  mutate(Type = recode(Type, count_2 = "2", count_50 = "50"),
         Porte = factor(Porte, levels = as.character(sort(unique(as.numeric(Porte))))))  # Set factor levels based on numeric order


# Create the count plot
ggplot(porte_long, aes(x = Porte, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("2" = "green", "50" = "red")) +
  labs(title = "Count of Gate Results",
       x = "Gate",
       y = "Count",
       fill = "Result Type")


  facet_grid(Embarcation ~ .)


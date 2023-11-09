library(tidyverse)
library(ggplot2)
library(viridis)

g_times <- read.csv("../data/game_times.csv")

# Converting AvgTime into a decimal
g_times <- g_times %>%
  separate(AvgTime, into = c("hours", "minutes"), sep = ":") %>%
  mutate(
    hours = as.numeric(hours),
    minutes = as.numeric(minutes)
  )
g_times$decimal_time <- g_times$hours + g_times$minutes / 60

# Creating barplot
g_times$Year <- as.factor(g_times$Year)

g_length <- ggplot(g_times, aes(x = Year, y = decimal_time, fill = Year)) +
  geom_bar(stat = "identity", position = "identity", color = "white", size = 1.5) +
  scale_fill_viridis(discrete = TRUE, option = "viridis") +
  labs(x = "Year",
       y = "Average Length of Game (Hours)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("barplot_glength.pdf", g_length, height=4, width=6)
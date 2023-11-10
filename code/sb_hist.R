library(ggplot2)

p_2023 <- read.csv("course-paper-or-presentation-lukenoel20/data/players_2023.csv")
p_2022 <- read.csv("course-paper-or-presentation-lukenoel20/data/players_2022.csv")

# Combine both datasets for comparison
df_combined <- rbind(cbind(p_2023, Season = "2023"), cbind(p_2022, Season = "2022"))

# Convert the SB column to numeric
df_combined$r_total_stolen_base <- as.numeric(df_combined$r_total_stolen_base)

# Creating overlayed histograms for stolen bases (2022 vs. 2023)
sb_hist <- ggplot(df_combined, aes(x = r_total_stolen_base, fill = factor(year), color = factor(year))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7, stat = "count") +
  labs(
       x = "Stolen Bases",
       y = "Count") +
  scale_fill_manual(name = "Year", values = c("2023" = "blue", "2022" = "orange")) +
  scale_color_manual(name = "Year", values = c("2023" = "darkblue", "2022" = "darkorange")) +
  theme_minimal()

ggsave("sb_hist.pdf", sb_hist, path = "course-paper-or-presentation-lukenoel20/manuscript", height=4.2, width=6)
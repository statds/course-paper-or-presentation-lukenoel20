library(ggplot2)
library(gridExtra)

p_2023 <- read.csv("course-paper-or-presentation-lukenoel20/data/players_2023.csv")
p_2022 <- read.csv("course-paper-or-presentation-lukenoel20/data/players_2022.csv")

# Combine both datasets for comparison
df_combined <- rbind(cbind(p_2023, Season = "2023"), cbind(p_2022, Season = "2022"))

df_combined$year <- as.factor(df_combined$year)

# Create boxplots for each variable 
plot_strikeout <- ggplot(df_combined, aes(x=year, y=strikeout, fill=year)) +
  geom_boxplot() +
  ggtitle("Strikeouts") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

plot_walk <- ggplot(df_combined, aes(x=year, y=walk, fill=year)) +
  geom_boxplot() +
  ggtitle("Walks") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

p_ba <- ggplot(df_combined, aes(x=year, y=batting_avg, fill=year)) +
  geom_boxplot() +
  ggtitle("Batting Avg") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

p_slg <- ggplot(df_combined, aes(x=year, y=slg_percent, fill=year)) +
  geom_boxplot() +
  ggtitle("SLG Percent") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

p_babip <- ggplot(df_combined, aes(x=year, y=babip, fill=year)) +
  geom_boxplot() +
  ggtitle("BABIP") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

p_sb <- ggplot(df_combined, aes(x=year, y=r_total_stolen_base, fill=year)) +
  geom_boxplot() +
  ggtitle("Stolen Bases") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

p_dp <- ggplot(df_combined, aes(x=year, y=b_gnd_into_dp, fill=year)) +
  geom_boxplot() +
  ggtitle("Ground Into DP") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

p_hgrd <- ggplot(df_combined, aes(x=year, y=b_hit_ground, fill=year)) +
  geom_boxplot() +
  ggtitle("Hits on Ground") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

p_xba <- ggplot(df_combined, aes(x=year, y=xba, fill=year)) +
  geom_boxplot() +
  ggtitle("XBA") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Arrange the plots in a 3x3 grid
grid_boxplots <- grid.arrange(
  plot_strikeout, plot_walk, p_ba,
  p_slg, p_babip, p_sb,
  p_dp, p_hgrd, p_xba,
  ncol = 3
)

ggsave("boxplots.pdf", grid_boxplots, path = "course-paper-or-presentation-lukenoel20/manuscript", height=8, width=6)
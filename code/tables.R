library(xtable)

p_2023 <- read.csv("course-paper-or-presentation-lukenoel20/data/players_2023.csv")
p_2022 <- read.csv("course-paper-or-presentation-lukenoel20/data/players_2022.csv")
g_times <- read.csv("course-paper-or-presentation-lukenoel20/data/game_times.csv")

# Performing a two-sample t-test and Wilcoxon Rank Sum test on each variable (game length is separate) and storing them for table later
# Stolen Bases
one_t <- t.test(p_2023$r_total_stolen_base, p_2022$r_total_stolen_base, alternative = 'greater')
one_w <- wilcox.test(p_2023$r_total_stolen_base, p_2022$r_total_stolen_base, alternative = 'greater')
r_one <- c(as.numeric(one_t$estimate[1]), as.numeric(one_t$estimate[2]), one_t$p.value, one_w$p.value)

# Strikeouts
two_t <- t.test(p_2023$strikeout, p_2022$strikeout)
two_w <- wilcox.test(p_2023$strikeout, p_2022$strikout)
r_two <- c(as.numeric(two_t$estimate[1]), as.numeric(two_t$estimate[2]), two_t$p.value, two_w$p.value)

# Walks
three_t <- t.test(p_2023$walk, p_2022$walk)
three_w <- wilcox.test(p_2023$walk, p_2022$walk)
r_three <- c(as.numeric(three_t$estimate[1]), as.numeric(three_t$estimate[2]), three_t$p.value, three_w$p.value)

# Batting Average
four_t <- t.test(p_2023$batting_avg, p_2022$batting_avg)
four_w <- wilcox.test(p_2023$batting_avg, p_2022$batting_avg)
r_four <- c(as.numeric(four_t$estimate[1]), as.numeric(four_t$estimate[2]), four_t$p.value, four_w$p.value)

# Slugging Percentage
five_t <- t.test(p_2023$slg_percent, p_2022$slg_percent)
five_w <- wilcox.test(p_2023$slg_percent, p_2022$slg_percent)
r_five <- c(as.numeric(five_t$estimate[1]), as.numeric(five_t$estimate[2]), five_t$p.value, five_w$p.value)

# BABIP
six_t <- t.test(p_2023$babip, p_2022$babip)
six_w <- wilcox.test(p_2023$babip, p_2022$babip)
r_six <- c(as.numeric(six_t$estimate[1]), as.numeric(six_t$estimate[2]), six_t$p.value, six_w$p.value)

# xBA
seven_t <- t.test(p_2023$xba, p_2022$xba)
seven_w <- wilcox.test(p_2023$xba, p_2022$xba)
r_seven <- c(as.numeric(seven_t$estimate[1]), as.numeric(seven_t$estimate[2]), seven_t$p.value, seven_w$p.value)

# Hits on Ground Balls
eight_t <- t.test(p_2023$b_hit_ground, p_2022$b_hit_ground)
eight_w <- wilcox.test(p_2023$b_hit_ground, p_2022$b_hit_ground)
r_eight <- c(as.numeric(eight_t$estimate[1]), as.numeric(eight_t$estimate[2]), eight_t$p.value, eight_w$p.value)

# Ground into Double Play (GIDP)
nine_t <- t.test(p_2023$b_gnd_into_dp, p_2022$b_gnd_into_dp)
nine_w <- wilcox.test(p_2023$b_gnd_into_dp, p_2022$b_gnd_into_dp)
r_nine <- c(as.numeric(nine_t$estimate[1]), as.numeric(nine_t$estimate[2]), nine_t$p.value, nine_w$p.value)

# Creating the table 1
cols <- c(2023, 2022, "t-test P-value", "Nonparametric P-value", "Adj P-value t", "Adj P-value W")
rows <- c("Stolen Bases", "Strikeouts", "Walks", "Batting Avg", "Slugging %",
          "BABIP", "xBA", "Hits on GB", "GIDP")
table_df <- data.frame(r_one, r_two, r_three, r_four, r_five, r_six,
                    r_seven, r_eight, r_nine, stringsAsFactors = FALSE)
table_final <- data.frame(t(table_df))
first <- round(table_final[1:2], 3)
second <- round(table_final[3:4], 4)
third <- round(table_final[3:4]*9, 4)
table_final <-cbind(first, second, third)
colnames(table_final) <- cols
row.names(table_final) <- rows

# Getting Latex code for table 1
# only want subset of cols in table not all
selected_columns <- c("2023", "2022", "t-test P-value", "Nonparametric P-value")

# Subset the data frame
table_data <- table_final[, selected_columns]

# Create an xtable object
table_object <- xtable(table_final)

# Print the LaTeX code for the table
print(table_object, include.rownames = TRUE)
#TABLE WAS CLEANED UP IN TEX FILE (with help from \citet{Price} repo)

# NOW STARTING TABLE 2 (GAME LENGTH TABLE)



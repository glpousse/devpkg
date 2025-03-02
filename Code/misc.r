########################################################
# This code replicates Figure 1 from Mehmood 2022, AER 
########################################################

library(haven)
library(dplyr)
library(dplyr)
library(ggplot2)
set.seed(123456)

df <- read_dta("./Input/DataTeam1.dta")

controls_case <- c("constitutional", "criminal", "land_case", "pagesjudgenum", "benchchiefjustice", "lawyer_number", 
                   "judge_number")
controls_judge <- c("TenureatDecision", "Gender", "Plot_Alloted", "ElevatedtoSupremeCourt", "Former_LowerCourtJudge",
                    "Former_MemberBarAssociation", "Prior_Political_Office", "Former_Lawyer", "Vote_share", "Muslim",
                     "Punjabi", "Sindhi", "Balochi", "Pashtun", "Others")
controls_district <- c("Area_sqkm", "Population", "Density_persqkm", "Agri_production", "avg_mean")
controls_bench <- c("TotalJudges_bench", "Criminal_count_bench", "Land_count_per_bench", "Human_count_per_bench")

# Panel A 

df1 <- df %>%
  mutate(period = ifelse(yeardecision < 2010, 0, 1))

# Step 3: Collapse the data by 'period' and calculate mean, sd, and count
df1_collapsed <- df1 %>%
  group_by(period) %>%
  summarize(
    meanStateWins = mean(StateWins, na.rm = TRUE),
    sdStateWins = sd(StateWins, na.rm = TRUE),
    n = n()
  )

# Step 4: Calculate confidence intervals
df1_collapsed <- df1_collapsed %>%
  mutate(
    hiStateWins = meanStateWins + qt(0.975, n - 1) * (sdStateWins / sqrt(n)),
    loStateWins = meanStateWins - qt(0.975, n - 1) * (sdStateWins / sqrt(n))
  )

# Step 5: Create the bar graph with error bars
ggplot(df1_collapsed, aes(x = factor(period), y = meanStateWins)) +
  geom_bar(stat = "identity", fill = "navyblue") +
  geom_errorbar(aes(ymin = loStateWins, ymax = hiStateWins), width = 0.2, color = "red") +
  labs(
    title = "Average State Wins",
    subtitle = "(Before and After the 2010 Reform)",
    y = "Average State Wins",
    x = NULL
  ) +
  scale_x_discrete(labels = c("Pre-Reform (1986-2009)", "Post-Reform (2010-2019)")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_blank()
  )

# Step 6: Save the graph as PNG
ggsave("./Output/Figures/Figure_1A.png", width = 10, height = 7, dpi = 300)


library(devtools)

# create a package `here`
create_package("/Users/glpou/Documents/SCIENCESPO/M2/S4/Development/Replication/Rossi.2022AER.pkg")
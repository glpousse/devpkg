rm(list = ls())
library(haven)
library(tidyverse)
library(patchwork)

###################
###### Setup ###### 
###################

df_origin <- read_dta("Input/temp/devacc.dta")

percentiles <- c(0.01, 0.05, 0.10, 0.15, 0.20, 0.30, 0.45, 0.50, 0.85, 0.90, 0.99)

###### Making the "Micro" Sample for plot 1 ######

micro_sample <- df_origin %>% filter(country == "India" | country == "Indonesia" | country == "Jamaica" | country == "Brazil")
micro_stats <- c("y_PRusm", "y_PusmR_j", "y_PusmR_cc", "y_PusmR")

micro_sample_long <- micro_sample %>% 
    pivot_longer(cols = all_of(micro_stats), 
    names_to = "Statistics",
    values_to = "Value"
)

all(c("country", "Statistics", "Value") %in% colnames(micro_sample_long)) # if TRUE -> safe to proceed

micro_sample_long <- micro_sample_long %>% select(country, Statistics, Value)

###### Making the "Broad" Sample for plot 2 ######

broad_sample <- df_origin

for(p in percentiles){
    broad_sample[[paste0("l_y_p", p * 100)]] <- quantile(broad_sample$l_y, probs = p, na.rm = TRUE, type = 2)
}

broad_sample <- broad_sample %>% filter(l_y==l_y_p1 | l_y==l_y_p5 | l_y==l_y_p10 | l_y==l_y_p15)

broad_stats <- c("y_PRus", "y_PusR_j", "y_PusR_cc", "y_PusR")
broad_sample_long <- broad_sample %>% 
    pivot_longer(cols = all_of(broad_stats), 
    names_to = "Statistics",
    values_to = "Value"
)

all(c("country", "Statistics", "Value") %in% colnames(broad_sample_long)) # if TRUE -> safe to proceed

broad_sample_long <- broad_sample_long %>% select(country, Statistics, Value)

######################
###### Figure 8 ###### 
######################

plot1 <- ggplot(micro_sample_long, aes(x = country, y = Value, fill = Statistics)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    labs(
        title = "",
        x = "Country", 
        y = "Relative GDP per worker (US = 1)",
        fill = "Statistics"
    ) +
    theme_minimal()


plot2 <- ggplot(broad_sample_long, aes(x = country, y = Value, fill = Statistics)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black") + 
    labs(
        title = "",
        x = "Country",
        y = "",
        fill = "Statistics"
    ) +
    theme_minimal()

sum(is.na(micro_sample_long))


test <- ggplot(broad_sample_long, aes(x = country, y = Value)) + 
    geom_bar(stat = "identity")




broad_sample_long$country <- as.character(broad_sample_long$country)  # Ensure it's a clean character vector
attr(broad_sample_long$country, "label") <- NULL
attr(broad_sample_long$country, "format.stata") <- NULL
str(broad_sample_long$country)

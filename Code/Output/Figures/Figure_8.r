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

# I need long format for grouped bar charts 

micro_sample_long <- micro_sample %>% 
    pivot_longer(cols = all_of(micro_stats), 
    names_to = "Statistics",
    values_to = "Value"
)

all(c("country", "Statistics", "Value") %in% colnames(micro_sample_long)) # if TRUE -> safe to proceed

micro_sample_long <- micro_sample_long %>% select(country, Statistics, Value, l_y)

# Ordering the stats the reproduce the same order as in the paper's figure

micro_sample_long$Statistics <- factor(micro_sample_long$Statistics, 
                                       levels = c("y_PRusm", "y_PusmR_j", "y_PusmR_cc", "y_PusmR")) 

###### Making the "Broad" Sample for plot 2 ######

broad_sample <- df_origin

for(p in percentiles){
    broad_sample[[paste0("l_y_p", p * 100)]] <- quantile(broad_sample$l_y, probs = p, na.rm = TRUE, type = 2)
}

broad_sample <- broad_sample %>% filter(l_y==l_y_p1 | l_y==l_y_p5 | l_y==l_y_p10 | l_y==l_y_p15)

broad_stats <- c("y_PRus", "y_PusR_j", "y_PusR_cc", "y_PusR")

# I need long format for grouped bar charts 

broad_sample_long <- broad_sample %>% 
    pivot_longer(cols = all_of(broad_stats), 
    names_to = "Statistics",
    values_to = "Value"
)

all(c("country", "Statistics", "Value") %in% colnames(broad_sample_long)) # if TRUE -> safe to proceed

broad_sample_long <- broad_sample_long %>% select(country, Statistics, Value, l_y)

broad_sample_long$Statistics <- factor(broad_sample_long$Statistics, 
                                       levels = c("y_PRus", "y_PusR_j", "y_PusR_cc", "y_PusR")
)

# Labelling the countries which were kept due to the percentile match

broad_sample_long$country_label <- broad_sample_long$country  

broad_sample_long$country_label[broad_sample_long$country == "Liberia"] <- "Liberia\n(1st perc)"
broad_sample_long$country_label[broad_sample_long$country == "Tanzania"] <- "Tanzania\n(5th perc)"
broad_sample_long$country_label[broad_sample_long$country == "Vietnam"] <- "Vietnam\n(10th perc)"
broad_sample_long$country_label[broad_sample_long$country == "El Salvador"] <- "El Salvador\n(15th perc)"


######################
###### Figure 8 ###### 
######################

plot1 <- ggplot(micro_sample_long, aes(x = reorder(country, l_y), y = Value, fill = Statistics)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5)) + 
    scale_fill_manual(
        values = c(
        "y_PRusm" = "grey10",
        "y_PusmR_j" = "grey40",
        "y_PusmR_cc"= "grey70",
        "y_PusmR"= "grey100"), 
        labels = c("y_PRusm" = "Data",
        "y_PusmR_j" = expression("Counterfactual, " ~ theta[Q] == theta[AQ]),
        "y_PusmR" = expression("Counterfactual, " ~ theta[Q] == 0),
        "y_PusmR_cc" = "Counterfactual, migrant-based calibration"),
        guide = guide_legend(nrow = 2, ncol = 2)
    ) +  
    labs(
        title = "Micro-data Sample",
        x = "", 
        y = "Relative GDP per worker (US = 1)",
        fill = ""
    ) +
    theme_minimal() + 
    theme(
        plot.title = element_text(size = 15, hjust = 0.5), 
        legend.position = "bottom", 
        aspect.ratio = 1,
        panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank(),   
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),  
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.background = element_rect(color = "black", fill = "white"),
    axis.line = element_line(color = "black", linewidth = 0.5), 
    axis.ticks = element_line(color = "black", linewidth = 0.5)  
) 

plot2 <- ggplot(broad_sample_long, aes(x = reorder(country_label, l_y), y = Value, fill = Statistics)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_y_continuous(limits = c(0, 0.51), breaks = seq(0, 0.51, by = 0.1)) + 
    scale_fill_manual(
        values = c(
        "y_PRus" = "grey10",
        "y_PusR_j" = "grey40",
        "y_PusR_cc" = "grey70",
        "y_PusR" = "grey100"), 
        labels = c("y_PRus" = "Data",
        "y_PusR_j" = expression("Counterfactual, " ~ theta[Q] == theta[AQ]),
        "y_PusR" = expression("Counterfactual, " ~ theta[Q] == 0),
        "y_PusR_cc" = "Counterfactual, migrant-based calibration"),
        guide = guide_legend(nrow = 2, ncol = 2)
    ) +  
    labs(
        title = "Broad Sample",
        x = "", 
        y = "",
        fill = ""
    ) +
    theme_minimal() + 
    theme(
        plot.title = element_text(size = 15, hjust = 0.5), 
        legend.position = "bottom", 
        aspect.ratio = 1,
        panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank(),   
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),  
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.background = element_rect(color = "black", fill = "white"),
    axis.line = element_line(color = "black", linewidth = 0.5), 
    axis.ticks = element_line(color = "black", linewidth = 0.5)  
) 

figure_8 <- plot1 + plot2 +
    plot_layout(ncol = 2, guides = "collect") &  
    theme(legend.position = "bottom", legend.justification = "center"
)

ggsave("Output/Figures/figure_8.pdf", plot = figure_8, width = 14, height = 7)
rm(list = ls())
library(haven)
library(tidyverse)
library(patchwork)

#########################
###### Definitions ###### 
#########################

countries <- c("brazil", "canada", "india", "indonesia", "israel", 
                "jamaica", "mexico", "panama", "tt", "uruguay", "venezuela", "usa"
)

for(country in countries){
    assign(paste0("df_", country), read_dta(paste0("Input/temp/emphrs_", country,".dta"))) 
}

pwt_dta <- read_dta("Input/data/pwt/gdp_pwt.dta")

#######################
###### Appending ###### 
#######################

df_append <- data.frame()

for(country in countries){
    temp_data <- get(paste0("df_", country))
    df_append <- bind_rows(df_append, temp_data)  # Append data
} 

df_append <- df_append %>% arrange(country)

#####################
###### Merging ###### 
#####################

df_merge <- df_append %>%
    inner_join(pwt_dta, by = c("country", "year")) %>% 
    arrange(country
)

######################################
###### Standardizing Time Units ###### 
######################################

df_merge <- df_merge %>%
    mutate(across(starts_with("hours_skti_"), ~ ifelse(country == "United States", . / 52, .))
)
######################
###### Figure 2 ###### 
######################

plot1 <- ggplot(df_merge, aes(x = l_y)) +  

    geom_point(aes(y = hours_skti_0, color = "Low-skill"), size = 2) +  
    geom_point(aes(y = hours_skti_1, color = "High-skill"), size = 2, shape = 4) +  
    geom_text(aes(y = hours_skti_0, label = countrycode), vjust = 0, hjust = -0.5, size = 3, color = "grey20") +  
    geom_text(aes(y = hours_skti_1, label = countrycode), vjust = 0, hjust = -0.5, size = 3, color = "grey40") +  
    geom_smooth(aes(y = hours_skti_0), method = "lm", color = "grey20", linetype = "solid", se = FALSE) +  
    geom_smooth(aes(y = hours_skti_1), method = "lm", color = "grey40", linetype = "dashed", se = FALSE) +  

    labs(
        x = "Log GDP p.w.",
        y = "Weekly Hours Worked per Employed Worker",
        color = ""
    ) +

    scale_color_manual(values = c("Low-skill" = "grey20", "High-skill" = "grey40")) +
    theme_minimal() + 

    theme(
        legend.position = "bottom", 
        aspect.ratio = 1,
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),  
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 14),
        legend.background = element_rect(color = "black", fill = "white"),
    axis.line = element_line(color = "black", linewidth = 0.5), 
    axis.ticks = element_line(color = "black", linewidth = 0.5)  
    ) +
    
    coord_cartesian(xlim = c(min(df_merge$l_y), max(df_merge$l_y) + 0.2), expand = TRUE
)  

plot2 <- ggplot(df_merge, aes(x = l_y)) + 

    geom_point(aes(y = emprate_skti_0, color = "Low-skill"), size = 2)+
    geom_point(aes(y = emprate_skti_1, color = "High-skill"), size = 2, shape = 4) +
    geom_text(aes(y = emprate_skti_0, label = countrycode), vjust = 0, hjust = -0.5, size = 3, color = "grey20") +
    geom_text(aes(y = emprate_skti_1, label = countrycode), vjust = 0, hjust = -0.5, size = 3, color = "grey40") +
    geom_smooth(aes(y = emprate_skti_0), method = "lm", color = "grey20", linetype = "solid", se = FALSE) +
    geom_smooth(aes(y = emprate_skti_1), method = "lm", color = "grey40", linetype = "dashed", se = FALSE) +

    labs(
        x = "Log GDP p.w.",
        y = "Employment Rate",
        color = ""
    ) +

    scale_color_manual(values = c("Low-skill" = "grey20", "High-skill" = "grey40")) +
    
    theme_minimal() + 
    theme(
        legend.position = "bottom", 
        aspect.ratio = 1,
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),  
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 14),
        legend.background = element_rect(color = "black", fill = "white"),
    axis.line = element_line(color = "black", linewidth = 0.5),  
    axis.ticks = element_line(color = "black", linewidth = 0.5) 
    ) +

    coord_cartesian(xlim = c(min(df_merge$l_y), max(df_merge$l_y) + 0.2), expand = TRUE
)  

figure_2 <- plot1 + plot2 + 
    plot_layout(ncol = 2, guides = "collect") &  
    theme(legend.position = "bottom", legend.justification = "center")

ggsave("Output/Figures/figure_2.pdf", plot = figure_2, width = 14, height = 7)
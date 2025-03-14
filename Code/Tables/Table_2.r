rm(list = ls())
library(haven)
library(tidyr)
library(dplyr)
library(kableExtra)

#########################
###### Definitions ###### 
#########################

df2_orig <- read_dta("Input/temp/AQ.dta")

df <- df2_orig %>%
  filter(sample_micro == 1 & !is.na(irAQ53_dum_skti_hrs_secall) & year == 2000) %>%
  arrange(l_y)

log_vars <- c("wrat53_dum_skti_secall", "H5L3_dum_skti_hrs_secall", "irAQ53_dum_skti_hrs_secall",
              "irAQ53rl_dum_skti_hrs_secall", "irAQ53rh_dum_skti_hrs_secall", 
              "wrat53_dumx_skti_secall", "H5L3_dumx_skti_hrs_secall", "irAQ53_dumx_skti_hrs_secall",
              "irAQ53rl_dumx_skti_hrs_secall", "irAQ53rh_dumx_skti_hrs_secall", 
              "wrat53_dumse_skti_secall", "H5L3_dumse_skti_hrs_secall", "irAQ53_dumse_skti_hrs_secall",
              "irAQ53rl_dumse_skti_hrs_secall", "irAQ53rh_dumse_skti_hrs_secall", 
              "wrat53_dum_skti_sec1", "H5L3_dum_skti_hrs_sec1", "irAQ53_dum_skti_hrs_sec1", 
              "irAQ53rl_dum_skti_hrs_sec1", "irAQ53rh_dum_skti_hrs_sec1", 
              "wrat53_dum_skti_sec2", "H5L3_dum_skti_hrs_sec2", "irAQ53_dum_skti_hrs_sec2", 
              "irAQ53rl_dum_skti_hrs_sec2", "irAQ53rh_dum_skti_hrs_sec2", 
              "wrat53_dum_skti_sec3", "H5L3_dum_skti_hrs_sec3", "irAQ53_dum_skti_hrs_sec3", 
              "irAQ53rl_dum_skti_hrs_sec3", "irAQ53rh_dum_skti_hrs_sec3", 
              "wrat53_dum_skti_sec4", "H5L3_dum_skti_hrs_sec4", "irAQ53_dum_skti_hrs_sec4", 
              "irAQ53rl_dum_skti_hrs_sec4", "irAQ53rh_dum_skti_hrs_sec4"
)

for (var in log_vars) {
  df <- df %>%
    mutate(!!paste0("l_", var) := log(get(var)))
}

rr <- list()

#########################
###### Regressions ###### 
#########################

# Regressions for Row 1
rr$ey11 <- lm(l_wrat53_dum_skti_secall ~ l_y, data = df)
rr$ey12 <- lm(l_H5L3_dum_skti_hrs_secall ~ l_y, data = df)
rr$ey13 <- lm(l_irAQ53_dum_skti_hrs_secall ~ l_y, data = df)
rr$ey14 <- lm(l_irAQ53rl_dum_skti_hrs_secall ~ l_y, data = df)
rr$ey15 <- lm(l_irAQ53rh_dum_skti_hrs_secall ~ l_y, data = df)
# Regressions for Row 2
rr$ey21 <- lm(l_wrat53_dumx_skti_secall ~ l_y, data = df)
rr$ey22 <- lm(l_H5L3_dumx_skti_hrs_secall ~ l_y, data = df)
rr$ey23 <- lm(l_irAQ53_dumx_skti_hrs_secall ~ l_y, data = df)
rr$ey24 <- lm(l_irAQ53rl_dumx_skti_hrs_secall ~ l_y, data = df)
rr$ey25 <- lm(l_irAQ53rh_dumx_skti_hrs_secall ~ l_y, data = df)
# Regressions for Row 3
rr$ey31 <- lm(l_wrat53_dum_skti_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
rr$ey32 <- lm(l_H5L3_dum_skti_hrs_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
rr$ey33 <- lm(l_irAQ53_dum_skti_hrs_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
rr$ey34 <- lm(l_irAQ53rl_dum_skti_hrs_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
rr$ey35 <- lm(l_irAQ53rh_dum_skti_hrs_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
# Regressions for Row 4
rr$ey41 <- lm(l_wrat53_dumse_skti_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
rr$ey42 <- lm(l_H5L3_dumse_skti_hrs_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
rr$ey43 <- lm(l_irAQ53_dumse_skti_hrs_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
rr$ey44 <- lm(l_irAQ53rl_dumse_skti_hrs_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
rr$ey45 <- lm(l_irAQ53rh_dumse_skti_hrs_secall ~ l_y, data = df %>% filter(!is.na(irAQ53_dumse_skti_hrs_secall)))
# Regressions for Row 5 
rr$ey51 <- lm(l_wrat53_dum_skti_sec1 ~ l_y, data = df)
rr$ey52 <- lm(l_H5L3_dum_skti_hrs_sec1 ~ l_y, data = df)
rr$ey53 <- lm(l_irAQ53_dum_skti_hrs_sec1 ~ l_y, data = df)
rr$ey54 <- lm(l_irAQ53rl_dum_skti_hrs_sec1 ~ l_y, data = df)
rr$ey55 <- lm(l_irAQ53rh_dum_skti_hrs_sec1 ~ l_y, data = df)
# Regressions for Row 6
rr$ey61 <- lm(l_wrat53_dum_skti_sec2 ~ l_y, data = df)
rr$ey62 <- lm(l_H5L3_dum_skti_hrs_sec2 ~ l_y, data = df)
rr$ey63 <- lm(l_irAQ53_dum_skti_hrs_sec2 ~ l_y, data = df)
rr$ey64 <- lm(l_irAQ53rl_dum_skti_hrs_sec2 ~ l_y, data = df)
rr$ey65 <- lm(l_irAQ53rh_dum_skti_hrs_sec2 ~ l_y, data = df)
# Regressions for Row 7 
rr$ey71 <- lm(l_wrat53_dum_skti_sec3 ~ l_y, data = df)
rr$ey72 <- lm(l_H5L3_dum_skti_hrs_sec3 ~ l_y, data = df)
rr$ey73 <- lm(l_irAQ53_dum_skti_hrs_sec3 ~ l_y, data = df)
rr$ey74 <- lm(l_irAQ53rl_dum_skti_hrs_sec3 ~ l_y, data = df)
rr$ey75 <- lm(l_irAQ53rh_dum_skti_hrs_sec3 ~ l_y, data = df)
# Regressions for Row 8 
rr$ey81 <- lm(l_wrat53_dum_skti_sec4 ~ l_y, data = df)
rr$ey82 <- lm(l_H5L3_dum_skti_hrs_sec4 ~ l_y, data = df)
rr$ey83 <- lm(l_irAQ53_dum_skti_hrs_sec4 ~ l_y, data = df)
rr$ey84 <- lm(l_irAQ53rl_dum_skti_hrs_sec4 ~ l_y, data = df)
rr$ey85 <- lm(l_irAQ53rh_dum_skti_hrs_sec4 ~ l_y, data = df)

###################
###### Setup ###### 
###################

# I'm going to make one df for the coeffs, one df for the SE's, then I'll itnerleave them 
# to make one main df. Then I will use kaggleExtra to make it a Latex table. 

df_coeff <- data.frame(matrix(1:48, nrow = 8, ncol = 6))
df_SE <- data.frame(matrix(1:48, nrow = 8, ncol = 6))

df_coeff$X1 <- c("1. Baseline", "2. Experience and gender", "3. Baseline (self-employment sample)", 
                 "4. Self-employment", "5. Agriculture", "6. Manufacturing", "7. Low-skill services", 
                 "8. High-skill services"
)

df_SE$X1 <- c("1. Baseline", "2. Experience and gender", "3. Baseline (self-employment sample)", 
                 "4. Self-employment", "5. Agriculture", "6. Manufacturing", "7. Low-skill services", 
                 "8. High-skill services"
)

for (i in 1:8){
  model_name1     <- paste0("ey", i, "1")
  model1          <- rr[[model_name1]]
  df_coeff[i, 2]  <- coef(model1)["l_y"]
  df_SE[i,2]   <- summary(model1)$coef["l_y", "Std. Error"]

  model_name2     <- paste0("ey", i, "2")
  model2          <- rr[[model_name2]]
  df_coeff[i, 3]  <- coef(model2)["l_y"]
  df_SE[i,3]   <- summary(model2)$coef["l_y", "Std. Error"]

  model_name3     <- paste0("ey", i, "3")
  model3          <- rr[[model_name3]]
  df_coeff[i, 4]  <- coef(model3)["l_y"]
  df_SE[i,4]   <- summary(model3)$coef["l_y", "Std. Error"]

  model_name4     <- paste0("ey", i, "4")
  model4          <- rr[[model_name4]]
  df_coeff[i, 5] <- coef(model4)["l_y"]
  df_SE[i,5]  <- summary(model4)$coef["l_y", "Std. Error"]

  model_name5     <- paste0("ey", i, "5")
  model5          <- rr[[model_name5]]
  df_coeff[i, 6] <- coef(model5)["l_y"]
  df_SE[i,6]  <- summary(model5)$coef["l_y", "Std. Error"]
}

# Now I interleave the df_coeff and df_SE into one df

df_table <- data.frame()

for (rowname in df_coeff$X1) {
  df_table <- bind_rows(df_table, 
                              df_coeff %>% filter(X1 == rowname),
                              df_SE %>% filter(X1 == rowname)%>% mutate(X1 = ""))
}

# Adding the brackets and rounding

df_table <- df_table %>%
  mutate(across(where(is.numeric), round, digits = 3)) %>%  
  mutate(across(-X1, ~ ifelse(row_number() %% 2 == 0, paste0("[", ., "]"), .))
)  

#####################
###### Table 2 ###### 
#####################

latex_table <- df_table %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, align = c("l", rep("c", ncol(df_table) - 1)),
      col.names = c("", "$\\theta_W$", "$\\theta_{\\tilde{H}/\\tilde{L}}$", "$\\sigma = 1.5$", "$\\sigma = 1.3$", "$\\sigma = 1.2$"),
      caption = "Skill Premium, Supply, and Efficiency Across Countries: Robustness") %>%
  add_header_above(c(" " = 3, "$\\\\theta_{AQ}$" = 3), bold = FALSE, italic = TRUE, escape = FALSE) %>%
  kable_styling(font_size = 9, position = "center") %>%
  row_spec(seq(2, nrow(df_table), by = 2), extra_latex_after = "\\addlinespace[5pt]") %>% 
  add_footnote(c("\\scriptsize Notes: This table replicates Table 2 from Rossi (2022). It shows the elasticities of the skill premium, relative skill supply, and relative skill efficiency with respect to GDP per worker (standard errors in brackets). The elasticities are computed using data for the 12 countries in the microdata sample, with the exceptions of rows 3 and 4 for which only the 8 countries with self-employment data are used. The code used to replicate this table can be found \\underline{\\href{https://github.com/glpousse/devpkg/blob/main/Code/Tables/Table_2.r}{here}}."), notation = getOption("kable_footnote_notation", "none"), escape = FALSE, threeparttable = TRUE
)

write(latex_table, file = "Output/Tables/Table_2.tex")

# Unwanted "\addlinespace"s appear in the written script. I remove them manually. 

latex_file <- "Output/Tables/Table_2.tex"
lines <- readLines(latex_file)

lines_to_remove <- c(20, 29, 37)

# Keep only the lines that are NOT in the list
filtered_lines <- lines[-lines_to_remove]

# Write back to the LaTeX file
writeLines(filtered_lines, latex_file)

##### Table 2 from Rossi, 2022 (AER) completed #####
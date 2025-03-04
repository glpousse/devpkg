rm(list = ls())
library(haven)
library(lm)
library(dplyr)
library(stargazer)
library(textreg)
library(broom)

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
              "irAQ53rl_dum_skti_hrs_sec4", "irAQ53rh_dum_skti_hrs_sec4")

for (var in log_vars) {
  df <- df %>%
    mutate(!!paste0("l_", var) := log(get(var)))
}

rr <- list()

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


# Exporting 1
stargazer(rr$ey11, rr$ey12, rr$ey13, rr$ey14, rr$ey15, 
          type = "html", 
          out = "Output/Tables/tab_2_row1.html", 
          dep.var.labels = "Baseline", 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          digits = 3)
# Exporting 2
stargazer(rr$ey21, rr$ey22, rr$ey23, rr$ey24, rr$ey25, 
          type = "html", 
          out = "output/tables/tab_2_row2.html", 
          dep.var.labels = "Experience and Gender", 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          digits = 3)
# Exporting 3
stargazer(rr$ey31, rr$ey32, rr$ey33, rr$ey34, rr$ey35, 
          type = "html", 
          out = "output/tables/tab_2_row3.html", 
          dep.var.labels = "Baseline (Self-Employment Sample)", 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          digits = 3)
# Exporting 4
stargazer(rr$ey41, rr$ey42, rr$ey43, rr$ey44, rr$ey45, 
          type = "html", 
          out = "output/tables/tab_2_row4.html", 
          dep.var.labels = "Self-Employment Sample", 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          digits = 3)
# Exporting 5
stargazer(rr$ey51, rr$ey52, rr$ey53, rr$ey54, rr$ey55, 
          type = "html", 
          out = "output/tables/tab_2_row5.html", 
          dep.var.labels = "Agriculture", 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          digits = 3)
# Exporting 6
stargazer(rr$ey61, rr$ey62, rr$ey63, rr$ey64, rr$ey65, 
          type = "html", 
          out = "output/tables/tab_2_row6.html", 
          dep.var.labels = "Manufacturing", 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          digits = 3)
# Exporting 7
stargazer(rr$ey71, rr$ey72, rr$ey73, rr$ey74, rr$ey75, 
          type = "html", 
          out = "output/tables/tab_2_row7.html", 
          dep.var.labels = "Low-Skill Services", 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          digits = 3)
# Exporting 8
stargazer(rr$ey81, rr$ey82, rr$ey83, rr$ey84, rr$ey85, 
          type = "html", 
          out = "output/tables/tab_2_row8.html", 
          dep.var.labels = "High-Skill Services", 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          digits = 3)

# Table 2 

# Still need to write some code to bring it all under one neat table. 
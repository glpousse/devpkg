library(haven)
library(dyplr)
library(kableExtra)
library(lm)

# Definitions need to be set. Then, blocks for eac row can be run consecutively, or independently, 
# to find the coefficients for each row of Table 4. 

#########################
###### Definitions ###### 
#########################

df3_orig <- read_dta("Input/temp/Q.dta")

log_vars1 <- c("irAQ53_blee_skti", "irAQ53rh_blee_skti", "irAQ53rl_blee_skti", 
          "irAQ53_dum_skti_hrs_secall", "irAQ53rh_dum_skti_hrs_secall", 
          "irAQ53rl_dum_skti_hrs_secall"
)

log_vars2 <- c("irQ53sel_dum", "irAQ53_blee_skti", "irAQ53rh_blee_skti", 
          "irAQ53rl_blee_skti", "irAQ53_dum_skti_hrs_secall", "irAQ53rh_dum_skti_hrs_secall", 
          "irAQ53rl_dum_skti_hrs_secall"
)

log_vars3 <- c("irQ53yh_dum", "irAQ53_blee_skti", "irAQ53rh_blee_skti",
          "irAQ53rl_blee_skti", "irAQ53_dum_skti_hrs_secall", "irAQ53rh_dum_skti_hrs_secall",
          "irAQ53rl_dum_skti_hrs_secall"
)

log_vars4 <- c("irQ53goodeng_dum", "irAQ53_blee_skti", "irAQ53rh_blee_skti", 
          "irAQ53rl_blee_skti", "irAQ53_dum_skti_hrs_secall", "irAQ53rh_dum_skti_hrs_secall", 
          "irAQ53rl_dum_skti_hrs_secall"
)

log_vars5 <- c("irQ53nodown_dum", "irQ53nomism_dum", "irAQ53_blee_skti", "irAQ53rh_blee_skti",
          "irAQ53rl_blee_skti", "irAQ53_dum_skti_hrs_secall", "irAQ53rh_dum_skti_hrs_secall", 
          "irAQ53rl_dum_skti_hrs_secall"
)

log_vars6 <- c("irQ53sorts_dum", "irQ53sortr_dum", "irAQ53_blee_skti", "irAQ53rh_blee_skti", 
          "irAQ53rl_blee_skti", "irAQ53_dum_skti_hrs_secall", "irAQ53rh_dum_skti_hrs_secall", 
          "irAQ53rl_dum_skti_hrs_secall")

###################
###### Row 1 ######  
###################
df3 <- df3_orig %>% filter(sample == "US Pooled" & sample_migr == 1)

for(var in log_vars1){
    df3 <- df3 %>% 
      mutate(!!paste0("l_", var) := log(get(var)))
}

df3 <- df3 %>%
  mutate(
    Q_1 = l_irQ53_dum,
    AQ_2 = l_irAQ53_blee_skti,
    AQ_3 = l_irAQ53rl_blee_skti,
    AQ_4 = l_irAQ53rh_blee_skti,
    Q_5 = l_irQ53_dum,
    AQ_6 = l_irAQ53_dum_skti_hrs_secall,
    AQ_7 = l_irAQ53rl_dum_skti_hrs_secall,
    AQ_8 = l_irAQ53rh_dum_skti_hrs_secall
)

Q_models_1 <- list() 
AQ_models_1 <- list() 
ratios_1 <- list()

Q_models_1$rrQ_1 <- lm(Q_1 ~ l_y, data = df3)
df3 <- df3 %>%
  mutate(b_Q_1 = coef(Q_models_1$rrQ_1)[2]
)

Q_models_1$rrQ_5 <- lm(Q_5 ~ l_y, data = df3 %>% filter(sample_micro == 1))
df3$b_Q_5 <- coef(Q_models_1$rrQ_5)[2]

for(i in c(2:4, 6:8)){
    formula <- as.formula(paste0("AQ_", i, " ~ l_y"))
    AQ_models_1[[paste0("rrAQ_", i)]] <- lm(formula, data = df3)
    df3 <- df3 %>%
      mutate(!!paste0("b_AQ_", i) := coef(AQ_models_1[[paste0("rrAQ_", i)]])[2])
}

df3$s <- (df3$b_Q_1/ df3$b_AQ_2)
ratios_1$Q1_AQ2r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_3)
ratios_1$Q1_AQ3r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_4)
ratios_1$Q1_AQ4r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_6)
ratios_1$Q5_AQ6r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_7)
ratios_1$Q5_AQ7r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_8)
ratios_1$Q5_AQ8r <- mean(df3$s, na.rm = TRUE)
# What goes into row one will be (in order) df3$b_Q_1, first three ratios, df3$b_Q_5, last three ratios. 

###################
###### Row 2 ######  
###################

df3 <- df3_orig %>% filter(sample_migr == 1 & country_obs == 1)

for(var in log_vars1){
    df3 <- df3 %>% 
      mutate(!!paste0("l_", var) := log(get(var)))
}

df3 <- df3 %>% 
  mutate(
    Q_1 = l_irQ53_pool_dum,
    AQ_2 = l_irAQ53_blee_skti,
    AQ_3 = l_irAQ53rl_blee_skti, 
    AQ_4 = l_irAQ53rh_blee_skti, 
    Q_5 = l_irQ53_pool_dum, 
    AQ_6 = l_irAQ53_dum_skti_hrs_secall, 
    AQ_7 = l_irAQ53rl_dum_skti_hrs_secall, 
    AQ_8 = l_irAQ53rh_dum_skti_hrs_secall
)

Q_models_2 <- list() 
AQ_models_2 <- list() 
ratios_2 <- list()

Q_models_2$rrQ_1 <- lm(Q_1 ~ l_y, data = df3)
df3 <- df3 %>%
  mutate(b_Q_1 = coef(Q_models_2$rrQ_1)[2]
)

Q_models_2$rrQ_5 <- lm(Q_5 ~ l_y, data = df3 %>% filter(sample_micro == 1))
df3$b_Q_5 <- coef(Q_models_2$rrQ_5)[2]

for(i in c(2:4, 6:8)){
    formula <- as.formula(paste0("AQ_", i, " ~ l_y"))
    AQ_models_2[[paste0("rrAQ_", i)]] <- lm(formula, data = df3)
    df3 <- df3 %>%
      mutate(!!paste0("b_AQ_", i) := coef(AQ_models_2[[paste0("rrAQ_", i)]])[2])
}

df3$s <- (df3$b_Q_1/ df3$b_AQ_2)
ratios_2$Q1_AQ2r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_3)
ratios_2$Q1_AQ3r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_4)
ratios_2$Q1_AQ4r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_6)
ratios_2$Q5_AQ6r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_7)
ratios_2$Q5_AQ7r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_8)
ratios_2$Q5_AQ8r <- mean(df3$s, na.rm = TRUE)

###################
###### Row 3 ######  
###################

df3 <- df3_orig %>% filter(sample_migr == 1 & country_obs == 1)

for(var in log_vars1){
    df3 <- df3 %>% 
      mutate(!!paste0("l_", var) := log(get(var)))
}

df3 <- df3 %>%
  mutate(
    Q_1 = l_irQ53_poolbl_dum,
    AQ_2 = l_irAQ53_blee_skti, 
    AQ_3 = l_irAQ53rl_blee_skti, 
    AQ_4 = l_irAQ53rh_blee_skti, 
    Q_5 = l_irQ53_poolbl_dum, 
    AQ_6 = l_irAQ53_dum_skti_hrs_secall, 
    AQ_7 = l_irAQ53rl_dum_skti_hrs_secall, 
    AQ_8 = l_irAQ53rh_dum_skti_hrs_secall
)

Q_models_3 <- list() 
AQ_models_3 <- list() 
ratios_3 <- list()

Q_models_3$rrQ_1 <- lm(Q_1 ~ l_y, data = df3)
df3 <- df3 %>%
  mutate(b_Q_1 = coef(Q_models_3$rrQ_1)[2]
)

Q_models_3$rrQ_5 <- lm(Q_5 ~ l_y, data = df3 %>% filter(sample_micro == 1))
df3$b_Q_5 <- coef(Q_models_3$rrQ_5)[2]

for(i in c(2:4, 6:8)){
    formula <- as.formula(paste0("AQ_", i, " ~ l_y"))
    AQ_models_3[[paste0("rrAQ_", i)]] <- lm(formula, data = df3)
    df3 <- df3 %>%
      mutate(!!paste0("b_AQ_", i) := coef(AQ_models_3[[paste0("rrAQ_", i)]])[2])
}

df3$s <- (df3$b_Q_1/ df3$b_AQ_2)
ratios_3$Q1_AQ2r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_3)
ratios_3$Q1_AQ3r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_4)
ratios_3$Q1_AQ4r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_6)
ratios_3$Q5_AQ6r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_7)
ratios_3$Q5_AQ7r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_8)
ratios_3$Q5_AQ8r <- mean(df3$s, na.rm = TRUE)

###################
###### Row 4 ######  
###################

df3 <- df3_orig %>% filter(sample == "US Pooled" & sample_migr == 1)

for(var in log_vars2){
    df3 <- df3 %>% 
      mutate(!!paste0("l_", var) := log(get(var)))
}

df3 <- df3 %>%
  mutate(
    Q_1 = l_irQ53sel_dum, 
    AQ_2 = l_irAQ53_blee_skti, 
    AQ_3 = l_irAQ53rl_blee_skti, 
    AQ_4 = l_irAQ53rh_blee_skti, 
    Q_5 = l_irQ53sel_dum, 
    AQ_6 = l_irAQ53_dum_skti_hrs_secall, 
    AQ_7 = l_irAQ53rl_dum_skti_hrs_secall, 
    AQ_8 = l_irAQ53rh_dum_skti_hrs_secall
)

Q_models_4 <- list() 
AQ_models_4 <- list() 
ratios_4 <- list()

Q_models_4$rrQ_1 <- lm(Q_1 ~ l_y, data = df3)
df3 <- df3 %>%
  mutate(b_Q_1 = coef(Q_models_4$rrQ_1)[2]
)

Q_models_4$rrQ_5 <- lm(Q_5 ~ l_y, data = df3 %>% filter(sample_micro == 1))
df3$b_Q_5 <- coef(Q_models_4$rrQ_5)[2]

for(i in c(2:4, 6:8)){
    formula <- as.formula(paste0("AQ_", i, " ~ l_y"))
    AQ_models_4[[paste0("rrAQ_", i)]] <- lm(formula, data = df3)
    df3 <- df3 %>%
      mutate(!!paste0("b_AQ_", i) := coef(AQ_models_4[[paste0("rrAQ_", i)]])[2])
}

df3$s <- (df3$b_Q_1/ df3$b_AQ_2)
ratios_4$Q1_AQ2r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_3)
ratios_4$Q1_AQ3r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_4)
ratios_4$Q1_AQ4r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_6)
ratios_4$Q5_AQ6r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_7)
ratios_4$Q5_AQ7r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_8)
ratios_4$Q5_AQ8r <- mean(df3$s, na.rm = TRUE)

###################
###### Row 5 ######  
###################

df3 <- df3_orig %>% filter(sample == "US Pooled" & sample_migr == 1)

for(var in log_vars3){
    df3 <- df3 %>% 
      mutate(!!paste0("l_", var) := log(get(var)))
}

df3 <- df3 %>%
  mutate(
    Q_1 = l_irQ53yh_dum, 
    AQ_2 = l_irAQ53_blee_skti, 
    AQ_3 = l_irAQ53rl_blee_skti, 
    AQ_4 = l_irAQ53rh_blee_skti, 
    Q_5 = l_irQ53yh_dum, 
    AQ_6 = l_irAQ53_dum_skti_hrs_secall, 
    AQ_7 = l_irAQ53rl_dum_skti_hrs_secall, 
    AQ_8 = l_irAQ53rh_dum_skti_hrs_secall
)

Q_models_5 <- list() 
AQ_models_5 <- list() 
ratios_5 <- list()

Q_models_5$rrQ_1 <- lm(Q_1 ~ l_y, data = df3)
df3 <- df3 %>%
  mutate(b_Q_1 = coef(Q_models_5$rrQ_1)[2]
)

Q_models_5$rrQ_5 <- lm(Q_5 ~ l_y, data = df3 %>% filter(sample_micro == 1))
df3$b_Q_5 <- coef(Q_models_5$rrQ_5)[2]

for(i in c(2:4, 6:8)){
    formula <- as.formula(paste0("AQ_", i, " ~ l_y"))
    AQ_models_5[[paste0("rrAQ_", i)]] <- lm(formula, data = df3)
    df3 <- df3 %>%
      mutate(!!paste0("b_AQ_", i) := coef(AQ_models_5[[paste0("rrAQ_", i)]])[2])
}

df3$s <- (df3$b_Q_1/ df3$b_AQ_2)
ratios_5$Q1_AQ2r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_3)
ratios_5$Q1_AQ3r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_4)
ratios_5$Q1_AQ4r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_6)
ratios_5$Q5_AQ6r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_7)
ratios_5$Q5_AQ7r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_8)
ratios_5$Q5_AQ8r <- mean(df3$s, na.rm = TRUE)

###################
###### Row 6 ######  
###################

df3 <- df3_orig %>% filter(sample == "US Pooled" & sample_migr == 1)

for(var in log_vars4){
    df3 <- df3 %>% 
      mutate(!!paste0("l_", var) := log(get(var)))
}

df3 <- df3 %>%
  mutate(
    Q_1 = l_irQ53goodeng_dum, 
    AQ_2 = l_irAQ53_blee_skti, 
    AQ_3 = l_irAQ53rl_blee_skti, 
    AQ_4 = l_irAQ53rh_blee_skti,
    Q_5 = l_irQ53goodeng_dum, 
    AQ_6 = l_irAQ53_dum_skti_hrs_secall, 
    AQ_7 = l_irAQ53rl_dum_skti_hrs_secall, 
    AQ_8 = l_irAQ53rh_dum_skti_hrs_secall
)

Q_models_6 <- list() 
AQ_models_6 <- list() 
ratios_6 <- list()

Q_models_6$rrQ_1 <- lm(Q_1 ~ l_y, data = df3)
df3 <- df3 %>%
  mutate(b_Q_1 = coef(Q_models_6$rrQ_1)[2]
)

Q_models_6$rrQ_5 <- lm(Q_5 ~ l_y, data = df3 %>% filter(sample_micro == 1))
df3$b_Q_5 <- coef(Q_models_6$rrQ_5)[2]

for(i in c(2:4, 6:8)){
    formula <- as.formula(paste0("AQ_", i, " ~ l_y"))
    AQ_models_6[[paste0("rrAQ_", i)]] <- lm(formula, data = df3)
    df3 <- df3 %>%
      mutate(!!paste0("b_AQ_", i) := coef(AQ_models_6[[paste0("rrAQ_", i)]])[2])
}

df3$s <- (df3$b_Q_1/ df3$b_AQ_2)
ratios_6$Q1_AQ2r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_3)
ratios_6$Q1_AQ3r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_4)
ratios_6$Q1_AQ4r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_6)
ratios_6$Q5_AQ6r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_7)
ratios_6$Q5_AQ7r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_8)
ratios_6$Q5_AQ8r <- mean(df3$s, na.rm = TRUE)

###################
###### Row 7 ######  
###################

df3 <- df3_orig %>% filter(sample == "US Pooled" & sample_migr == 1)

for(var in log_vars5){
    df3 <- df3 %>% 
      mutate(!!paste0("l_", var) := log(get(var)))
}

df3 <- df3 %>%
  mutate(
    Q_1 = l_irQ53nodown_dum, 
    AQ_2 = l_irAQ53_blee_skti, 
    AQ_3 = l_irAQ53rl_blee_skti, 
    AQ_4 = l_irAQ53rh_blee_skti,
    Q_5 = l_irQ53nodown_dum, 
    AQ_6 = l_irAQ53_dum_skti_hrs_secall, 
    AQ_7 = l_irAQ53rl_dum_skti_hrs_secall, 
    AQ_8 = l_irAQ53rh_dum_skti_hrs_secall
)

Q_models_7 <- list() 
AQ_models_7 <- list() 
ratios_7 <- list()

Q_models_7$rrQ_1 <- lm(Q_1 ~ l_y, data = df3)
df3 <- df3 %>%
  mutate(b_Q_1 = coef(Q_models_7$rrQ_1)[2]
)

Q_models_7$rrQ_5 <- lm(Q_5 ~ l_y, data = df3 %>% filter(sample_micro == 1))
df3$b_Q_5 <- coef(Q_models_7$rrQ_5)[2]

for(i in c(2:4, 6:8)){
    formula <- as.formula(paste0("AQ_", i, " ~ l_y"))
    AQ_models_7[[paste0("rrAQ_", i)]] <- lm(formula, data = df3)
    df3 <- df3 %>%
      mutate(!!paste0("b_AQ_", i) := coef(AQ_models_7[[paste0("rrAQ_", i)]])[2])
}

df3$s <- (df3$b_Q_1/ df3$b_AQ_2)
ratios_7$Q1_AQ2r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_3)
ratios_7$Q1_AQ3r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_4)
ratios_7$Q1_AQ4r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_6)
ratios_7$Q5_AQ6r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_7)
ratios_7$Q5_AQ7r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_8)
ratios_7$Q5_AQ8r <- mean(df3$s, na.rm = TRUE)

###################
###### Row 8 ######  
###################

df3 <- df3_orig %>% filter(sample == "US Pooled" & sample_migr == 1)

for(var in log_vars6){
    df3 <- df3 %>% 
      mutate(!!paste0("l_", var) := log(get(var)))
}

df3 <- df3 %>%
  mutate(
    Q_1 = l_irQ53sorts_dum, 
    AQ_2 = l_irAQ53_blee_skti, 
    AQ_3 = l_irAQ53rl_blee_skti, 
    AQ_4 = l_irAQ53rh_blee_skti, 
    Q_5 = l_irQ53sorts_dum, 
    AQ_6 = l_irAQ53_dum_skti_hrs_secall, 
    AQ_7 = l_irAQ53rl_dum_skti_hrs_secall, 
    AQ_8 = l_irAQ53rh_dum_skti_hrs_secall, 
)

Q_models_8 <- list() 
AQ_models_8 <- list() 
ratios_8 <- list()

Q_models_8$rrQ_1 <- lm(Q_1 ~ l_y, data = df3)
df3 <- df3 %>%
  mutate(b_Q_1 = coef(Q_models_8$rrQ_1)[2]
)

Q_models_8$rrQ_5 <- lm(Q_5 ~ l_y, data = df3 %>% filter(sample_micro == 1))
df3$b_Q_5 <- coef(Q_models_8$rrQ_5)[2]

for(i in c(2:4, 6:8)){
    formula <- as.formula(paste0("AQ_", i, " ~ l_y"))
    AQ_models_8[[paste0("rrAQ_", i)]] <- lm(formula, data = df3)
    df3 <- df3 %>%
      mutate(!!paste0("b_AQ_", i) := coef(AQ_models_8[[paste0("rrAQ_", i)]])[2])
}

df3$s <- (df3$b_Q_1/ df3$b_AQ_2)
ratios_8$Q1_AQ2r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_3)
ratios_8$Q1_AQ3r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_4)
ratios_8$Q1_AQ4r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_6)
ratios_8$Q5_AQ6r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_7)
ratios_8$Q5_AQ7r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_8)
ratios_8$Q5_AQ8r <- mean(df3$s, na.rm = TRUE)

###################
###### Row 9 ###### 
###################

df3 <- df3_orig %>% filter(sample == "US Pooled" & sample_migr == 1)

for(var in log_vars6){
    df3 <- df3 %>% 
      mutate(!!paste0("l_", var) := log(get(var)))
}

df3 <- df3 %>%
  mutate(
    Q_1 = l_irQ53sortr_dum, 
    AQ_2 = l_irAQ53_blee_skti, 
    AQ_3 = l_irAQ53rl_blee_skti, 
    AQ_4 = l_irAQ53rh_blee_skti, 
    Q_5 = l_irQ53sortr_dum, 
    AQ_6 = l_irAQ53_dum_skti_hrs_secall, 
    AQ_7 = l_irAQ53rl_dum_skti_hrs_secall, 
    AQ_8 = l_irAQ53rh_dum_skti_hrs_secall
)

Q_models_9 <- list() 
AQ_models_9  <- list() 
ratios_9 <- list()

Q_models_9 $rrQ_1 <- lm(Q_1 ~ l_y, data = df3)
df3 <- df3 %>%
  mutate(b_Q_1 = coef(Q_models_9 $rrQ_1)[2]
)

Q_models_9 $rrQ_5 <- lm(Q_5 ~ l_y, data = df3 %>% filter(sample_micro == 1))
df3$b_Q_5 <- coef(Q_models_9 $rrQ_5)[2]

for(i in c(2:4, 6:8)){
    formula <- as.formula(paste0("AQ_", i, " ~ l_y"))
    AQ_models_9 [[paste0("rrAQ_", i)]] <- lm(formula, data = df3)
    df3 <- df3 %>%
      mutate(!!paste0("b_AQ_", i) := coef(AQ_models_9 [[paste0("rrAQ_", i)]])[2])
}

df3$s <- (df3$b_Q_1/ df3$b_AQ_2)
ratios_9 $Q1_AQ2r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_3)
ratios_9 $Q1_AQ3r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_1/ df3$b_AQ_4)
ratios_9 $Q1_AQ4r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_6)
ratios_9 $Q5_AQ6r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_7)
ratios_9 $Q5_AQ7r <- mean(df3$s, na.rm = TRUE)
df3$s <- (df3$b_Q_5/ df3$b_AQ_8)
ratios_9 $Q5_AQ8r <- mean(df3$s, na.rm = TRUE)

##############################
###### Building Table 3 ######  
##############################

# I will build a df ressembling the table structure I need, then use Kable. 

df_table <- data.frame(matrix(1:99, nrow = 9, ncol = 11))

df_table$X1 <- c("1. US Immigrants", "2. All Host Countries", "3. Bilateral Control",
                  "4. Selection Adjusted", "5. 10+ Years in US", "6. English Speakers",
                  "7. Skill Downgrading", "8. Sorting (sectors)", "9. Sorting (geographic)"
)

for (i in 1:9) {
  df_table[i, 2] <- coef(get(paste0("Q_models_", i))$rrQ_1)["l_y"]
  df_table[i, 7] <- coef(get(paste0("Q_models_", i))$rrQ_5)["l_y"]
  df_table[i, 3] <- summary(get(paste0("Q_models_", i))$rrQ_1)$coefficients["l_y", "Std. Error"]
  df_table[i, 8] <- summary(get(paste0("Q_models_", i))$rrQ_5)$coefficients["l_y", "Std. Error"]
}

for (i in 1:9) {
  df_table[i,4]  <- get(paste0("ratios_", i))["Q1_AQ2r"]
  df_table[i,5]  <- get(paste0("ratios_", i))["Q1_AQ3r"]
  df_table[i,6]  <- get(paste0("ratios_", i))["Q1_AQ4r"]
  df_table[i,9]  <- get(paste0("ratios_", i))["Q5_AQ6r"]
  df_table[i,10]  <- get(paste0("ratios_", i))["Q5_AQ7r"]
  df_table[i,11] <- get(paste0("ratios_", i))["Q5_AQ8r"]
}

df_table <- df_table %>%
  mutate(across(where(is.numeric), round, digits = 3)) %>%
  mutate(
    X2 = paste0(X2, " \n[", X3, "]"),
    X7 = paste0(X7, " \n[", X8, "]")
  ) %>%
  select(-X3, -X8
)

latex_table <- df_table %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, align = c("l", rep("c", ncol(df_table) - 1)),
      col.names = c("", "$\\theta_Q$", "$\\sigma = 1.5$", "$\\sigma = 1.3$", "$\\sigma = 2$",
                    "$\\theta_Q$", "$\\sigma = 1.5$", "$\\sigma = 1.3$", "$\\sigma = 2$"),
      caption = "Relative Human Capital Across Countries") %>%
  pack_rows("Robustness (US Immigrants)", 5,9, indent = FALSE, escape = FALSE, italic = TRUE, bold = FALSE, latex_gap_space = "5pt") %>%
  add_header_above(c(" " = 2, "$\\\\theta_{Q} / \\\\theta_{AQ}$" = 3, " " = 1, "$\\\\theta_{Q} / \\\\theta_{AQ}$" = 3), bold = FALSE, italic = TRUE, escape = FALSE) %>%
  add_header_above(c(" " = 1, "Broad sample (observations = 102)" = 4, 
                     "Microdata sample (observations = 12)" = 4)) %>%
  column_spec(1, width = "4cm") %>%  
  column_spec(2:9, width = "1.25cm") %>%
add_footnote(c("\\scriptsize Notes: This is my footnote"), notation = getOption("kable_footnote_notation", "none"), escape = FALSE, threeparttable = TRUE
)

write(latex_table, file = "Output/Tables/Table_3.tex")

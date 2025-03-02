library(haven)
library(dyplr)
library(lm)

df3_orig <- read_dta("Input/temp/Q.dta")

#########################
###### Definitions ######  
#########################

log_vars <- c("irAQ53_blee_skti", "irAQ53rh_blee_skti", "irAQ53rl_blee_skti", 
          "irAQ53_dum_skti_hrs_secall", "irAQ53rh_dum_skti_hrs_secall", 
          "irAQ53rl_dum_skti_hrs_secall"
)

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

for(var in log_vars){
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

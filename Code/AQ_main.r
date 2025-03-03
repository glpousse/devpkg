library(haven)
library(tidyverse)
library(patchwork)

#########################
###### Definitions ######
#########################

countries <- c("brazil", "canada", "india", "indonesia", "israel",
               "jamaica", "mexico", "panama", "tt", "uruguay", "venezuela", "usa"
)

for(country in countries){
  assign(paste0("df_", country), read_dta(paste0("Input/temp/emphrs_", country,".dta")))
}

pwt_dta <- read_dta("Input/data/pwt/gdp_pwt.dta")

###### Appending ###### 

df_append <- data.frame()

for(country in countries){
  temp_data <- get(paste0("df_", country))
  df_append <- bind_rows(df_append, temp_data)  # Append data
}

df_append <- df_append %>% arrange(country)

###### Merging ###### 

df_merge <- df_append %>%
  inner_join(pwt_dta, by = c("country", "year")) %>%
  arrange(country
  )

###### Standardizing Time Units ###### 

df_merge <- df_merge %>%
  mutate(across(starts_with("hours_skti_"), ~ ifelse(country == "United States", . / 52, .))
  )


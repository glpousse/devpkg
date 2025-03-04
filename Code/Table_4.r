rm(list = ls())
library(haven)
library(dplyr)
library(stringr)

###### Setting up the df ######

df_origin <- read_dta("Input/temp/devacc.dta")

df <- df_origin %>% 
    filter(country == "India") %>%
    select(country, matches("^y_PusmR.*$"), matches("^y_PusmR.*_cc$"), matches("^y_PusmR.*_j$")
)

t_values <- c("j", "cc")
s_values <- c("s2", "s4", "sinf")

i_values <- 
j_values <- c("s15", "s2", "s4", "sinf")

# I'm modifying the colnames while keeping them in place 
colnames(df) <- sapply(colnames(df), function(var) {
  for (t in t_values) {

    for (s in s_values) {
      if (grepl(paste0("_", s, "_", t, "$"), var)) {
        var <- str_replace(var, paste0("_", s, "_", t, "$"), paste0("_", t, "_", s))
      }
    }
    
    if (grepl(paste0("^y_PusmR_", t, "$"), var)) {
      var <- paste0(var, "_s15")  # Appending "_s15" to y_PusmR_t (we're only in the t-loop here)
    }
  }
  return(var)  # I'm making sure the final column name is returned
}, USE.NAMES = FALSE)  # To prevent unwanted named vectors in colnames

for(varname in c("y_PusmR","y_PusmR_v2")){
    colnames(df)[colnames(df) == varname] <- paste0(varname, "_s15")
}

print(colnames(df)) # Make a text to make sure the colnames match with the STATA script?

###### Making the long df ######

df_long <- df %>%

  pivot_longer(
    cols = starts_with("y_PusmR_"),  # Select columns to reshape
    names_to = "type",               # New column for suffixes
    names_prefix = "y_PusmR_",       # Remove prefix "y_PusmR_"
    values_to = "y_PusmR") %>%       # New column for values
  
  mutate(
    method = ifelse(grepl("^[^_]+_[^_]+$", type), sub("_.*", "", type), "."),
    sigma = ifelse(grepl("^[^_]+_[^_]+$", type), sub(".*_", "", type), type),
    method = ifelse(method == ".", "m", method),
    sigma = ifelse(sigma == ".", "type", sigma)) %>%

  select(-type
)

###### Back to Wide ######

df_wide <- df_long %>%
  pivot_wider(
    names_from = sigma,   
    values_from = y_PusmR) %>%

  mutate(
    ind = case_when(
      method == "j"  ~ 1,
      method == "cc" ~ 2,
      method == "m"  ~ 3,
      method == "v2" ~ 4,
      TRUE ~ NA_real_)) %>%

  arrange(ind) %>% 

  select(-c("ind", "country")
)

# df_wide is holds th data for table 4!


rm(list = ls())
library(haven)
library(tidyr)
library(stringr)
library(kableExtra)

###############################
###### Setup for Table 4 ###### 
###############################

df_origin <- read_dta("2_Input/temp/devacc.dta")

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

# df_wide holds the data for table 4 

# Rounding 

df_wide[, -1] <- round(df_wide[, -1], 3)

# Renaming ex-ante for Latex 

df_wide["method"] <- c("$1.\\ \\theta_Q = \\theta_{AQ}$", 
                     "$2.\\ \\theta_Q = 0$",
                     "$3.\\ \\theta_Q = 0.055$ ",
                     "$4.\\ \\theta_Q = 0.05 \\times \\theta_{AQ}$ "
)

#####################
###### Table 4 ######
#####################

latex_table <- df_wide %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, align = c("l", rep("c", ncol(df_wide) - 1)), 
      col.names = c("", "$\\sigma = 1.5$", "$\\sigma = 2$", "$\\sigma = 4$", "$\\sigma = \\infty$"),
      caption = "Relative Human Capital and Development Accounting: US vs. India") %>%
  add_header_above(c(" " = 1, "Counterfactual relative GDP (US = 1)" = 4)) %>%
  row_spec(0, bold = TRUE) %>% 
  column_spec(1, width = "6cm") %>%
  column_spec(2:5, width = "1.2cm") %>%  
  pack_rows("Relative Human Capital Interpretation", 1, 1, escape = FALSE, italic = TRUE, bold = FALSE, latex_gap_space = "5pt") %>%
  pack_rows("Relative Technology Interpretation", 2, 2, escape = FALSE, italic = TRUE, bold = FALSE, latex_gap_space = "5pt") %>%
  pack_rows("Migrant-Based Calibration", 3, 4, escape = FALSE, italic = TRUE, bold = FALSE, latex_gap_space = "5pt") %>%
  kable_styling(font_size = 9, position = "center") %>%
  add_footnote(c("\\scriptsize Notes: This table replicates Table 4 from Rossi (2022). It shows the counterfactual GDP ratio $y^*_P/y_R$, where $P$ is India and $R$ is the United States, under different calibrations of the elasticity of relative human capital $\\theta_Q$. For comparison, the actual GDP ratio in the data is $y_P/y_R = 0.057$. The code used to replicate this table can be found \\underline{\\href{https://github.com/glpousse/devpkg/blob/main/Code/Tables/Table_4.r}{here}}."), notation = getOption("kable_footnote_notation", "none"), escape = FALSE, threeparttable = TRUE
)

write(latex_table, file = "3_Output/Tables/Table_4.tex")

##### Table 4 from Rossi, 2022 (AER) completed #####
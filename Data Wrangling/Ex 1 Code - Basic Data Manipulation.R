# Call relevant packages and setwd()
library(dplyr)
library(tidyr)
setwd("C:/Users/morgane/Documents/Data_Science_Foundations/Data Wrangling")

# Read companies (df) data into R
df <- tbl_df(read.csv("refine_original.csv"))

# Wrangle companies column into uniform responses
patterns <- c("^[pfPF].*", "^[aA].*", "^[vV].*","^[uU].*")
replacements <- c("Phillips", "Akzo", "Van Houten", "Unilever")

for (pattern in patterns) {
  df$company <- sub(pattern, replacements[which(patterns == pattern)], df$company)
}
  
# Separate product code and number into two columns, and add product category
df <- df %>% 
  separate(Product.code...number, c("product_code", "product_number"), "-") %>% 
  mutate(product_category = case_when(product_code == "p" ~ "Smartphone",
                                      product_code == "v" ~ "TV", 
                                      product_code == "x" ~ "Laptop", 
                                      product_code == "q" ~ "Tablet"))

# Concatenate address, city and country into full_address
df <- df %>% unite(full_address, address, city, country, sep = ",")

# Add binary columns to indicate company and category
df <- df %>% mutate(company_phillips = ifelse(company == "Phillips", 1, 0)) %>% 
  mutate(company_akzo = ifelse(company == "Akzo", 1, 0)) %>% 
  mutate(company_van_houten = ifelse(company == "Van Houten", 1, 0)) %>% 
  mutate(company_unilever = ifelse(company == "Unilever", 1, 0)) %>% 
  mutate(product_smartphone = ifelse(product_code == "p", 1, 0)) %>% 
  mutate(product_tv = ifelse(product_code == "v", 1, 0)) %>% 
  mutate(product_laptop = ifelse(product_code == "x", 1, 0)) %>% 
  mutate(product_tablet = ifelse(product_code == "q", 1, 0))

write.csv(df, "refine_clean.csv")
  
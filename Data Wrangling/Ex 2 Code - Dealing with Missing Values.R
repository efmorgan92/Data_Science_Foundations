library(plyr)
library(dplyr)
library(stringr)

# Load data into the data frame
df <- tbl_df(read.csv("titanic_original.csv"))

# Remove missing values from "embarked"
df$embarked[df$embarked == ""] <- "S"

# Populate missing age values with mean age
df$age[is.na(df$age)] <- mean(df$age, na.rm = TRUE)

# Replace empty boat values with dummy variables
levels(df$boat)[levels(df$boat)==""] <- "None"

# Missing cabin numbers may indicate that records weren't kept for these passengers, 
# suggesting that they traveled in a lower class. It does not make sense to add a 
# cabin number for these patients, because we would be guessing. 

# Make a has_cabin_number indicator column
df$has_cabin_number <- case_when(df$cabin == "" ~ 0, 
                                 df$cabin != "" ~ 1)

write.csv(df, "titanic_clean.csv")
# ============================================================
# 01_data_cleaning.R
# Melbourne Housing Price Prediction
# Data Cleaning
# ============================================================

library(readr)
library(dplyr)

# ------------------------------------------------------------
# 1. Create Project Folders
# ------------------------------------------------------------

dir.create("data", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)


# ------------------------------------------------------------
# 2. Load Raw Dataset
# ------------------------------------------------------------

df <- read_csv(
  "Melbourne_housing_FULL.csv",
  show_col_types = FALSE
)

cat("Original dataset dimensions:\n")
print(dim(df))


# ------------------------------------------------------------
# 3. Basic Inspection
# ------------------------------------------------------------

cat("\n================ STRUCTURE ================\n")
str(df)

cat("\n================ SUMMARY ================\n")
print(summary(df))


# ------------------------------------------------------------
# 4. Missing Values
# ------------------------------------------------------------

missing_summary <- data.frame(
  Variable = names(df),
  
  Missing_N = sapply(
    df,
    function(x) sum(is.na(x))
  ),
  
  Missing_Pct = sapply(
    df,
    function(x) mean(is.na(x)) * 100
  )
) %>%
  arrange(desc(Missing_Pct))


cat("\n================ MISSING VALUES ================\n")
print(missing_summary)


# ------------------------------------------------------------
# 5. Remove Exact Duplicate Rows
# ------------------------------------------------------------

before_duplicates <- nrow(df)

df <- df %>%
  distinct()

after_duplicates <- nrow(df)

cat(
  "\nDuplicates removed:",
  before_duplicates - after_duplicates,
  "\n"
)


# ------------------------------------------------------------
# 6. Convert Date
# ------------------------------------------------------------

df$Date <- as.Date(
  df$Date,
  format = "%d/%m/%Y"
)

cat("\nDate range:\n")
print(range(df$Date, na.rm = TRUE))


# ------------------------------------------------------------
# 7. Remove Rows with Missing or Invalid Target
# ------------------------------------------------------------

# Price is our target variable.
# We do NOT impute Price.

before_price_filter <- nrow(df)

df <- df %>%
  filter(
    !is.na(Price),
    Price > 0
  )

cat(
  "\nRows removed because Price was missing/invalid:",
  before_price_filter - nrow(df),
  "\n"
)


# ------------------------------------------------------------
# 8. Check for Impossible Values
# ------------------------------------------------------------

# We only CHECK these values.
# We do not automatically delete rows.

cat("\n================ IMPOSSIBLE VALUE CHECK ================\n")

cat(
  "Rooms <= 0:",
  sum(df$Rooms <= 0, na.rm = TRUE),
  "\n"
)

cat(
  "Bathroom < 0:",
  sum(df$Bathroom < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Car < 0:",
  sum(df$Car < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Distance < 0:",
  sum(df$Distance < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Landsize < 0:",
  sum(df$Landsize < 0, na.rm = TRUE),
  "\n"
)

cat(
  "BuildingArea < 0:",
  sum(df$BuildingArea < 0, na.rm = TRUE),
  "\n"
)


# ------------------------------------------------------------
# 9. Select Variables for Modeling
# ------------------------------------------------------------

model_df <- df %>%
  select(
    Price,
    Date,
    Rooms,
    Distance,
    Bedroom2,
    Bathroom,
    Car,
    Landsize,
    BuildingArea,
    YearBuilt,
    Lattitude,
    Longtitude,
    Suburb,
    Type,
    Method,
    Regionname
  )


# ------------------------------------------------------------
# 10. Final Inspection
# ------------------------------------------------------------

cat("\n================ FINAL DATASET ================\n")

cat(
  "Rows:",
  nrow(model_df),
  "\n"
)

cat(
  "Columns:",
  ncol(model_df),
  "\n"
)

cat("\nStructure:\n")
str(model_df)

cat("\nSummary:\n")
print(summary(model_df))


# ------------------------------------------------------------
# 11. Final Missing Value Summary
# ------------------------------------------------------------

final_missing <- data.frame(
  Variable = names(model_df),
  
  Missing_N = sapply(
    model_df,
    function(x) sum(is.na(x))
  ),
  
  Missing_Pct = sapply(
    model_df,
    function(x) mean(is.na(x)) * 100
  )
) %>%
  arrange(desc(Missing_Pct))


cat("\n================ FINAL MISSING VALUES ================\n")
print(final_missing)


# ------------------------------------------------------------
# 12. Save Clean Dataset
# ------------------------------------------------------------

write_csv(
  model_df,
  "data/modeling_data.csv"
)

cat(
  "\nClean modeling dataset saved to:",
  "data/modeling_data.csv\n"
)

cat("\n================ CLEANING COMPLETED ================\n")
# ============================================================
# 02_preprocessing.R
# Melbourne Housing Price Prediction
# Feature Engineering + Train/Test Split + Imputation
# ============================================================


# ============================================================
# 1. Load Libraries
# ============================================================

library(readr)
library(dplyr)
library(forcats)


# ============================================================
# 2. Load Clean Dataset
# ============================================================

model_df <- read_csv(
  "data/modeling_data.csv",
  show_col_types = FALSE
)

cat("Dataset dimensions:\n")
print(dim(model_df))


# ============================================================
# 3. Feature Engineering
# ============================================================

model_df <- model_df %>%
  mutate(
    SaleYear = as.numeric(format(Date, "%Y")),
    SaleMonth = as.numeric(format(Date, "%m")),
    PropertyAge = SaleYear - YearBuilt,
    PropertyAge = ifelse(PropertyAge < 0, NA, PropertyAge),
    YearBuiltMissing = ifelse(is.na(YearBuilt), 1, 0)
  ) %>%
  select(
    -Date,
    -YearBuilt
  )

cat("\nFeatures after engineering:\n")
print(names(model_df))


# ============================================================
# 4. Train / Test Split
# ============================================================

set.seed(123)

n <- nrow(model_df)

train_index <- sample(
  seq_len(n),
  size = floor(0.80 * n)
)

train <- model_df[train_index, ]
test  <- model_df[-train_index, ]

cat("\n================ TRAIN / TEST SPLIT ================\n")
cat("Training rows:", nrow(train), "\n")
cat("Testing rows :", nrow(test), "\n")


# ============================================================
# 5. Define Variables
# ============================================================

target <- "Price"

numeric_predictors <- c(
  "Rooms",
  "Distance",
  "Bedroom2",
  "Bathroom",
  "Car",
  "Landsize",
  "BuildingArea",
  "Lattitude",
  "Longtitude",
  "SaleYear",
  "SaleMonth",
  "PropertyAge"
)

categorical_predictors <- c(
  "Suburb",
  "Type",
  "Method",
  "Regionname"
)


# ============================================================
# 6. Define Sanity Bounds
# ============================================================

sanity_bounds <- list(
  Landsize = c(0, Inf),
  BuildingArea = c(0, Inf),
  PropertyAge = c(0, 200),
  Bathroom = c(0, Inf),
  Car = c(0, Inf)
)


# ============================================================
# 7. Numeric Imputation
# ============================================================

# Calculate medians using training data only
train_medians <- sapply(
  train[numeric_predictors],
  median,
  na.rm = TRUE
)

cat("\n================ TRAINING MEDIANS ================\n")
print(train_medians)


# Apply training medians to training data
for (v in numeric_predictors) {
  
  missing_rows <- is.na(train[[v]])
  
  if (any(missing_rows)) {
    train[[v]][missing_rows] <- train_medians[[v]]
  }
}


# Apply training medians to test data
for (v in numeric_predictors) {
  
  missing_rows <- is.na(test[[v]])
  
  if (any(missing_rows)) {
    test[[v]][missing_rows] <- train_medians[[v]]
  }
}

cat("\nNumeric imputation completed for train and test data.\n")


# ============================================================
# 8. Apply Sanity Bounds
# ============================================================

apply_bounds <- function(df, bounds) {
  
  for (v in names(bounds)) {
    
    if (v %in% names(df)) {
      
      lo <- bounds[[v]][1]
      hi <- bounds[[v]][2]
      
      df[[v]] <- pmin(
        pmax(df[[v]], lo),
        hi
      )
    }
  }
  
  return(df)
}

train <- apply_bounds(
  train,
  sanity_bounds
)

test <- apply_bounds(
  test,
  sanity_bounds
)

cat("Sanity bounds applied to numeric predictors.\n")


# ============================================================
# 9. Handle Missing Categorical Values
# ============================================================

for (v in categorical_predictors) {
  
  train[[v]] <- as.character(train[[v]])
  test[[v]] <- as.character(test[[v]])
  
  train[[v]][is.na(train[[v]])] <- "Unknown"
  test[[v]][is.na(test[[v]])] <- "Unknown"
}

cat("\nCategorical missing values handled.\n")


# ============================================================
# 10. Handle Rare Suburbs
# ============================================================

cat("\n================ SUBURB INFORMATION ================\n")

cat(
  "Unique suburbs before grouping:",
  length(unique(train$Suburb)),
  "\n"
)


# Group rare suburbs based on training data only
train$Suburb <- fct_lump_min(
  as.factor(train$Suburb),
  min = 20,
  other_level = "Other"
)

cat(
  "Unique suburbs after grouping:",
  length(levels(train$Suburb)),
  "\n"
)


# Get valid suburb levels from training data
known_suburbs <- levels(train$Suburb)


# Convert test suburbs not seen during training to "Other"
test$Suburb <- as.character(test$Suburb)

test$Suburb[
  !(test$Suburb %in% known_suburbs)
] <- "Other"


# Convert test Suburb to the same factor levels as training
test$Suburb <- factor(
  test$Suburb,
  levels = known_suburbs
)


# ============================================================
# 11. Convert Remaining Categorical Variables to Factors
# ============================================================

for (v in c("Type", "Method", "Regionname")) {
  
  train[[v]] <- factor(train[[v]])
  
  test[[v]] <- factor(
    test[[v]],
    levels = levels(train[[v]])
  )
}


# ============================================================
# 12. Check for Factor-Level Problems
# ============================================================

na_after_factor <- sum(is.na(test$Type)) +
  sum(is.na(test$Method)) +
  sum(is.na(test$Regionname)) +
  sum(is.na(test$Suburb))

if (na_after_factor > 0) {
  
  cat(
    "\nWARNING:",
    na_after_factor,
    "values became NA because of unmatched factor levels.\n"
  )
  
} else {
  
  cat(
    "\nFactor levels successfully aligned between train and test.\n"
  )
}


# ============================================================
# 13. Final Missing Value Check
# ============================================================

cat(
  "\n================ FINAL TRAIN MISSING VALUES ================\n"
)

print(colSums(is.na(train)))


cat(
  "\n================ FINAL TEST MISSING VALUES ================\n"
)

print(colSums(is.na(test)))


# ============================================================
# 14. Final Dataset Dimensions
# ============================================================

cat(
  "\n================ FINAL DATASETS ================\n"
)

cat(
  "Train:",
  nrow(train),
  "rows x",
  ncol(train),
  "columns\n"
)

cat(
  "Test :",
  nrow(test),
  "rows x",
  ncol(test),
  "columns\n"
)


# ============================================================
# 15. Save Processed Data
# ============================================================

write_csv(
  train,
  "data/train_data.csv"
)

write_csv(
  test,
  "data/test_data.csv"
)


# ============================================================
# 16. Save Preprocessing Parameters
# ============================================================

preprocessing_params <- list(
  
  train_medians = train_medians,
  
  sanity_bounds = sanity_bounds,
  
  known_suburbs = known_suburbs,
  
  type_levels = levels(train$Type),
  
  method_levels = levels(train$Method),
  
  region_levels = levels(train$Regionname),
  
  numeric_predictors = numeric_predictors,
  
  categorical_predictors = categorical_predictors
)

saveRDS(
  preprocessing_params,
  "data/preprocessing_params.rds"
)


# ============================================================
# 17. Save RDS Versions
# ============================================================

saveRDS(
  train,
  "data/train_data.rds"
)

saveRDS(
  test,
  "data/test_data.rds"
)


# ============================================================
# 18. Preprocessing Completed
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "PREPROCESSING COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
)
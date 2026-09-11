
library(readr)
library(dplyr)
library(car)

# ------------------------------------------------------------
# 1. Load Training Data
# ------------------------------------------------------------

train <- readRDS(
  "data/train_data.rds"
)

cat("Training dataset dimensions:\n")
print(dim(train))

cat("\n================ VARIABLES ================\n")
print(names(train))

# ------------------------------------------------------------
# 2. Numeric Predictor VIF
# ------------------------------------------------------------

numeric_vif_model <- lm(
  Price ~
    Rooms +
    Distance +
    Bedroom2 +
    Bathroom +
    Car +
    Landsize +
    BuildingArea +
    Lattitude +
    Longtitude +
    SaleYear +
    SaleMonth +
    PropertyAge +
    YearBuiltMissing,
  data = train
)

cat("\n================ NUMERIC VIF ================\n")

vif_numeric <- car::vif(
  numeric_vif_model
)

print(vif_numeric)

# ------------------------------------------------------------
# 3. Convert VIF Results to Data Frame
# ------------------------------------------------------------

vif_numeric_df <- data.frame(
  Variable = names(vif_numeric),
  VIF = as.numeric(vif_numeric)
)

vif_numeric_df <- vif_numeric_df %>%
  arrange(desc(VIF))

cat("\n================ SORTED VIF ================\n")
print(vif_numeric_df)

# ------------------------------------------------------------
# 4. VIF Interpretation
# ------------------------------------------------------------

vif_numeric_df <- vif_numeric_df %>%
  mutate(
    Interpretation = case_when(
      VIF < 5 ~ "Acceptable",
      VIF >= 5 & VIF < 10 ~ "Potential concern",
      VIF >= 10 ~ "High multicollinearity"
    )
  )

cat("\n================ VIF INTERPRETATION ================\n")
print(vif_numeric_df)

# ------------------------------------------------------------
# 5. Save Results
# ------------------------------------------------------------

dir.create(
  "results",
  showWarnings = FALSE
)

write_csv(
  vif_numeric_df,
  "results/vif_numeric.csv"
)

# ============================================================
# 6. Full Model VIF / GVIF
# ============================================================


full_vif_model <- lm(
  Price ~
    Rooms +
    Distance +
    Bedroom2 +
    Bathroom +
    Car +
    Landsize +
    BuildingArea +
    Lattitude +
    Longtitude +
    SaleYear +
    SaleMonth +
    PropertyAge +
    YearBuiltMissing +
    Suburb +
    Type +
    Method +
    Regionname,
  data = train
)

cat("\n================ FULL MODEL VIF / GVIF ================\n")

vif_full <- car::vif(
  full_vif_model
)

print(vif_full)

# ------------------------------------------------------------
# 7. Save Full VIF / GVIF
# ------------------------------------------------------------

if (is.matrix(vif_full)) {
  
  vif_full_df <- as.data.frame(vif_full)
  
  vif_full_df$Variable <- rownames(vif_full_df)
  
  rownames(vif_full_df) <- NULL
  
  vif_full_df <- vif_full_df %>%
    select(
      Variable,
      everything()
    )
  
} else {
  
  vif_full_df <- data.frame(
    Variable = names(vif_full),
    VIF = as.numeric(vif_full)
  )
}

write_csv(
  vif_full_df,
  "results/vif_full.csv"
)

# ============================================================
# 8. Compare Rooms and Bedroom2
# ============================================================


if (
  "Rooms" %in% names(vif_numeric) &&
  "Bedroom2" %in% names(vif_numeric)
) {
  
  cat(
    "\nRooms VIF:",
    round(vif_numeric["Rooms"], 3),
    "\n"
  )
  
  cat(
    "Bedroom2 VIF:",
    round(vif_numeric["Bedroom2"], 3),
    "\n"
  )
}



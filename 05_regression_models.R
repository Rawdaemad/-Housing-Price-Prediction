
library(readr)
library(dplyr)

train <- readRDS(
  "data/train_data.rds"
)

test <- readRDS(
  "data/test_data.rds"
)

cat("Train dimensions:\n")
print(dim(train))

cat("\nTest dimensions:\n")
print(dim(test))


cat("\n================ MISSING VALUES ================\n")

cat(
  "Train missing values:",
  sum(is.na(train)),
  "\n"
)

cat(
  "Test missing values:",
  sum(is.na(test)),
  "\n"
)


model_1 <- lm(
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

cat("\n================ MODEL 1: BASELINE ================\n")

print(summary(model_1))

# ============================================================
# 4. MODEL 2 — REMOVE BEDROOM2


model_2 <- lm(
  Price ~
    Rooms +
    Distance +
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

cat("\n================ MODEL 2: WITHOUT BEDROOM2 ================\n")

print(summary(model_2))

# ============================================================
# 5. MODEL 3 — WITHOUT LOCATION CATEGORIES
# ============================================================

model_3 <- lm(
  Price ~
    Rooms +
    Distance +
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
    Type +
    Method,
  data = train
)

cat("\n================ MODEL 3: WITHOUT SUBURB/REGION ================\n")

print(summary(model_3))

# ============================================================
# 6. MODEL 4 — LOG PRICE
# ============================================================

model_4 <- lm(
  log(Price) ~
    Rooms +
    Distance +
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

cat("\n================ MODEL 4: LOG PRICE ================\n")

print(summary(model_4))

# ============================================================
# 7. MODEL 5 — LOG PRICE WITHOUT SUBURB
# ============================================================

model_5 <- lm(
  log(Price) ~
    Rooms +
    Distance +
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
    Type +
    Method +
    Regionname,
  data = train
)

cat("\n================ MODEL 5: LOG PRICE WITHOUT SUBURB ================\n")

print(summary(model_5))

# ============================================================
# 8. PREDICTIONS ON TEST SET
# ============================================================

pred_1 <- predict(
  model_1,
  newdata = test
)

pred_2 <- predict(
  model_2,
  newdata = test
)

pred_3 <- predict(
  model_3,
  newdata = test
)

# Models 4 and 5 predict log(Price)
# so we convert predictions back to the original Price scale.

pred_4_log <- predict(
  model_4,
  newdata = test
)

pred_5_log <- predict(
  model_5,
  newdata = test
)

pred_4 <- exp(pred_4_log)
pred_5 <- exp(pred_5_log)

# ============================================================
# 9. Prevent Negative Price Predictions
# ============================================================

pred_1 <- pmax(pred_1, 0)
pred_2 <- pmax(pred_2, 0)
pred_3 <- pmax(pred_3, 0)
pred_4 <- pmax(pred_4, 0)
pred_5 <- pmax(pred_5, 0)

# ============================================================
# 10. Evaluation Function
# ============================================================

calculate_metrics <- function(actual, predicted) {
  
  rmse <- sqrt(
    mean(
      (actual - predicted)^2,
      na.rm = TRUE
    )
  )
  
  mae <- mean(
    abs(actual - predicted),
    na.rm = TRUE
  )
  
  r2 <- 1 -
    sum(
      (actual - predicted)^2,
      na.rm = TRUE
    ) /
    sum(
      (actual - mean(actual, na.rm = TRUE))^2,
      na.rm = TRUE
    )
  
  data.frame(
    R2 = r2,
    RMSE = rmse,
    MAE = mae
  )
}

# ============================================================
# 11. Evaluate All Models
# ============================================================

actual <- test$Price

metrics_1 <- calculate_metrics(
  actual,
  pred_1
)

metrics_2 <- calculate_metrics(
  actual,
  pred_2
)

metrics_3 <- calculate_metrics(
  actual,
  pred_3
)

metrics_4 <- calculate_metrics(
  actual,
  pred_4
)

metrics_5 <- calculate_metrics(
  actual,
  pred_5
)

# ------------------------------------------------------------
# Add Model Names
# ------------------------------------------------------------

metrics_1$Model <- "Model 1 - Baseline"
metrics_2$Model <- "Model 2 - Without Bedroom2"
metrics_3$Model <- "Model 3 - Without Suburb/Region"
metrics_4$Model <- "Model 4 - Log Price"
metrics_5$Model <- "Model 5 - Log Price Without Suburb"

# ------------------------------------------------------------
# Combine Results
# ------------------------------------------------------------

model_comparison <- bind_rows(
  metrics_1,
  metrics_2,
  metrics_3,
  metrics_4,
  metrics_5
) %>%
  select(
    Model,
    R2,
    RMSE,
    MAE
  )

cat("\n====================================================\n")
cat("TEST SET MODEL COMPARISON\n")
cat("====================================================\n")

print(model_comparison)

# ============================================================
# 12. Sort Models by Performance
# ============================================================

model_comparison_sorted <- model_comparison %>%
  arrange(
    desc(R2)
  )

cat("\n================ SORTED BY R2 ================\n")

print(model_comparison_sorted)

# ============================================================
# 13. Save Results
# ============================================================

dir.create(
  "results",
  showWarnings = FALSE
)

write_csv(
  model_comparison,
  "results/model_comparison.csv"
)

write_csv(
  model_comparison_sorted,
  "results/model_comparison_sorted.csv"
)

# ============================================================
# 14. Training Performance
# ============================================================
# We also record adjusted R-squared from the training models.
# This is NOT used as the final performance metric.

training_summary <- data.frame(
  Model = c(
    "Model 1 - Baseline",
    "Model 2 - Without Bedroom2",
    "Model 3 - Without Suburb/Region",
    "Model 4 - Log Price",
    "Model 5 - Log Price Without Suburb"
  ),
  R2_Train = c(
    summary(model_1)$r.squared,
    summary(model_2)$r.squared,
    summary(model_3)$r.squared,
    summary(model_4)$r.squared,
    summary(model_5)$r.squared
  ),
  Adjusted_R2_Train = c(
    summary(model_1)$adj.r.squared,
    summary(model_2)$adj.r.squared,
    summary(model_3)$adj.r.squared,
    summary(model_4)$adj.r.squared,
    summary(model_5)$adj.r.squared
  )
)

cat("\n================ TRAINING PERFORMANCE ================\n")

print(training_summary)

write_csv(
  training_summary,
  "results/training_model_summary.csv"
)

# ============================================================
# 15. Save Models
# ============================================================

saveRDS(
  model_1,
  "results/model_1_baseline.rds"
)

saveRDS(
  model_2,
  "results/model_2_without_bedroom2.rds"
)

saveRDS(
  model_3,
  "results/model_3_without_location_categories.rds"
)

saveRDS(
  model_4,
  "results/model_4_log_price.rds"
)

saveRDS(
  model_5,
  "results/model_5_log_price_without_suburb.rds"
)


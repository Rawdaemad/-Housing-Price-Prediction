
library(readr)
library(dplyr)
library(lmtest)

train <- readRDS(
  "data/train_data.rds"
)

test <- readRDS(
  "data/test_data.rds"
)

cat("\n============================================\n")
cat("WLS MODELING\n")
cat("============================================\n")


model_2 <- readRDS(
  "results/model_2_without_bedroom2.rds"
)

cat("\nBest OLS model loaded successfully.\n")



train$fitted_ols <- fitted(model_2)
train$residual_ols <- residuals(model_2)

cat("\nOLS fitted values and residuals calculated.\n")


# ============================================================
# 4. Model the Error Variance
# ============================================================

train$log_residual_sq <- log(
  train$residual_ols^2 + 1
)


variance_model <- lm(
  log_residual_sq ~ fitted_ols,
  data = train
)

cat("\nVariance model fitted successfully.\n")


# ============================================================
# 5. Estimate Variance for Each Training Observation
# ============================================================

train$predicted_log_variance <- predict(
  variance_model,
  newdata = train
)

train$estimated_variance <- exp(
  train$predicted_log_variance
)

# Prevent zero or extremely small variance estimates
train$estimated_variance <- pmax(
  train$estimated_variance,
  1e-8
)


# ============================================================
# 6. Create WLS Weights
# ============================================================

train$wls_weight <- 1 / train$estimated_variance


# ============================================================
# 7. Protect Against Extreme Weights
# ============================================================

lower_weight <- quantile(
  train$wls_weight,
  0.01,
  na.rm = TRUE
)

upper_weight <- quantile(
  train$wls_weight,
  0.99,
  na.rm = TRUE
)

train$wls_weight <- pmin(
  pmax(
    train$wls_weight,
    lower_weight
  ),
  upper_weight
)

cat("\nWLS weights created and capped at 1st/99th percentiles.\n")

cat("\nWeight range:\n")
print(
  range(
    train$wls_weight,
    na.rm = TRUE
  )
)


# ============================================================
# 8. Fit WLS Model
# ============================================================

wls_model <- lm(
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
  data = train,
  weights = wls_weight
)

cat("\nWLS model fitted successfully.\n")


# ============================================================
# 9. WLS Model Summary
# ============================================================

cat("\n============================================\n")
cat("WLS MODEL SUMMARY\n")
cat("============================================\n")

print(
  summary(wls_model)
)


# ============================================================
# 10. Test Predictions
# ============================================================

test_pred_wls <- predict(
  wls_model,
  newdata = test
)

# Price cannot be negative
test_pred_wls <- pmax(
  test_pred_wls,
  0
)

actual <- test$Price


# ============================================================
# 11. Evaluation Function
# ============================================================

calculate_metrics <- function(
    actual,
    predicted
) {
  
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
# 12. Evaluate WLS
# ============================================================

wls_metrics <- calculate_metrics(
  actual,
  test_pred_wls
)

wls_metrics$Model <- "WLS"


# ============================================================
# 13. Evaluate Original OLS Model
# ============================================================

ols_pred <- predict(
  model_2,
  newdata = test
)

ols_pred <- pmax(
  ols_pred,
  0
)

ols_metrics <- calculate_metrics(
  actual,
  ols_pred
)

ols_metrics$Model <- "Model 2 - OLS"


# ============================================================
# 14. OLS vs WLS Comparison
# ============================================================

comparison <- bind_rows(
  ols_metrics,
  wls_metrics
) %>%
  select(
    Model,
    R2,
    RMSE,
    MAE
  )

comparison <- comparison %>%
  mutate(
    R2 = round(R2, 6),
    RMSE = round(RMSE, 2),
    MAE = round(MAE, 2)
  )


cat("\n============================================\n")
cat("OLS vs WLS TEST PERFORMANCE\n")
cat("============================================\n")

print(comparison)


# ============================================================
# 15. Calculate Improvement
# ============================================================

ols_r2 <- ols_metrics$R2
wls_r2 <- wls_metrics$R2

ols_rmse <- ols_metrics$RMSE
wls_rmse <- wls_metrics$RMSE

ols_mae <- ols_metrics$MAE
wls_mae <- wls_metrics$MAE


cat("\n============================================\n")
cat("WLS IMPROVEMENT\n")
cat("============================================\n")

cat(
  "R2 change:",
  round(wls_r2 - ols_r2, 6),
  "\n"
)

cat(
  "RMSE change:",
  round(wls_rmse - ols_rmse, 2),
  "\n"
)

cat(
  "MAE change:",
  round(wls_mae - ols_mae, 2),
  "\n"
)


# ============================================================
# 16. Breusch-Pagan Test for WLS
# ============================================================
cat("\n============================================\n")
cat("BREUSCH-PAGAN TEST - WLS\n")
cat("============================================\n")

bp_wls <- tryCatch(
  {
    bptest(wls_model)
  },
  error = function(e) {
    cat(
      "\nBreusch-Pagan test could not be completed.\n"
    )
    
    cat(
      "Reason:",
      conditionMessage(e),
      "\n"
    )
    
    NULL
  }
)


if (!is.null(bp_wls)) {
  
  print(bp_wls)
  
  bp_wls_results <- data.frame(
    Model = "WLS",
    Statistic = as.numeric(
      bp_wls$statistic
    ),
    DF = as.numeric(
      bp_wls$parameter
    ),
    P_Value = bp_wls$p.value,
    Interpretation = ifelse(
      bp_wls$p.value < 0.05,
      "Evidence of heteroscedasticity",
      "No strong evidence of heteroscedasticity"
    )
  )
  
  write_csv(
    bp_wls_results,
    "results/breusch_pagan_wls.csv"
  )
}


write_csv(
  comparison,
  "results/ols_vs_wls_comparison.csv"
)

saveRDS(
  variance_model,
  "results/variance_model.rds"
)



saveRDS(
  wls_model,
  "results/wls_model.rds"
)


wls_predictions <- data.frame(
  Actual_Price = actual,
  Predicted_Price = test_pred_wls,
  Error = actual - test_pred_wls
)

write_csv(
  wls_predictions,
  "results/wls_test_predictions.csv"
)



weight_summary <- data.frame(
  Minimum_Weight = min(
    train$wls_weight,
    na.rm = TRUE
  ),
  Median_Weight = median(
    train$wls_weight,
    na.rm = TRUE
  ),
  Mean_Weight = mean(
    train$wls_weight,
    na.rm = TRUE
  ),
  Maximum_Weight = max(
    train$wls_weight,
    na.rm = TRUE
  )
)

write_csv(
  weight_summary,
  "results/wls_weight_summary.csv"
)



library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)

train <- readRDS(
  "data/train_data.rds"
)

cat("Train dimensions:\n")
print(dim(train))


model_2 <- readRDS(
  "results/model_2_without_bedroom2.rds"
)

model_4 <- readRDS(
  "results/model_4_log_price.rds"
)

cat("\nCandidate models loaded successfully.\n")

==========================================================

cat("\nModel 2:\n")
cat("Observations:", nobs(model_2), "\n")
cat("Coefficients:", length(coef(model_2)), "\n")

cat("\nModel 4:\n")
cat("Observations:", nobs(model_4), "\n")
cat("Coefficients:", length(coef(model_4)), "\n")


diagnostic_2 <- data.frame(
  Fitted = fitted(model_2),
  Residuals = residuals(model_2)
)

diagnostic_4 <- data.frame(
  Fitted = fitted(model_4),
  Residuals = residuals(model_4)
)



write_csv(
  diagnostic_2,
  "results/model_2_basic_diagnostics.csv"
)

write_csv(
  diagnostic_4,
  "results/model_4_basic_diagnostics.csv"
)


# ============================================================
# 6. MODEL 2 - Residuals vs Fitted
# ============================================================

p1 <- ggplot(
  diagnostic_2,
  aes(
    x = Fitted,
    y = Residuals
  )
) +
  geom_point(
    alpha = 0.20
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Model 2 - Residuals vs Fitted",
    x = "Fitted Price",
    y = "Residuals"
  ) +
  theme_minimal()

print(p1)

ggsave(
  "figures/14_model2_residuals_vs_fitted.png",
  p1,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 7. MODEL 4 - Residuals vs Fitted
# ============================================================

p2 <- ggplot(
  diagnostic_4,
  aes(
    x = Fitted,
    y = Residuals
  )
) +
  geom_point(
    alpha = 0.20
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Model 4 - Residuals vs Fitted",
    x = "Fitted log(Price)",
    y = "Residuals on log scale"
  ) +
  theme_minimal()

print(p2)

ggsave(
  "figures/15_model4_residuals_vs_fitted.png",
  p2,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 8. MODEL 2 - Q-Q Plot
# ============================================================

p3 <- ggplot(
  diagnostic_2,
  aes(
    sample = Residuals
  )
) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Model 2 - Normal Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

print(p3)

ggsave(
  "figures/16_model2_qq_plot.png",
  p3,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 9. MODEL 4 - Q-Q Plot
# ============================================================

p4 <- ggplot(
  diagnostic_4,
  aes(
    sample = Residuals
  )
) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Model 4 - Normal Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

print(p4)

ggsave(
  "figures/17_model4_qq_plot.png",
  p4,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 10. MODEL 2 - Residual Histogram
# ============================================================

p5 <- ggplot(
  diagnostic_2,
  aes(
    x = Residuals
  )
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Model 2 - Residual Distribution",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal()

print(p5)

ggsave(
  "figures/18_model2_residual_histogram.png",
  p5,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 11. MODEL 4 - Residual Histogram
# ============================================================

p6 <- ggplot(
  diagnostic_4,
  aes(
    x = Residuals
  )
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Model 4 - Residual Distribution",
    x = "Residuals on log scale",
    y = "Frequency"
  ) +
  theme_minimal()

print(p6)

ggsave(
  "figures/19_model4_residual_histogram.png",
  p6,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 12. Breusch-Pagan Test
# ============================================================
# Important:
# The full models contain many Suburb coefficients.
# Therefore, the standard BP test can be memory-intensive.
#
cat("\n============================================================\n")
cat("BREUSCH-PAGAN TEST - MODEL 2\n")
cat("============================================================\n\n")

bp_2 <- tryCatch(
  {
    bptest(model_2)
  },
  error = function(e) {
    cat("Model 2 BP test could not be completed.\n")
    cat("Reason:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(bp_2)) {
  print(bp_2)
}


cat("\n============================================================\n")
cat("BREUSCH-PAGAN TEST - MODEL 4\n")
cat("============================================================\n\n")

bp_4 <- tryCatch(
  {
    bptest(model_4)
  },
  error = function(e) {
    cat("Model 4 BP test could not be completed.\n")
    cat("Reason:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(bp_4)) {
  print(bp_4)
}


# ============================================================
# 13. Save Breusch-Pagan Results
# ============================================================

bp_results <- data.frame(
  Model = character(),
  Statistic = numeric(),
  DF = numeric(),
  P_Value = numeric(),
  Interpretation = character()
)

if (!is.null(bp_2)) {
  
  bp_results <- rbind(
    bp_results,
    data.frame(
      Model = "Model 2 - Without Bedroom2",
      Statistic = as.numeric(bp_2$statistic),
      DF = as.numeric(bp_2$parameter),
      P_Value = bp_2$p.value,
      Interpretation = ifelse(
        bp_2$p.value < 0.05,
        "Evidence of heteroscedasticity",
        "No strong evidence of heteroscedasticity"
      )
    )
  )
}

if (!is.null(bp_4)) {
  
  bp_results <- rbind(
    bp_results,
    data.frame(
      Model = "Model 4 - Log Price",
      Statistic = as.numeric(bp_4$statistic),
      DF = as.numeric(bp_4$parameter),
      P_Value = bp_4$p.value,
      Interpretation = ifelse(
        bp_4$p.value < 0.05,
        "Evidence of heteroscedasticity",
        "No strong evidence of heteroscedasticity"
      )
    )
  )
}

if (nrow(bp_results) > 0) {
  
  cat("\n================ BREUSCH-PAGAN RESULTS ================\n")
  print(bp_results)
  
  write_csv(
    bp_results,
    "results/breusch_pagan_comparison.csv"
  )
}


# ============================================================
# 14. Basic Residual Summary
# ============================================================

diagnostic_summary <- data.frame(
  
  Model = c(
    "Model 2 - Without Bedroom2",
    "Model 4 - Log Price"
  ),
  
  Mean_Residual = c(
    mean(diagnostic_2$Residuals),
    mean(diagnostic_4$Residuals)
  ),
  
  SD_Residual = c(
    sd(diagnostic_2$Residuals),
    sd(diagnostic_4$Residuals)
  ),
  
  Mean_Absolute_Residual = c(
    mean(abs(diagnostic_2$Residuals)),
    mean(abs(diagnostic_4$Residuals))
  ),
  
  Median_Absolute_Residual = c(
    median(abs(diagnostic_2$Residuals)),
    median(abs(diagnostic_4$Residuals))
  ),
  
  Max_Absolute_Residual = c(
    max(abs(diagnostic_2$Residuals)),
    max(abs(diagnostic_4$Residuals))
  )
)


cat("\n============================================================\n")
cat("DIAGNOSTIC SUMMARY\n")
cat("============================================================\n\n")

print(diagnostic_summary)

write_csv(
  diagnostic_summary,
  "results/diagnostic_summary.csv"
)



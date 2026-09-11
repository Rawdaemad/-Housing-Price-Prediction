
library(dplyr)
library(readr)


model_1 <- readRDS(
  "results/model_1_baseline.rds"
)

model_2 <- readRDS(
  "results/model_2_without_bedroom2.rds"
)

model_3 <- readRDS(
  "results/model_3_without_location_categories.rds"
)



cat("\n============================================\n")
cat("ANOVA - NESTED MODEL COMPARISON\n")
cat("============================================\n")

cat("\nModel 1 - Baseline:\n")
cat("Parameters:", length(coef(model_1)), "\n")

cat("\nModel 2 - Without Bedroom2:\n")
cat("Parameters:", length(coef(model_2)), "\n")

cat("\nModel 3 - Without Location Categories:\n")
cat("Parameters:", length(coef(model_3)), "\n")


# ============================================================
# 3. ANOVA: Model 1 vs Model 2
# ============================================================

cat("\n============================================\n")
cat("ANOVA: MODEL 1 vs MODEL 2\n")
cat("============================================\n")

cat("\nQuestion:\n")
cat("Does removing Bedroom2 significantly reduce model fit?\n")

anova_1_2 <- anova(
  model_2,
  model_1
)

print(anova_1_2)


# ============================================================
# 4. ANOVA: Model 2 vs Model 3
# ============================================================

cat("\n============================================\n")
cat("ANOVA: MODEL 2 vs MODEL 3\n")
cat("============================================\n")

cat("\nQuestion:\n")
cat("Does removing location categories significantly reduce model fit?\n")

anova_2_3 <- anova(
  model_3,
  model_2
)

print(anova_2_3)


# ============================================================
# 5. Convert ANOVA Results to Data Frames
# ============================================================

anova_1_2_df <- as.data.frame(anova_1_2)

anova_1_2_df$Model <- rownames(anova_1_2_df)

rownames(anova_1_2_df) <- NULL


anova_2_3_df <- as.data.frame(anova_2_3)

anova_2_3_df$Model <- rownames(anova_2_3_df)

rownames(anova_2_3_df) <- NULL


# ============================================================
# 6. Extract P-values
# ============================================================

p_model1_vs_model2 <- anova_1_2$`Pr(>F)`[2]

p_model2_vs_model3 <- anova_2_3$`Pr(>F)`[2]


# ============================================================
# 7. Statistical Interpretation
# ============================================================

interpret_anova <- function(p_value, removed_feature) {
  
  if (is.na(p_value)) {
    
    return(
      "P-value could not be calculated."
    )
    
  }
  
  if (p_value < 0.05) {
    
    return(
      paste(
        "Removing",
        removed_feature,
        "causes a statistically significant reduction in model fit."
      )
    )
    
  } else {
    
    return(
      paste(
        "Removing",
        removed_feature,
        "does not cause a statistically significant reduction in model fit."
      )
    )
  }
}


interpretation_1_2 <- interpret_anova(
  p_model1_vs_model2,
  "Bedroom2"
)


interpretation_2_3 <- interpret_anova(
  p_model2_vs_model3,
  "location categories"
)


# ============================================================
# 8. Create ANOVA Summary
# ============================================================

anova_summary <- data.frame(
  
  Comparison = c(
    "Model 1 vs Model 2",
    "Model 2 vs Model 3"
  ),
  
  Removed_Feature = c(
    "Bedroom2",
    "Suburb + Regionname"
  ),
  
  P_Value = c(
    p_model1_vs_model2,
    p_model2_vs_model3
  ),
  
  Decision = c(
    
    ifelse(
      is.na(p_model1_vs_model2),
      "P-value unavailable",
      
      ifelse(
        p_model1_vs_model2 < 0.05,
        "Significant difference",
        "No significant difference"
      )
    ),
    
    ifelse(
      is.na(p_model2_vs_model3),
      "P-value unavailable",
      
      ifelse(
        p_model2_vs_model3 < 0.05,
        "Significant difference",
        "No significant difference"
      )
    )
  ),
  
  Interpretation = c(
    interpretation_1_2,
    interpretation_2_3
  )
)


# ============================================================
# 9. Print ANOVA Summary
# ============================================================

cat("\n============================================\n")
cat("ANOVA SUMMARY\n")
cat("============================================\n")

print(anova_summary)


# ============================================================
# 10. Save ANOVA Results
# ============================================================

dir.create(
  "results",
  showWarnings = FALSE
)


write_csv(
  anova_1_2_df,
  "results/anova_model1_vs_model2.csv"
)


write_csv(
  anova_2_3_df,
  "results/anova_model2_vs_model3.csv"
)


write_csv(
  anova_summary,
  "results/anova_summary.csv"
)


# ============================================================
# 11. Final Interpretation
# ============================================================

cat("\n============================================\n")
cat("FINAL INTERPRETATION\n")
cat("============================================\n")


cat("\nModel 1 vs Model 2:\n")
cat(interpretation_1_2, "\n")


cat("\nModel 2 vs Model 3:\n")
cat(interpretation_2_3, "\n")






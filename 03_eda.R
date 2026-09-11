
library(readr)
library(dplyr)
library(ggplot2)


# ============================================================
# 2. Load Clean Modeling Data
# ============================================================

model_df <- read_csv(
  "data/modeling_data.csv",
  show_col_types = FALSE
)

cat("Dataset dimensions:\n")
print(dim(model_df))

cat("\n================ STRUCTURE ================\n")
str(model_df)


# ============================================================
# 3. Create Output Folders
# ============================================================

dir.create(
  "figures",
  showWarnings = FALSE
)

dir.create(
  "results",
  showWarnings = FALSE
)


# ============================================================
# 4. General Summary
# ============================================================

cat("\n================ SUMMARY ================\n")
print(summary(model_df))


# ============================================================
# 5. Missing Values
# ============================================================

missing_summary <- data.frame(
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

cat("\n================ MISSING VALUES ================\n")
print(missing_summary)

write_csv(
  missing_summary,
  "results/eda_missing_summary.csv"
)


# ============================================================
# 6. Create Date Features for EDA
# ============================================================

model_df <- model_df %>%
  mutate(
    SaleYear = as.numeric(
      format(Date, "%Y")
    ),
    SaleMonth = as.numeric(
      format(Date, "%m")
    )
  )

cat("\nDate features created for EDA.\n")


# ============================================================
# 7. Numeric Variables
# ============================================================

num_house <- model_df %>%
  select(where(is.numeric))

cat("\n================ NUMERIC VARIABLES ================\n")
print(names(num_house))

cat("\n================ NUMERIC SUMMARY ================\n")
print(summary(num_house))


# ============================================================
# 8. Price Summary
# ============================================================

price_summary <- model_df %>%
  summarise(
    Mean_Price = mean(
      Price,
      na.rm = TRUE
    ),
    Median_Price = median(
      Price,
      na.rm = TRUE
    ),
    SD_Price = sd(
      Price,
      na.rm = TRUE
    ),
    Min_Price = min(
      Price,
      na.rm = TRUE
    ),
    Max_Price = max(
      Price,
      na.rm = TRUE
    )
  )

cat("\n================ PRICE SUMMARY ================\n")
print(price_summary)

write_csv(
  price_summary,
  "results/price_summary.csv"
)


# ============================================================
# 9. Price Distribution
# ============================================================

p1 <- ggplot(
  model_df,
  aes(x = Price)
) +
  geom_histogram(
    bins = 50,
    alpha = 0.7
  ) +
  labs(
    title = "Distribution of House Prices",
    x = "Price",
    y = "Number of Houses"
  ) +
  theme_minimal()

print(p1)

ggsave(
  "figures/01_price_distribution.png",
  p1,
  width = 10,
  height = 6
)


# ============================================================
# 10. Log Price Distribution
# ============================================================

p2 <- ggplot(
  model_df,
  aes(x = log(Price))
) +
  geom_histogram(
    bins = 50,
    alpha = 0.7
  ) +
  labs(
    title = "Distribution of Log House Prices",
    x = "Log(Price)",
    y = "Number of Houses"
  ) +
  theme_minimal()

print(p2)

ggsave(
  "figures/02_log_price_distribution.png",
  p2,
  width = 10,
  height = 6
)


# ============================================================
# 11. Price Boxplot
# ============================================================

p3 <- ggplot(
  model_df,
  aes(y = Price)
) +
  geom_boxplot() +
  labs(
    title = "Boxplot of House Prices",
    y = "Price"
  ) +
  theme_minimal()

print(p3)

ggsave(
  "figures/03_price_boxplot.png",
  p3,
  width = 7,
  height = 6
)


# ============================================================
# 12. Correlation Matrix
# ============================================================

cor_matrix <- cor(
  num_house,
  use = "pairwise.complete.obs"
)

cat("\n================ CORRELATION MATRIX ================\n")
print(
  round(
    cor_matrix,
    3
  )
)

write.csv(
  cor_matrix,
  "results/correlation_matrix.csv"
)


# ============================================================
# 13. Correlation Heatmap
# ============================================================

png(
  "figures/04_correlation_heatmap.png",
  width = 1000,
  height = 800
)

heatmap(
  cor_matrix,
  scale = "none",
  margins = c(10, 10)
)

dev.off()


# ============================================================
# 14. Price Correlation with Numeric Variables
# ============================================================

price_correlations <- cor_matrix[
  ,
  "Price"
]

price_correlations <- sort(
  price_correlations,
  decreasing = TRUE
)

cat("\n================ PRICE CORRELATIONS ================\n")
print(
  round(
    price_correlations,
    3
  )
)

price_cor_df <- data.frame(
  Variable = names(price_correlations),
  Correlation_with_Price =
    as.numeric(price_correlations)
)

write_csv(
  price_cor_df,
  "results/price_correlations.csv"
)


# ============================================================
# 15. Price vs Rooms
# ============================================================

p4 <- ggplot(
  model_df,
  aes(
    x = Rooms,
    y = Price
  )
) +
  geom_point(
    alpha = 0.3
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "House Price vs Number of Rooms",
    x = "Number of Rooms",
    y = "Price"
  ) +
  theme_minimal()

print(p4)

ggsave(
  "figures/05_price_vs_rooms.png",
  p4,
  width = 10,
  height = 6
)


# ============================================================
# 16. Price vs Bathroom
# ============================================================

p5 <- ggplot(
  model_df,
  aes(
    x = Bathroom,
    y = Price
  )
) +
  geom_point(
    alpha = 0.3
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "House Price vs Number of Bathrooms",
    x = "Number of Bathrooms",
    y = "Price"
  ) +
  theme_minimal()

print(p5)

ggsave(
  "figures/06_price_vs_bathroom.png",
  p5,
  width = 10,
  height = 6
)


# ============================================================
# 17. Price vs Building Area
# ============================================================

p6 <- ggplot(
  model_df,
  aes(
    x = BuildingArea,
    y = Price
  )
) +
  geom_point(
    alpha = 0.3
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "House Price vs Building Area",
    x = "Building Area",
    y = "Price"
  ) +
  theme_minimal()

print(p6)

ggsave(
  "figures/07_price_vs_building_area.png",
  p6,
  width = 10,
  height = 6
)


# ============================================================
# 18. Price vs Land Size
# ============================================================

p7 <- ggplot(
  model_df,
  aes(
    x = Landsize,
    y = Price
  )
) +
  geom_point(
    alpha = 0.3
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "House Price vs Land Size",
    x = "Land Size",
    y = "Price"
  ) +
  theme_minimal()

print(p7)

ggsave(
  "figures/08_price_vs_landsize.png",
  p7,
  width = 10,
  height = 6
)


# ============================================================
# 19. Price vs Distance
# ============================================================

p8 <- ggplot(
  model_df,
  aes(
    x = Distance,
    y = Price
  )
) +
  geom_point(
    alpha = 0.3
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "House Price vs Distance from CBD",
    x = "Distance from CBD",
    y = "Price"
  ) +
  theme_minimal()

print(p8)

ggsave(
  "figures/09_price_vs_distance.png",
  p8,
  width = 10,
  height = 6
)


# ============================================================
# 20. Boxplots for Important Numeric Variables
# ============================================================

important_numeric <- c(
  "Rooms",
  "Bathroom",
  "Car",
  "Landsize",
  "BuildingArea",
  "Distance"
)

for (var in important_numeric) {
  
  p <- ggplot(
    model_df,
    aes(
      y = .data[[var]]
    )
  ) +
    geom_boxplot() +
    labs(
      title = paste(
        "Boxplot of",
        var
      ),
      y = var
    ) +
    theme_minimal()
  
  print(p)
  
  ggsave(
    paste0(
      "figures/boxplot_",
      var,
      ".png"
    ),
    p,
    width = 7,
    height = 6
  )
}


# ============================================================
# 21. Histograms for Important Numeric Variables
# ============================================================

for (var in important_numeric) {
  
  p <- ggplot(
    model_df,
    aes(
      x = .data[[var]]
    )
  ) +
    geom_histogram(
      bins = 40,
      alpha = 0.7
    ) +
    labs(
      title = paste(
        "Distribution of",
        var
      ),
      x = var,
      y = "Number of Houses"
    ) +
    theme_minimal()
  
  print(p)
  
  ggsave(
    paste0(
      "figures/histogram_",
      var,
      ".png"
    ),
    p,
    width = 9,
    height = 6
  )
}


# ============================================================
# 22. Price by Property Type
# ============================================================

type_summary <- model_df %>%
  group_by(Type) %>%
  summarise(
    Mean_Price = mean(
      Price,
      na.rm = TRUE
    ),
    Median_Price = median(
      Price,
      na.rm = TRUE
    ),
    Number_of_Houses = n()
  ) %>%
  ungroup() %>%
  arrange(
    desc(Mean_Price)
  )

cat("\n================ PRICE BY TYPE ================\n")
print(type_summary)

write_csv(
  type_summary,
  "results/price_by_type.csv"
)


p9 <- ggplot(
  type_summary,
  aes(
    x = reorder(
      Type,
      Mean_Price
    ),
    y = Mean_Price
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average House Price by Property Type",
    x = "Property Type",
    y = "Average Price"
  ) +
  theme_minimal()

print(p9)

ggsave(
  "figures/10_price_by_type.png",
  p9,
  width = 10,
  height = 6
)


# ============================================================
# 23. Price by Region
# ============================================================

region_summary <- model_df %>%
  group_by(Regionname) %>%
  summarise(
    Mean_Price = mean(
      Price,
      na.rm = TRUE
    ),
    Median_Price = median(
      Price,
      na.rm = TRUE
    ),
    Number_of_Houses = n()
  ) %>%
  ungroup() %>%
  arrange(
    desc(Mean_Price)
  )

cat("\n================ PRICE BY REGION ================\n")
print(region_summary)

write_csv(
  region_summary,
  "results/price_by_region.csv"
)


p10 <- ggplot(
  region_summary,
  aes(
    x = reorder(
      Regionname,
      Mean_Price
    ),
    y = Mean_Price
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average House Price by Region",
    x = "Region",
    y = "Average Price"
  ) +
  theme_minimal()

print(p10)

ggsave(
  "figures/11_price_by_region.png",
  p10,
  width = 10,
  height = 6
)


# ============================================================
# 24. Average Price by Number of Rooms
# ============================================================

rooms_summary <- model_df %>%
  group_by(Rooms) %>%
  summarise(
    Mean_Price = mean(
      Price,
      na.rm = TRUE
    ),
    Median_Price = median(
      Price,
      na.rm = TRUE
    ),
    Number_of_Houses = n()
  ) %>%
  ungroup() %>%
  arrange(Rooms)

cat("\n================ PRICE BY ROOMS ================\n")
print(rooms_summary)

write_csv(
  rooms_summary,
  "results/rooms_summary.csv"
)


p11 <- ggplot(
  rooms_summary,
  aes(
    x = Rooms,
    y = Mean_Price
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average House Price by Number of Rooms",
    x = "Number of Rooms",
    y = "Average Price"
  ) +
  theme_minimal()

print(p11)

ggsave(
  "figures/12_average_price_by_rooms.png",
  p11,
  width = 10,
  height = 6
)


# ============================================================
# 25. Price by Year
# ============================================================

year_summary <- model_df %>%
  group_by(SaleYear) %>%
  summarise(
    Mean_Price = mean(
      Price,
      na.rm = TRUE
    ),
    Median_Price = median(
      Price,
      na.rm = TRUE
    ),
    Number_of_Houses = n()
  ) %>%
  ungroup() %>%
  arrange(SaleYear)

cat("\n================ PRICE BY YEAR ================\n")
print(year_summary)

write_csv(
  year_summary,
  "results/year_summary.csv"
)


p12 <- ggplot(
  year_summary,
  aes(
    x = SaleYear,
    y = Mean_Price
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average House Price by Year",
    x = "Sale Year",
    y = "Average Price"
  ) +
  theme_minimal()

print(p12)

ggsave(
  "figures/13_average_price_by_year.png",
  p12,
  width = 10,
  height = 6
)


# ============================================================
# 26. Outlier Investigation
# ============================================================

outlier_variables <- c(
  "Price",
  "Landsize",
  "BuildingArea"
)

outlier_summary <- data.frame()

for (var in outlier_variables) {
  
  x <- model_df[[var]]
  
  Q1 <- quantile(
    x,
    0.25,
    na.rm = TRUE
  )
  
  Q3 <- quantile(
    x,
    0.75,
    na.rm = TRUE
  )
  
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 -
    1.5 * IQR_value
  
  upper_bound <- Q3 +
    1.5 * IQR_value
  
  outliers <- sum(
    x < lower_bound |
      x > upper_bound,
    na.rm = TRUE
  )
  
  outlier_summary <- rbind(
    outlier_summary,
    data.frame(
      Variable = var,
      Q1 = Q1,
      Q3 = Q3,
      IQR = IQR_value,
      Lower_Bound = lower_bound,
      Upper_Bound = upper_bound,
      Number_of_Outliers = outliers
    )
  )
}

cat(
  "\n================ OUTLIER SUMMARY ================\n"
)

print(outlier_summary)

write_csv(
  outlier_summary,
  "results/outlier_summary.csv"
)




# 🏠 Melbourne Housing Price Prediction

> **End-to-End Regression Analysis & House Price Prediction using R**

An end-to-end data science project focused on understanding and predicting residential property prices in Melbourne, Australia.

The project covers the complete machine learning workflow — from data cleaning and exploratory analysis to regression modeling, statistical testing, diagnostics, and final model evaluation.

---

## 📌 Project Overview

### 🎯 Objective

The main objective is to identify the key factors associated with Melbourne house prices and develop a regression model capable of predicting prices for unseen properties.

### 💡 Business Question

> **Can we predict Melbourne house prices using property characteristics and location information?**

The analysis investigates how factors such as:

- Property size
- Number of rooms
- Bathrooms
- Distance from the city center
- Property type
- Suburb
- Region
- Geographic coordinates
- Property age

contribute to house price variation.

---

## 📊 Final Model Performance

The selected model was evaluated on **unseen test data**.

| Metric | Result |
|---|---:|
| **R²** | **0.6774** |
| **RMSE** | **359,911** |
| **MAE** | **235,466** |

### What does this mean?

The model explains approximately **67.7% of the variation in house prices** in the test dataset.

The **MAE of approximately 235K** means that the model's average absolute prediction error is around 235,466 price units.

> **Final Model:** OLS Regression without `Bedroom2`

---

## 🔎 Key Insights

### 1. 📍 Location is a Major Predictor

Location-related variables such as `Suburb` and `Regionname` have a strong impact on predictive performance.

When these variables were removed, test performance dropped substantially.

This indicates that **where a property is located is highly important for predicting its price**.

---

### 2. 🏡 High-Value Properties Are More Difficult to Predict

The model performs reasonably well for many properties but struggles with some very high-priced properties.

For example, one of the largest errors was:

```text
Actual Price:     6,460,000
Predicted Price:  2,275,582

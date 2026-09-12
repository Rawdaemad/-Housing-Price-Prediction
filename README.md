# Melbourne Housing Price Prediction

An end-to-end house price prediction project built in R using the Melbourne Housing dataset.

The project focuses on understanding the factors associated with house prices, building and comparing multiple regression models, validating model assumptions, and selecting a final predictive model based on unseen test data.

---

## Project Overview

### Business Problem

House prices are influenced by several factors such as property size, number of rooms, location, distance from the city center, property type, and other characteristics.

The objective of this project is to build a statistical machine learning pipeline that can:

- Clean and prepare housing data
- Explore the main factors associated with house prices
- Engineer useful predictive features
- Detect multicollinearity
- Build and compare multiple regression models
- Diagnose regression assumptions
- Investigate heteroscedasticity
- Evaluate Weighted Least Squares (WLS)
- Select the best-performing predictive model on unseen data

---

## Dataset

The dataset contains Melbourne residential property sales and includes variables such as:

- Price
- Rooms
- Distance
- Bedroom2
- Bathroom
- Car
- Landsize
- BuildingArea
- YearBuilt
- Latitude
- Longitude
- Suburb
- Type
- Method
- Regionname
- Date

The raw dataset is not included in this repository when distribution restrictions or file-size limitations apply.

---

## Project Workflow

```text
Raw Data
   ↓
Data Cleaning
   ↓
Feature Engineering
   ↓
Train / Test Split
   ↓
Missing Value Handling
   ↓
Exploratory Data Analysis
   ↓
Multicollinearity Analysis (VIF)
   ↓
Regression Model Development
   ↓
Model Diagnostics
   ↓
Heteroscedasticity Testing
   ↓
Weighted Least Squares
   ↓
ANOVA Model Comparison
   ↓
Final Model Comparison
   ↓
Final Evaluation

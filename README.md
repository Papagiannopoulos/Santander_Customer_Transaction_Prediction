## Which customers will make a specific transaction in the future?

**Aim**: Identify which customers will make a specific transaction in the future

## Project Summary

This project referred to the 2nd most popular [Kaggle competition]( https://www.kaggle.com/c/santander-customer-transaction-prediction),
where ***i scored at the top 8% Global rank***.  
In this imbalanced classification problem, **data are anonymized**.  
I performed a *deep EDA and data manipulation* including preprocessing & feature engineering
to understan the underlying data pattenrs, building predictive models such as lightGMB and logisting regression.  
To further familiarized with Shiny app, **all visualizations additionally created using Rshiny**.

## Project Structure

### Table of Contents
1. 🔍 **[ Dataset](#dataset)** - Data description & source  
2. 🧹 **[ Data Processing](#-data-processing)** - Cleaning and feature engineering  
3. 🔁 **[ Reproducibility](#-reproducibility)** - Reproducibility steps  
4. 📦 **[ Requirements](#-requirements)** - Install dependencies  

## Dataset
Dataset is anonymized containing 200 numeric feature variables, the binary “target” column, and a string “ID_code” column.  
The task is to predict the value of “target” column in the test set.  
- **[Download source](https://www.kaggle.com/competitions/santander-customer-transaction-prediction/data) provided for this competition has the same structure as the real data where Santander has available to solve this problem.**

## Evaluation
Submissions are evaluated on the Area Under the Curve (AUC) between the predicted probability and the observed target.

## 🔁 Reproducibility
1) Clone the repo!  
Run the following commands in terminal:    
		git clone 'https://github.com/Papagiannopoulos/Santander_Customer_Transaction_Prediction.git'  
		cd 'Santander_Customer_Transaction_Prediction'
2) Download the data & add them in your cloned repo  
3) Run the requirements.R

## 📦 Requirements
- Before running the Shiny app.R, run the requirements script to install all necessary libraries
- R version: 4.4.1 (2024-06-14 ucrt)

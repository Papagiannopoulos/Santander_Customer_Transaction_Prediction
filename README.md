## Which customers will make a specific transaction in the future?

This repository referred to the second most popular [Kaggle competition]( https://www.kaggle.com/c/santander-customer-transaction-prediction), where I scored in the top 8% Global rank.
In this imbalanced classification problem, I identified which customers will make a specific transaction in the future, irrespective of the amount of money transacted.
I have included codes (Rscript, Rmarkdown, ShinyApp) where I am describing all the steps I followed (data preprocessing, EDA, feature engineering, model development).
The models I used was logistic regressions and LightGBM.

#### Evaluation
Submissions are evaluated on the area under the ROC curve between the predicted probability and the observed target.

#### Dataset Description
Dataset is anonymized containing 200 numeric feature variables, the binary “target” column, and a string “ID_code” column.
The task is to predict the value of “target” column in the test set.

#### Data Availability
The [data](https://www.kaggle.com/competitions/santander-customer-transaction-prediction/data) provided for this competition has the same structure as the real data where Santander has available to solve this problem.

#### Reproducibility
After downloading the data, set your local path where you have included the data run the codes

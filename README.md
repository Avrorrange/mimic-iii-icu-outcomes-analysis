# MIMIC-III ICU Outcomes Analysis

## Overview

This project analyses Intensive Care Unit (ICU) outcomes using the MIMIC-III clinical database.

The project consists of two related analyses:

1. **ICU mortality analysis** – logistic regression was used to investigate factors associated with in-hospital mortality and whether mortality differed between ICU types after accounting for patient characteristics and clinical factors.
2. **Long ICU stay prediction** – machine learning was used to predict whether a patient would remain in the ICU for 7 days or longer using information available during the first 24 hours of admission.

The project involved substantial data cleaning, merging of multiple clinical datasets, exploratory analysis, statistical modelling and machine-learning model development.

## Tools

* **R** – data cleaning, data integration, exploratory analysis and statistical modelling
* **Python** – machine-learning development and evaluation
* **XGBoost** – long-stay classification
* **Logistic regression** – ICU mortality modelling
* **RStudio**
* **Jupyter Notebook**

## Part 1: ICU Mortality Analysis

The first analysis investigated factors associated with in-hospital mortality and whether ICU type was independently associated with mortality.

Multiple MIMIC-III tables containing information on admissions, laboratory measurements, vital signs, ventilation, vasopressor use, diagnoses and patient outcomes were combined using ICU stay identifiers.

Data preparation included:

* restricting the analysis to adult ICU patients
* removing implausible physiological measurements
* summarising repeated measurements using early-stay values
* calculating Charlson Comorbidity Index scores
* examining missing data
* assessing multicollinearity
* selecting variables for statistical modelling

A multivariable logistic regression model was then developed to model in-hospital mortality.

### Key Result

The mortality model achieved an **AUC of 0.889**.

After adjustment for other variables, ICU type remained associated with mortality. For example:

* CSRU patients had lower odds of mortality than the CCU reference group
  **OR = 0.258, 95% CI 0.205–0.323**
* MICU patients had higher odds of mortality than the CCU reference group
  **OR = 1.254, 95% CI 1.077–1.463**

These results should be interpreted as associations rather than evidence that ICU type itself causes differences in mortality.

## Part 2: Long ICU Stay Prediction

The second analysis investigated whether early clinical information could predict a prolonged ICU stay.

A **long stay** was defined as an ICU stay of **7 days or more**.

Clinical information from approximately the first 24 hours of ICU admission was used to construct the prediction dataset.

Data preparation was performed in R before the cleaned dataset was exported to Python for machine-learning development.

### Machine-Learning Approach

XGBoost was selected because it:

* can model non-linear relationships
* can handle missing values internally
* performs well with mixed clinical predictors
* is suitable for imbalanced classification problems

Three versions of the model were investigated using progressively smaller feature sets.

Hyperparameters including tree depth, learning rate, number of estimators, subsampling and column sampling were tuned.

### Key Result

The strongest model achieved approximately:

* **ROC AUC: 0.848**
* **AUPRC: 0.584**

The results suggest that prolonged ICU stays can be predicted to some extent using information available early in a patient's ICU admission, although further model refinement would be required for clinical use.

## Repository Files

* `mortality-data-cleaning-and-analysis.R`
  Data cleaning, exploratory analysis and logistic-regression modelling for the mortality analysis.

* `long-stay-data-cleaning.R`
  Data preparation and feature engineering for the long-stay prediction analysis.

* `long-stay-machine-learning.ipynb`
  Python/Jupyter notebook containing XGBoost model development and evaluation.

* `mimic-iii-icu-outcomes-report.pdf`
  Full project report containing methodology, results, figures and discussion.

## Data

This project uses the **MIMIC-III critical care database**.

The source dataset is not included in this repository. Users wishing to reproduce the analysis must obtain access to MIMIC-III separately and configure the source data paths used by the analysis scripts.

## Limitations

Important limitations include:

* substantial missingness in some clinical variables
* exclusion of records with incomplete or inconsistent information
* reliance on data from a single medical centre
* limited information for some potentially important patient characteristics
* the observational nature of the mortality analysis
* class imbalance in the long-stay prediction problem

The machine-learning model is an analytical project and is **not intended for clinical decision-making**.

## Author

Christopher Leung

Master of Health Data Science
UNSW

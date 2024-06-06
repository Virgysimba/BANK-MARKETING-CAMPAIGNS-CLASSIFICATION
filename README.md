# CLASSIFICATION OF BANK MARKETING CAMPAIGNS

## Introduction
Digital marketing campaigns are ubiquitous as organizations compete for their client's attention. In this project I explore the challenge of creating an effective marketing strategy in the context of banks. The data set is taken from UCI's Machine Learning Repository, you can find a detailed description [here](https://archive.ics.uci.edu/ml/datasets/Bank+Marketing). Using the data from the previously carried out marketing campaign and client information I build models to predict whether a client will subscribe to a term deposit or not in a future marketing campaign. Ultimately, the goal is to enable the marketing team to optimize their resources to reach out to those clients who have the highest likelihood to need and subscribe to the term deposit.

## Data Set Description
- Source: Marketing campaign data from a Portuguese Bank (2008 to 2010)
- Size: 41188 records, 20 explanatory variables, and 1 binary response variable
- Skewed dataset
   - 11% positive responses
- 23% of total records have missing Values in at least one explanatory variable 
    
## Data Set Preprocessing
The original data set contains explanatory variables that falls into one of the following categories:
1. Client Demographic Data
2. Current Campaign Data
3. Previous Campaign Data
4. Social and Economic Attributes

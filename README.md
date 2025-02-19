# Obesity_In_Jordan

## Abstract: 
	
The goal of this study is to find the statistical relations between the main and important variables, mainly the obesity indicator variable. The study is based on an incomplete dataset that has recorded several questions and concluded metrics from the questions. This data is based on a survey that’s been filled out by over 900 Jordanians. Through different analytics, this study will uncover any correlation between obesity and several other variables.

## 1- Introduction

Obesity is an unnaturally large culmination of fat within a human’s body. It produces a dangerous risk to the individual’s life and its quality. It can lead to the introduction of many diseases, with heart disease being the most prominent.

1 in 8 people in the world are diagnosed with obesity, and because of its danger and widespread influence, it’s important to study its factors and independent variables.

This study aims to do so by studying the main variables and how they correlate to obesity, like PSQI, BRS, K10, family history of overweightness, age, gender, city, mode of transport, and main food sections eaten at a weekly basis.

## 2- Methodology

There are three main stages that have gone to this study: Data cleaning, data exploratory analysis, statistical tests, and correlation.

### 2.1- Data Cleaning

Removing Null Data:

First thing done, we observed the data directly from its source file and found almost half of it being missing.

For example, rows 1001 – 1741 only had 4 out of 47 variables filled, and rows 1742 – 1882 only had one variable filled!

Therefore, it was wise to only take the first 1000 data points into study, like so:

	df = df[0:1000, ]

After this, we detected that there were 129 rows with missing data. To resolve the situation, we took the columns that had the missing data and extracted each of their medians:

	na_cols <- names(which(colSums(is.na(df)) > 0))

	medians <- apply(df[apply(df, 1, function(x) !any(is.na(x))), na_cols], 2, median)

	After extracting the medians, we replaced the NA values of each column with their respective medians:

	i <- 1

	for(value in medians) {
  
		df[na_cols[i]][is.na(df[na_cols[i]])] <- value
  
	  i <- i + 1
	}

 Renaming Long Column Names:
	
Due to the long column names, we found it to be unorthodox to use them each time we call for their respective data. To solve this:

	colnames(df) <- c("Gender", "Age", "Family_History_Overweight", "Food_Between_Meals", "Smoke", "Transportaion",
                  "City", "Physical_Activity_Time", "Breakfast_In_Week", "Grains_In_Week", "Dairy_In_Week",
                  "Caffeine_In_Week", "Seafood_In_Week", "BRS_Q1", "BRS_Q2", "BRS_Q3", "BRS_Q4", "BRS_Q5", "BRS_Q6",
                  "K10_Q1", "K10_Q2", "K10_Q3", "K10_Q4", "K10_Q5", "K10_Q6", "K10_Q7", "K10_Q8", "K10_Q9", "K10_Q10",
                  "PSQI_Q1", "PSQI_Q2", "PSQI_Q3", "PSQI_Q4", "PSQI_Q5A", "PSQI_Q5B", "PSQI_Q5C", "PSQI_Q5D", "PSQI_Q5E",
                  "PSQI_Q5F", "PSQI_Q5G", "PSQI_Q5H", "PSQI_Q5I", "PSQI_Q6", "PSQI_Q7", "PSQI_Q8", "PSQI_Q9", "Obesity_Indicator")

This made it significantly easier.


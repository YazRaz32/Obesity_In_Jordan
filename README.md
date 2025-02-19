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

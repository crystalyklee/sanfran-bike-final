STEP 1: Conduct an EDA on the trip and weather datasets
The data analyst should run an exploratory data analysis (EDA) on the trip and weather data using funModeling

STEP 2: Determine cancelled trips: less than 3 mins, start station = end station
Record trip ids in the report and remove from data set 
Identify duplicates

Step 3: Identify outliers
Use EDA to determine which columns
Record trip ids for report
Remove from dataset

Step 4: Determine rush hours
Find hours in weekday where trip volume is the highest
Use histograms to look at distribution of trip volumes per hour in each day of the week 

Step 5: Determine 10 most frequent starting and ending stations during the rush hours of each weekday

Step 6: Determine 10 most frequent starting and ending stations during the weekends

Step 7: Calculate average utilization of bikes for each month
Total time used or total time per month

Step 8: Create new weather dataset
Combine trip data with weather data 
Join using the dates 

Step 9: Correlation matrix of new dataset using cor() from corrplot package
Flag highest correlations

You team lead expects a Data Analysis Report with all your findings for the next meeting in two weeks. You
will create a small research report (preferably a Word-compatible file). Your report should include a brief
description and summary of the dataset (using tables/plots from EDA), describe any pre-processing you
applied to the dataset (e.g. number of excluded records, exclusion reason), the requirements from your
team and your findings.

Notes:
Prefer to use ‘tidyverse’ packages when appropriate for the purposes of this exercise.
Create a word file for the report including a brief description and summary of the data set using tables and plots
Describe pre-processing (number of excluded records and why they were excluded)
Requirements from the team
Findings
Add your instructor (SebnemSK) as a collaborator.

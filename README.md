# R Code Sample - OLS Regression and Data Visualization

RQ: How does the presence of state-level Paid Family Leave (PFL) policy impact maternal and infant health outcomes? 

This code sample comes from my capstone project completed in conformity with the requirements of the degree of MS Data Analytics and Policy from Johns Hopkins University. The code sample uses data selected from KFF State Health Facts. KFF’s data compliation uses data from the Center for Disease Control, American Community Survey, Behavior Risk Factor Surveillance System, and National Vital Statistics Report. There is one csv file in this repository, WomensHealth5.0.csv, which contains the merged and cleaned data from the many csv files collected from KFF State Health Facts. Note that the CapstoneData.R file contains the code for merging and cleaning those csv files into the WomensHealth5.0.csv file.

After cleaning and merging the several datasets, I generated summary statistics for the dataset and exported them using the stargazer package. From there, I ran a series of ordinary least square (OLS) regression models on the selected dependent variables. For each dependent variable, three regression models are used to determine the sensitivity of the findings when additional control variables are included. First, a bivariate regression model estimates the relationship between a given dependent variable and implemented PFL (implemented). Then, two additional multivariate models are used for each dependent variable. 

Using the stargazer package, I generated tables for presenting the OLS regression results. The tables group dependent variables by infant health outcomes, maternal health outcomes, and breastfeeding practices.

To assist in the presentation of my data and the regression results I created several visualizations including choropleths, and bar graphs of dependent variables.   

No statistically significant relationships between the presence of implemented PFL and maternal and infant health outcomes were identified, however the results of the models merit further investigation into the impacts of PFL on maternal and infant health. Further study will be particularly pertinent once data is available for more years, allowing for more states to implement PFF legislation and to assess longer-term impacts. 


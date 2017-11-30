#!/usr/bin/Rscript
'
The following R script reads the csv file titled survey_data, and develops
four multinomial models:
1) salary_model
2) checking_amount_model
3) savings_model
4) savings_amount_model

These models, in turn, along with the tsv file entitled pop_dist are used to
identify the the demographic-specific responses for all 256 combinations of 
age, sex, gender, and education combinations; these results are saved in the
workind directory in the file demographic_predicted_responses.

These stratified responses are then used to provide the distribution of the US
adult population which would provide specific responses to the following four
survey questions:

1. What is your monthly salary (if none, choose 0)? 
2. Which range below (in US Dollars, USD) matches the balance in your checking account,
(if no account, choose 0-9)? 
3. Do you have a savings account? (yes/no)
4. Which range below (in US Dollars, USD) matches the balance in your savings account,
(if no account, choose 0-9)?
'

#removing memory
rm(list = objects())
#load libraries
library(tidyverse)
library(nnet)

#get and clean survey data
survey_df = read.csv("survey_data.csv", stringsAsFactors = FALSE)

#choose only the relevant columns
survey_df = survey_df[, 28:35]

#values were changed to dates, need to re-instate to intended figure
survey_df$Answer.Savings.Amount[survey_df$Answer.Savings.Amount == "Oct-49"] = "10-49"
survey_df$Answer.Checking.Amount[survey_df$Answer.Checking.Amount == "Oct-49"] = "10-49"


#simplify Column Names
colnames(survey_df) = c("age", "checking_amount","education", "sex", "race", 
                        "salary", "savings", "savings_amount")

#get population data
population_df = read.delim("pop_dist.tsv", stringsAsFactors=FALSE)

#Save total population number
Total_US = sum(population_df$N)

#build the models
salary_model = multinom(salary ~ age + education + sex + race, data = survey_df)
checking_amount_model = multinom(checking_amount ~ age + education + sex + race, 
                                 data = survey_df)
savings_model = multinom(savings ~ age + education + sex + race, data = survey_df)
savings_amount_model = multinom(savings_amount ~ age + education + sex + race, 
                                data = survey_df)

#make prediction
population_df$salary = predict(salary_model, type = "class", newdata = population_df)
population_df$checking_amount = predict(checking_amount_model, type = "class", 
                                        newdata = population_df)
population_df$savings = predict(savings_model, type = "class", newdata = population_df)
population_df$savings_amount = predict(savings_amount_model, type = "class", 
                                       newdata = population_df)

#save the dataframe in working directory
write.csv(population_df, "demographic_predicted_responses.csv")

#get US-level responses
#get the proportion of US citizens by salary response
US_salary = tapply(population_df$N, population_df$salary, sum) / Total_US
print(paste('The majority of adult US-residents have a monthly-salary range (in USD) of', 
            names(which(US_salary==max(US_salary)))))
print('The distribution looks like:')
US_salary

#get the proportion of US citizens by checking amount
US_checking_amount = tapply(population_df$N, population_df$checking_amount, sum) / Total_US
#print out the response based on the answer provided by the majority of the population
print(paste('The majority of adult US-residents have a checking account balance (in USD) of',
            names(which(US_checking_amount==max(US_checking_amount)))))
print('The distribution looks like:')
US_checking_amount

#get the proportion of US citizens by whether they have a savings account or not
US_savings=tapply(population_df$N, population_df$savings, sum)/Total_US
#print out the response based on the answer provided by the majority of the population
print(paste('Does the majority of adult US-residents have a savings account?',
            names(which(US_savings==max(US_savings)))))
print('The distribution looks like:')
US_savings

#get the proportion of US citizens by savings account balance
US_savings_amount = tapply(population_df$N, population_df$savings_amount, sum,
                           na.rm=TRUE) / Total_US
#replace any NAs with 0
US_savings_amount[ is.na(US_savings_amount) ] = 0
#print out the response based on the answer provided by the majority of the population
print(paste('The majority of adult US-residents have a savings account balance (in USD) of',
            names(which(US_savings_amount==max(US_savings_amount)))))
print('The distribution looks like:')
US_savings_amount

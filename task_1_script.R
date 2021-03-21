## set working directory
setwd('~/The Sparks Foundation/Task 1/')

## import libraries
library(ggplot2)
library(ggpubr)
library(dplyr)

## read students data
students_data = read.csv('student_scores - student_scores.csv')

## view first few rows
head(students_data)

## get dimensions of the data
dim(students_data)

## train linear regression model
lin_reg = lm(Scores ~ Hours, students_data)

## get linear regression output
summary(lin_reg)

## add predicted value and residual to the dataframe
students_data$Predicted = predict(lin_reg)
students_data$Residuals = resid(lin_reg)

## plot linear regression model with confidence interval
students_data %>%
        ggplot(aes(x = Hours, y = Scores)) + 
        geom_point(col = 'steelblue', size = 3) +
        geom_smooth(method = 'lm', color = 'red', lwd = 1, se = TRUE) + 
        theme_minimal() + 
        labs(title = 'Linear regression line with confidence interval')

## plot linear regression model with residual
students_data %>%
        ggplot(aes(x = Hours, y = Scores)) + 
        geom_point(col = 'steelblue', size = 3) +
        geom_line(aes(y = Predicted), col = 'red', lwd = 1) + 
        geom_segment(aes(xend = Hours, yend = Predicted), col = 'darkgrey', lwd = 0.5) + 
        theme_minimal() + 
        labs(title = 'Regression line along the actual data points with residuals')

## residual plot
students_data %>%
        ggplot(aes(x = Predicted, y = Residuals)) + 
        geom_point(col = 'steelblue', size = 3) + 
        geom_hline(yintercept = 0, col = 'red', lwd = 1)  + 
        geom_segment(aes(xend = Predicted, yend = 0), col = 'darkgrey', lwd = 0.5) + 
        theme_minimal() + 
        labs(title = 'Residual plot of the regression model')

## predicting score using hours
predict(lin_reg, newdata = data.frame(Hours = 9.25))

## predicting score using hours with confidence interval
predict(lin_reg, newdata = data.frame(Hours = 9.25), interval = 'confidence')

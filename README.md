# Linear-Regression-Model-Selection

## Summary:
Using R, I minimized Mallows' Cp values and predicted residual error sum of squares (PRESS) to improve the linear regression model's predictive ability for fuel efficiency (miles per gallon) in USA, Japan, and Germany. Both Least angle regression (Lars) and the branch-and-bound algorithm (leaps) aims at finding the best subset of predictors. The project explores how these iterative procedures compare in predicting the response variable of mpg. The following plots the predictions (x) against the true value (y). 

<img width="282" height="394" alt="Screen Shot 2025-10-06 at 5 09 06 AM" src="https://github.com/user-attachments/assets/fd639dfc-3d45-4e40-b7ac-13d4d39c2e28" />

## Dataset used:
This dataset is included in the ISLR package in R. 

## Business Problem:
Linear regression is utilized extensively in a wide array of fields such as stock trends, customer behavior analysis, process optimization, engineering products, etc. It is useful for finding relationships between variables and predicting behaviors. In this case, I looked at characteristics of cars such as horsepower, weight, time to accelerate, model year, etc. that could help predict fuel efficiency. Knowledge of a vehicle's mpg is an important selling point in car sales.

## Results:
The fuel efficiency predictions for Japan seem to overall be underestimated in comparison to the predictions of Germany and USA which have a higher slope in the prediction vs true value plot. This could possibly be interpreted as the independent variables of the model result in a lower prediction than in reality for Japan while it increases fuel efficiency in other ways not accounted for. The USA cars have the least fuel efficiency as it has a large cluster of data points close to the origin. I would recommend investigation into Japanese carmaker's engineering for further insight on making US domestic cars more competitive on the market. 


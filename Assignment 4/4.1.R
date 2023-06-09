# Load the required libraries
library(car)
library(glmnet)

# 1. Fit multiple linear regression with mpg as dependent variable and rest of the variables in the mtcars data as independent variables and save it as mlr object 
mlr <- lm(mpg ~ ., data = mtcars)

# Step 2: Get the summary of mlr
summary(mlr)

# We have multiple coefficients such as cyl, disp, hp, drat, etc. 
# and the Estimate column represent the change in mpg 
# i.e for cyl -0.11144 means that mpg decreases by 0.11144 as cyl increases

# H0 : There is no significant relation between dependent & independent variables
# H1 : There is a relation between dependent & independent variables

# We also have p-value for each variables and all variable has p>0.05 so we can reject null hypthesis 
# i.e. there is 

# Step 3: Get VIF and drop variables with VIF > 10
vif_mlr <- car::vif(mlr)
drop_vars <- names(vif_mlr[vif_mlr > 10])
mlr <- update(mlr, . ~ . - drop_vars)

# Step 4: Fit mlr model with predictors having VIF <= 10
summary(mlr)

# Step 5: Fit lasso regression using cv.glmnet
x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
cv_model <- cv.glmnet(x, y)

# Step 6: Get the best lambda value and plot cv_model
best_lambda <- cv_model$lambda.min
plot(cv_model)

# Step 7: Fit the best lasso regression model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

# Step 8: Get the coefficients of the best_model
coef(best_model, s = best_lambda, exact = TRUE)

# Step 9: Fit multiple linear regression with important variables from best_model
selected_vars <- coef(best_model, s = best_lambda, exact = TRUE)
selected_vars <- selected_vars[selected_vars != 0]
selected_vars <- names(selected_vars)
mlr_best <- lm(mpg ~ ., data = mtcars[, c("mpg", selected_vars)])

# Step 10: Compare statistically significant variables from Step 4 and Step 9
summary(mlr)$coefficients[summary(mlr)$coefficients[, "Pr(>|t|)"] < 0.05, ]
summary(mlr_best)$coefficients[summary(mlr_best)$coefficients[, "Pr(>|t|)"] < 0.05, ]

# Step 11: Summary for handling multicollinearity with VIF dropouts and LASSO regression
# Multicollinearity can be addressed using VIF dropouts, which involves dropping variables with VIF > 10.
# This helps to remove highly correlated predictors and reduce multicollinearity.
# LASSO regression, on the other hand, performs variable selection by shrinking some coefficients to zero,
# effectively eliminating less important predictors. It can handle multicollinearity by regularization.
# By comparing the statistically significant variables from Step 4 (mlr) and Step 9 (mlr_best),
# we can observe that both methods can identify important variables, but LASSO regression provides a more
# automated and objective approach by directly shrinking coefficients based on the regularization parameter.

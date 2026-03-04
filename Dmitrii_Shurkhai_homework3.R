#Homework 3 by Dmitrii Shurkhai

#Question1
#Question 1 (40 points)
#Using the Auto data set found in the ISLR package, perform the tasks below
#using the supervised machine learning algorithm lm() for simple and multiple
#linear regression. You do not need to split the data set into training and test for
#this exercise.

library(ISLR)
data(Auto)

head(Auto)

str(Auto)
#Perform a correlation analysis on the Auto data frame using the pairs()
#and cor() functions (be sure not to use the name variable in this
#analysis). Review the results and provide a commentary of your findings.


#We have names here and its not numeric, so lets use correlation without it

pairs(Auto[ , -9])

cor(Auto[ , -9])

# mpg and weight = -0.83 it means than heavier car than it will be less efficient by mpg,
#mpg and desplacement = -0.77 cars with larger engines generally have lower miles per gallon,
#horsepower and displacement = 0.89 vehicles with larger engine displacement tend to produce more horsepower, 
#horsepower and weight = 0.86 then heavier car then it usually more powerful,
#horsepower and cylinders = 0.84 cars with more cylinders typically generate greater horsepower

#Use the lm() function to perform a simple linear regression with mpg as
#the response variable and horsepower as the predictor. Store the results
#in a linear model object named lm1

lm1 <- lm(mpg ~ horsepower, data = Auto)

# 1. Use the summary() function on the lm1 object to print the results.

summary(lm1)

# 2. Comment on the output of summary(), for example: is there a
#relationship between the predictor and the response variable? If so,
#how strong is the relationship? Is the relationship positive or
#negative?

# horsepower and mpg have a negative relationship
# when horsepower increases, mpg decreases
# p-value is very small (<2e-16), so the relationship is significant
# R-squared ≈ 0.61, so horsepower explains about 61% of mpg variation


# 3. Create a scatterplot using the response variable and predictor. In
#addition, use the abline() function to display the ordinary least
#squares (OLS) regression line.
plot(Auto$horsepower, Auto$mpg,
     xlab="Horsepower",
     ylab="MPG",
     main="MPG vs Horsepower")

abline(lm1, col="blue", lwd=3)

## the scatterplot shows that when horsepower goes up, mpg goes down
# cars with more horsepower usually have lower fuel efficiency


#C. Use the lm() function to perform a second simple linear regression with
#mpg as the response variable and weight as the predictor. Store the
#results in a linear model object named lm2

lm2 <- lm(mpg ~ weight, data = Auto)

#1. Use the summary() function on the lm3 object to print the results.

summary(lm2)

#2. Comment on the output of summary(), for example: are there
#relationships between the predictors and the response variable? If
#so, how strong are the relationship? Are the relationships positive or
#negative?

# weight and mpg have a negative relationship
# when weight increases, mpg decreases
# p-value is very small (<2e-16), so the relationship is significant
# R-squared ≈ 0.69, so weight explains about 69% of mpg variation

#3. Create a scatterplot using the response variable and predictor. In
#addition, use the abline() function to display the ordinary least
#squares (OLS) regression line.



plot(Auto$weight, Auto$mpg,
     xlab="Weight",
     ylab="MPG",
     main="MPG vs Weight")

abline(lm2, col="blue", lwd=3)


#D. Use the lm() function to perform a multiple linear regression with mpg as
#the response variable and horsepower and weight as the predictors.
#Store the results in a linear model object named lm3.

lm3 <- lm(mpg ~ horsepower + weight, data = Auto)


#1. Use the summary() function on the lm3 object to print the results.

summary(lm3)

#2. Comment on the output of summary(), for example: are there
#relationships between the predictors and the response variable? If
#so, how strong are the relationship? Are the relationships positive or
#negative?

# horsepower and weight both have negative relationships with mpg
# when horsepower or weight increase, mpg decreases
# both p-values are very small, so both predictors are significant
# R-squared ≈ 0.71, so the model explains about 71% of mpg variation

#3. Use the plot() function on the linear model object lm3 to produce
#four diagnostic plots describing the regression fit. Comment on each
#of the plots and any problems you see with the fit.

plot(lm3)


# Residuals vs Fitted:
# points are mostly random but there is a little curve not perfectly linear

# Normal Q-Q:
# points are close to the line, but the ends move away

# Scale-Location:
# spread gets a bit bigger for larger fitted values 

# Residuals vs Leverage:
# a few points have higher leverage

#4. Using the computed coefficients of the lm3 linear model object,
#what is the predicted mpg value associated with a horsepower
#value of 98, and a weight value of 2500?
  

predict(lm3, newdata = data.frame(horsepower = 98, weight = 2500))

# mpg will be approximatly 26.51914

# Question 2. Using the Auto data set, perform the tasks below using the supervised machine
#learning algorithm glm() for logistic regression. Develop a model to predict
#whether a given car gets high or low gas mileage:
#  A. Create a binary categorical variable mpg01 that contains a 1 if mpg
#contains a value > its median, and a 0 if mpg contains a value <= its median.
#You can use the median() function in base R for this purpose. Create a
#new data frame containing all the variables from Auto plus the new
#mpg01 variable


mpg_median <- median(Auto$mpg)

mpg01 <- ifelse(Auto$mpg > mpg_median, 1, 0)

Auto2 <- data.frame(Auto, mpg01)

# mpg01 = 1 means high mpg
# mpg01 = 0 means low mpg

#B. Perform “feature engineering” to determine which of the predictors seem
#most likely to be useful in predicting mpg01. The cor() statistical
#function may be useful here to compute a correlation matrix of the
#predictors. Provide a commentary for your choice of predictors.

cor(Auto2[, -9])   # again we need to remove no numeric column

# weight, horsepower, displacement and cylinders
# show strong correlation with mpg
# these variables are likely useful, maybe not horsepower predictors for mpg01


#C. Split the data into a training set and test set. You can choose the split
#percentage (you might experiment with several percentages in order to
#minimize the test error metric).

set.seed(1)

train_index <- sample(1:nrow(Auto2), 0.8*nrow(Auto2))

train <- Auto2[train_index, ]
test <- Auto2[-train_index, ]

# 80% training data
# 20% test data


#D. Train the glm() algorithm using the training set with mpg01 as the
#response variable along with the predictors you chose above.

glm_model <- glm(mpg01 ~ weight + displacement + cylinders,
                 data=train,
                 family=binomial)

summary(glm_model)

#E. Use the predict.glm() function on the test set in order to get
#predicted probabilities of class membership

prob <- predict(glm_model, test, type="response")

#F. Based on the predicted probabilities, create a vector of 0s and 1s where the
#1s indicate the predicted probability is > 0.5. The 1s indicate where the
#predicted probability is successful (you might experiment with several
#                                     threshold values to minimize the test error metric).

pred <- ifelse(prob > 0.5, 1, 0)
pred

#G. Compare the above vector (predicted response variable values) with the
#mpg01 variable values in the test set (actual response variable values) and
#create a vector index of 0s and 1s indicating whether the two values are not
#equal.

error_vec <- ifelse(pred != test$mpg01, 1, 0)

#H. Calculate the mean() of the vector in the above step. This is your test
#error metric. Your goal is to minimize this metric.

mean(error_vec)

#0.1012658

#Question 3 (20 points)
#Use the K-means clustering algorithm kmeans() on the iris data set for the
#Sepal.Length and Sepal.Width variables. Perform the following steps:
#A. Set the number of centroids to 3

centroids <- 3

#B. Call the kmeans() algorithm and store the resulting kmeans class object
#to a variable named kc. You need to set seed to get reproducible results
#because kmeans() uses a random number generator to come up with the
#centers if you use the centers argument.

set.seed(42)
kc <- kmeans(iris[, c("Sepal.Length","Sepal.Width")], centers = centroids)

#C. Review and print the cluster component of the kmeans object.

kc$cluster

#D. Review and print the centers component of the kmeans object.

kc$centers

#E. Produce a scatterplot data visualization to plot each of the resulting
#clusters of data points and their centers. Use different colors for the data
#points residing in each cluster.

plot(iris$Sepal.Length, iris$Sepal.Width,
     col = kc$cluster, pch = 19,
     xlab = "Sepal Length", ylab = "Sepal Width",
     main = "K-means Clusters (k=3)")

#Also, plot a special character (e.g. “+”)
#showing the centroid of each cluster.

points(kc$centers[,1], kc$centers[,2],
       pch = "+", col = "blue", cex = 3, lwd = 3)

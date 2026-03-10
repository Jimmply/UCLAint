############################################################
# Final project by Dmitrii Shurkhai
# UCLA Extension - COM SCI X 450.1
# Class Project: Supervised ML - Regression (Random Forest)
# Dataset: California Housing (housing.csv)
##########################################################


###########################
# 1. Access the Data Set
###########################

getwd()

setwd("/Users/jimmy/Documents/UCLAintr/final_project")  # <- update if your file is elsewhere

data_path <- "data/housing.csv"  # <- update if your file is elsewhere
housing <- read.csv(data_path, stringsAsFactors = FALSE)

# Cast ocean_proximity to factor and display levels
housing$ocean_proximity <- as.factor(housing$ocean_proximity)
levels(housing$ocean_proximity)

########################
# 2. EDA and Data Visualization
########################

# a) head() and tail()
head(housing)
tail(housing)

# b) summary()
summary(housing)

# c) Correlation analysis on numeric variables
num_cols <- sapply(housing, is.numeric)
housing_numeric <- housing[, num_cols]
cor_matrix <- cor(housing_numeric, use = "complete.obs")
cor_matrix

# d) Histograms for each numeric variable
op <- par(no.readonly = TRUE)
par(mfrow = c(3, 3))  # layout (adjust if you want)
for (nm in names(housing_numeric)) {
  hist(
    x = housing_numeric[[nm]],
    main = paste("Histogram:", nm),
    xlab = nm
  )
}
par(op)

# e) Boxplots for each numeric variable
op <- par(no.readonly = TRUE)
par(mfrow = c(3, 3))
for (nm in names(housing_numeric)) {
  boxplot(
    x = housing_numeric[[nm]],
    main = paste("Boxplot:", nm),
    ylab = nm
  )
}
par(op)

# f) Boxplots with respect to ocean_proximity:
#    housing_median_age, median_income, median_house_value vs ocean_proximity
boxplot(
  housing_median_age ~ ocean_proximity,
  data = housing,
  main = "Housing Median Age by Ocean Proximity",
  xlab = "Ocean Proximity",
  ylab = "Housing Median Age (years)",
  las = 2
)

boxplot(
  median_income ~ ocean_proximity,
  data = housing,
  main = "Median Income by Ocean Proximity",
  xlab = "Ocean Proximity",
  ylab = "Median Income (10k USD units)",
  las = 2
)

boxplot(
  median_house_value ~ ocean_proximity,
  data = housing,
  main = "Median House Value by Ocean Proximity",
  xlab = "Ocean Proximity",
  ylab = "Median House Value (USD)",
  las = 2
)

########################
# 3. Data Transformation
########################

cleaned_housing <- housing

# a) Impute missing total_bedrooms with statistical median
bed_median <- median(cleaned_housing$total_bedrooms, na.rm = TRUE)
cleaned_housing$total_bedrooms[is.na(cleaned_housing$total_bedrooms)] <- bed_median

# b) One-hot encode ocean_proximity into binary variables (1/0), then remove factor
# Required levels in assignment:
# "NEAR BAY" "<1H OCEAN" "INLAND" "NEAR OCEAN" "ISLAND"
levels_needed <- c("NEAR BAY", "<1H OCEAN", "INLAND", "NEAR OCEAN", "ISLAND")

# Ensure all required levels exist; if a level is missing, create its dummy as 0s
for (lv in levels_needed) {
  cleaned_housing[[lv]] <- ifelse(as.character(cleaned_housing$ocean_proximity) == lv, 1, 0)
}

# Remove original factor column
cleaned_housing$ocean_proximity <- NULL

# c) Create mean_bedrooms and mean_rooms using households, then remove total_* columns
# Guard against division by zero (shouldn't happen in this dataset, but safe)
cleaned_housing$mean_bedrooms <- cleaned_housing$total_bedrooms / pmax(cleaned_housing$households, 1)
cleaned_housing$mean_rooms    <- cleaned_housing$total_rooms    / pmax(cleaned_housing$households, 1)

cleaned_housing$total_bedrooms <- NULL
cleaned_housing$total_rooms    <- NULL

# d) Feature scaling: scale numeric vars except response and binary categorical vars
# Response variable:
response <- "median_house_value"

# Binary dummy columns:
dummy_cols <- levels_needed

# Identify numeric columns to scale:
# - numeric
# - not response
# - not dummy columns
num_cols2 <- names(cleaned_housing)[sapply(cleaned_housing, is.numeric)]
scale_cols <- setdiff(num_cols2, c(response, dummy_cols))

# Scale (center/scale)
cleaned_housing[scale_cols] <- lapply(cleaned_housing[scale_cols], function(x) as.numeric(scale(x)))

# e) Ensure the final variable set matches the required output list/order
# "NEAR BAY" "<1H OCEAN" "INLAND" "NEAR OCEAN" "ISLAND" "longitude"
# "latitude" "housing_median_age" "population" "households" "median_income"
# "mean_bedrooms" "mean_rooms" "median_house_value"

final_order <- c(
  "NEAR BAY", "<1H OCEAN", "INLAND", "NEAR OCEAN", "ISLAND",
  "longitude", "latitude", "housing_median_age", "population", "households",
  "median_income", "mean_bedrooms", "mean_rooms", "median_house_value"
)

# Subset/reorder (use backticks for non-syntactic names)
cleaned_housing <- cleaned_housing[, final_order]

# Quick sanity checks
str(cleaned_housing)
summary(cleaned_housing)

########################
# 4. Create Training and Test Sets
########################
n <- nrow(cleaned_housing)
set.seed(42) #42!
train_idx <- sample(seq_len(n), size = floor(0.7 * n), replace = FALSE)

train <- cleaned_housing[train_idx, ]
test  <- cleaned_housing[-train_idx, ]

########################
# 5. Random Forest - Regression
########################

# Split train into predictors and response
train_x <- train[, setdiff(names(train), response)]
train_y <- train[[response]]  # numeric vector

# Fit model
rf <- randomForest(
  x = train_x,
  y = train_y,
  ntree = 500,
  importance = TRUE
)

# Inspect available model outputs/metrics
names(rf)

########################
# 6. Evaluating Model Performance
########################

# a) Training RMSE using last element of rf$mse
train_rmse <- sqrt(tail(rf$mse, 1))
train_rmse

# b) Predict on test set
test_x <- test[, setdiff(names(test), response)]
test_y <- test[[response]]

pred_y <- predict(object = rf, newdata = test_x)

# c) Test RMSE (UDF)
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}
test_rmse <- rmse(actual = test_y, predicted = pred_y)
test_rmse

# d) Compare train vs test RMSE (printed)
cat("Training RMSE:", train_rmse, "\n")
cat("Test RMSE:    ", test_rmse, "\n")

# e) Variable importance plot
varImpPlot(x = rf, main = "Random Forest Variable Importance")

#############################################################
# Final
#############################################################

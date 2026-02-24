#Homework2 by Dmitrii Shurkhai

#Question1
#Using the sqldf() function found in the sqldf package to select data from
#the CO2 data set, execute the SQL statement required to calculate the average
#value for uptake grouped by Type. Please use only SQL for the solution to this
#question.

library(sqldf)

sqldf("
  SELECT Type,
         AVG(uptake) AS avg_uptake
  FROM CO2
  GROUP BY Type
")

#Question2
# Use the following vector assignment statements to provide data content for a
#new data frame:
#  Died.At <- c(22,40,72,41)
#Writer.At <- c(16, 18, 36, 36)
#First.Name <- c("John", "Edgar", "Walt", "Jane")
#Second.Name <- c("Doe", "Poe", "Whitman", "Austen")
#Sex <- c("MALE", "MALE", "MALE", "FEMALE")
#Date.Of.Death <- c("2015-05-10", "1849-10-07", "1892-
#03-26","1817-07-18")
Died.At <- c(22, 40, 72, 41)
Writer.At <- c(16, 18, 36, 36)
First.Name <- c("John", "Edgar", "Walt", "Jane")
Second.Name <- c("Doe", "Poe", "Whitman", "Austen")
Sex <- c("MALE", "MALE", "MALE", "FEMALE")
Date.Of.Death <- c("2015-05-10", "1849-10-07", "1892-03-26", "1817-07-18")

#Write some data munging code to performing the following operations:
#  • Create a new data frame df with the above data for each of six columns.
df <- data.frame(
  Died.At = Died.At,
  Writer.At = Writer.At,
  First.Name = First.Name,
  Second.Name = Second.Name,
  Sex = Sex,
  Date.Of.Death = Date.Of.Death,
  stringsAsFactors = FALSE
)
#• Use the appropriate as.() function to cast the Sex variable to a factor.
df$Sex <- as.factor(df$Sex)
#• The variable names are inconvenient so write R code to change them to:
#  age_at_death, age_as_writer, first_name, surname,
#gender, date_died [Hint: remember the names() function for data
#                   frames.]
names(df) <- c("age_at_death", "age_as_writer", "first_name", "surname", "gender", "date_died")
#• Say “John Doe” died on his birthday, calculate and display his birthdate
#value (only John’s) based on the variables date_died and
#age_at_death

john_doe <- df$first_name == "John" & df$surname == "Doe"
df$date_died <- as.Date(df$date_died)
library(lubridate)
john_birthdate_exact <- df$date_died[john_doe] %m-% years(df$age_at_death[john_doe])
john_birthdate_exact

#Question3
#When recording experimental observations, there are two general formats found
#in data sets – “long” and “wide.” The long format for recording observations is
#when there is one observation row per variable. 

product <- c("A", "B")
height  <- c(10, 20)
width   <- c(5, 10)
weight  <- c(2, NA)

observations_wide <- data.frame(product, height, width, weight)

#Write a data transformation R script to take the observations_wide data
#frame above and convert it to long format. Here is what the output should look
#like below (make sure you order the rows to match the results shown).

library(reshape2)

observations_long <- melt(
  observations_wide,
  id.vars = "product",
  variable.name = "variable",
  value.name = "value",
  na.rm = TRUE
)

observations_long

#not the right order

observations_long <- observations_long[
  order(observations_long$product, observations_long$variable),
]

observations_long 

# product variable value
#A height 10
#A width 5
#A weight 2
#B height 20
#B width 10

#Question4

#Using the mtcars data set, write an R script that calculates the average miles per
#gallon (mpg variable) by number of cylinders in the car (cyl variable). The
#output should be the following:
#  4 6 8
#26.66364 19.74286 15.10000


mpg_split <- split(mtcars$mpg, mtcars$cyl)

avg_mpg <- sapply(mpg_split, mean)

avg_mpg

#Question5

#Using the mtcars data set, write an R script to calculate the absolute difference
#between the average horsepower of 4-cylinder cars and the average horsepower
#of 8-cylinder cars. [Hint: you may wish to use the base R abs() function for
#                     calculating the absolute value of a number.]

mean_hp <- tapply(mtcars$hp, mtcars$cyl, mean)

# Compute the absolute difference between 4-cyl and 8-cyl
abs_diff <- abs(mean_hp["4"] - mean_hp["8"])

abs_diff
#4
#126.5779

#Question6
#Using the airquality data set, provide the R code that calculates mean value
#of the Temp variable when the Month variable is equal to 6?

mean(airquality$Temp[airquality$Month == 6])
#79.1

#Question7
#Write the dplyr code required to calculate the mean mpg for each
#transmission type (0 = automatic, 1 = manual) using the am variable in the
#mtcars data set. Sort the resulting list by mean mpg. Use a single dplyr

library(dplyr)

mtcars %>%
  group_by(am) %>%
  summarise(mean_mpg = mean(mpg)) %>%
  arrange(mean_mpg)
#statement with multiple pipes in the solution. The resulting output should be:
  # A tibble: 2 x 2
  # am mean_mpg
  # <dbl> <dbl>
  #1 0 17.1
  #2 1 24.4

#Question8
#In this question you’ll need to use the scatterplot3d package in order to
#render a 3D visualization for the mtcars data set. You’ll need to use the
#scatterplot3d() function in this package. Please provide the R code that
#produces a data visualization with the following requirements:
#  • Use the variables for the X, Y and Z axis respectively: wt, disp, mpg
#• Include an appropriate title for the plot
#• Include labels for each axis and include the units for the variables
#• Add a fourth data point to the plot, the am variable (transmission type), by
#using the pch argument.
#• Please specify each argument name when calling the function.

library(scatterplot3d)

scatterplot3d(
  x = mtcars$wt,
  y = mtcars$disp,
  z = mtcars$mpg,
  main = "3D Scatterplot of MPG vs Weight and Displacement",
  xlab = "Weight",
  ylab = "Displacement",
  zlab = "Miles per Gallon",
  pch = ifelse(mtcars$am == 0, 16, 17)
)

#Question9
#Using the CO2 data set, produce a histogram for the uptake variable for a
#subset of the data set where the Type variable is equal to “Quebec.” The
#visualization must follow the specifications:
#  • Use 20 cells (aka buckets, breaks) for the plot
#• Use the color name "cornflowerblue" for the plot
#• Add a title “Quebec”
#• Add an X-axis label "CO2 Uptake Rate"
#• Add a Y-axis label “Frequency”
#• Store the histogram in an object variable
#• Extract and display the metadata for break points (cell boundaries)
#• Extract and display the metadata for the counts for each cell

uptake_qc <- CO2$uptake[CO2$Type == "Quebec"]

hist_qc <- hist(
  x = uptake_qc,
  breaks = 20,
  col = "cornflowerblue",
  main = "Quebec",
  xlab = "CO2 Uptake Rate",
  ylab = "Frequency",
  plot = TRUE
)


# Extract and display the metadata for break points (cell boundaries)
hist_qc$breaks

#Extract and display the metadata for the counts for each cell
hist_qc$counts

#Question10
#Provide the R code necessary to reproduce the boxplot data visualization below
#using the mtcars data set. Make sure the width of each box represents the
#number of observations in the group.


boxplot(
  mpg ~ gear,
  data = mtcars,
  varwidth = TRUE,
  main = "Car milage data",
  xlab = "Number of Forward Gears",
  ylab = "Miles per Gallon",
  col = "lightgray"
)


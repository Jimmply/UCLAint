#Homework by Dmitrii Shurkhai

#Question1
#Define a new matrix named mat with 3 row and 4 columns, filling with a

#sequence of values 1:12 by row
mat <- matrix(1:12, nrow=3, ncol=4)

#Add a new row to the matrix containing all 8’s
mat <- cbind(rep(9, nrow(mat)), mat)

#Add a new column to the matrix containing all 9’s
mat <- rbind(c(9, rep(8, ncol(mat) - 1)), mat)

#Print our matrix
#.   [,1] [,2] [,3] [,4] [,5]
#[1,] 9   8     8     8   8
#[2,] 9   1     2     3   4
#[3,] 9   5     6     7   8
#[4,] 9   9    10     11  12
mat

#Question2

# (a) Character vector of student names
a <- c("Ellen", "Catherine", "Stephen")

# (b) Integer vector of homework scores
b <- c(91L, 94L, 100L)

# (c) Attendance matrix (rows = sessions, columns = students)
att <- matrix(
  c(TRUE, TRUE, FALSE,
    TRUE, FALSE, TRUE),
  nrow = 2,
  byrow = TRUE
)
# Define the list object 'lst'
lst <- list(
  names = a,
  homework = b,
  attendance = att
)
lst

# Display the names of all students
lst$names

# Display Stephen’s homework score
lst$homework[3]

# Display Catherine’s attendance for both sessions
lst$attendance[, 2]

#Question 3

# Create the character vector 'gender'
gender <- c(
  rep("male", 25),
  rep("female", 30)
)

#Use a factor variable to quickly and efficiently calculate a total 
#number of values in each gender category. 
table(factor(gender))


#Question 4

#Using the airquality data set, determine how many missing values are in the
#Ozone variable of this data frame? 
data = data(airquality)

sum(is.na(airquality$Ozone))

#Question 5

#Extract the subset of rows of the data frame where Ozone values are above 31
#and Temp values are above 90. What is the mean of Solar.R in this subset?

# Subset rows where Ozone > 31 AND Temp > 90
aq_sub <- airquality[airquality$Ozone > 31 & airquality$Temp > 90, ]

# Mean of Solar.R in that subset (remove NA)
mean_solar <- mean(aq_sub$Solar.R, na.rm = TRUE)
mean_solar

#mean is 212.8

#Question 6

#Make a copy of the airquality data frame so you can add a new variable
#hotcold. The new variable shall have two possible character values: “hot” if
#the value of the Temp variable is greater than the median value for Temp, and
#“cold” otherwise. Include the first 12 and last 12 rows of the data frame using
#head() and tail() to show your code is working.
aq2 <- airquality  # copy

temp_med <- median(aq2$Temp, na.rm = TRUE)

aq2$hotcold <- ifelse(aq2$Temp > temp_med, "hot", "cold")

head(aq2, 5)
tail(aq2, 5)

#Question 7

#Write an R script to solve the following simple task: take the integers from 1 to
#100, square them, and then add up all the even squares while subtracting all the
#odd squares. The final answer will be a single integer value.

nums <- 1:100
sq <- nums^2

result <- sum(sq[sq %% 2 == 0]) - sum(sq[sq %% 2 == 1])
result

#Question8

#Using the following matrix definition: mat1 <- matrix(rep(seq(4), 4),
#ncol = 4) use one of R’s loop functions to compute the sum of the integers in
#each row plus 2. You must use a user defined function (UDF) or an anonymous
#function for the solution. The output should be a numeric vector of length 4.

mat1 <- matrix(rep(seq(4), 4), ncol = 4)

# apply() is a loop function; using an anonymous function
out_vec <- apply(mat1, 1, function(r) sum(r) + 2)

out_vec  
# numeric vector length 4

#Question9

### This script will demonstrate basic familiarity with R
###
### Your Name: Quinn Quillan Torres
### Date: April 13, 2021


# Load the R library, ggplot2, and no others (i.e., you should use base R for most
# of this exercise)
library('ggplot2')

# Create a dataframe with NAs in every cell, 100 rows, and columns with the
# following names: bblid,sex,age,cognition.
df <- data.frame (bblid  = (rep(NA,100)),
                  sex = (rep(NA,100)),
                  age =  (rep(NA,100)),
                  cognition =  (rep(NA,100))
)
# Check by displaying the first 20 rows of df
head(df,20)





# Sample 100 random integers, without replacement, between 10,000 and 20,000
# to serve as bblids (our lab's subject identifier). Put these integers into the
# bblid column.

# This samples 100 random integers between 10,000 and 20,000, replace = FALSE so no replacement
df$bblid <- sample(10000:20000,100, replace = FALSE)

# Check by displaying first few rows
head(df)



# Simulate 100 ages and 100 cognition scores from a bivariate normal distribution
# with rho=.7, and put these values into their respective columns in the dataframe.

# Assigning values ( N, mean1, mean2, sigma1, sigma2, rho)

# Number of samples
N<- 100

# Correlation coefficient ( Strength of relationship between two variables)
rho<- .7 

# Our two means 
mu1<- 1 ; mu2<- 1

# Our two standard deviations
s1<- 2; s2<- 2

# Generating our age and cognition values using bivariate normal 
# Generating our age values
df$age<- rnorm(N, mu1, s1)
# Generating our cognition values
df$cognition<- rnorm(N, mu2 + (s2/s1) * rho * (df$age - mu1), sqrt((1 - rho^2)*s2^2))





# Scale the age values so mean=15 and variance=6.
# Multiply each record in the age column by 1.225 
df$age<- df$age * 1.225
# Add each record in the age column by 14
df$age<- df$age + 14







# Check if any age values are below zero. How many are there?
# df$age < 0 will label each record in the age col as T/F, sum the col where values that are true are counted as 1's
num_ages_belZero<- sum(df$age < 0)
# Print the output
sprintf("There are this many ages below zero: %s", num_ages_belZero) 





# For any age values that are below zero, replace that age with zero. Write this
# code whether or not there are ages below zero.
df$age[df$age <0] <- 0





# Create two ggplots: a histogram of ages, and a histogram of cognition. Use
# non-default themes and colors (explore!).

# This plot will make a histogram of ages on the x-axis and frequency on the y axis
histAge<- ggplot(df, aes(x=age)) + geom_histogram(binwidth = 2 * IQR(df$cognition) / length(df$age)^(1/3), color = 'black', fill="darkslategray3", position="identity") + ggtitle('Histogram of Age') +
  theme( panel.background = element_rect(fill = "lightblue", colour = "lightblue", size = 0.5, linetype = "solid"))
histAge

# This plot will make a histogram of cognition values on the x axes and frequency on the y axis 
histCog<- ggplot(df, aes(x=cognition)) + geom_histogram(binwidth = 2 * IQR(df$cognition) / length(df$cognition)^(1/3), color = 'black', fill="firebrick4", position="identity") + ggtitle('Histogram of Cognition')
histCog





# Calculate the correlation between age and cognition. Does it look familiar?
cor(df$age, df$cognition)
# We get a value approximately equal to .7 which is the same value of rho
# that was given. 





# Now assign the first 50 rows in your dataframe to be female, and the latter
# 50 to be male. Make sure your sex variable is coded as a factor.

# COnvert the first 50 values in sex column to female 
df$sex[1:50] <- 'female'
# Convert the second 50 values in sex column to male
df$sex[51:100]<- 'male'
# Convert data type of sex columnn to factor 
df$sex<- as.factor(df$sex)


# Create a scatterplot with age on the x-axis, and cognition on the y-axis.
# Color your points by sex, and plot a regression line per sex.
scatterAge_Cog<- ggplot(df, aes(x = age, y = cognition, color = sex, shape = sex)) + geom_point(size = 2) + geom_smooth(method = lm, se = FALSE, fullrange = TRUE) + ggtitle("Scatter Plot of Cognition and Age")
scatterAge_Cog



# Export all of your plots to one pdf, with each plot on a different page.
pdf('Quinn RExercise Plots.pdf')

histAge
histCog
scatterAge_Cog


dev.off()

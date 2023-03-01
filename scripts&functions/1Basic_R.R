
# Assigning a value to a variable
x <- 5

# Printing the value of a variable
x

# Performing arithmetic operations
x + 2
x * 3
x / 2

# Creating a character variable
y <- "Hello, world!"

# Concatenating strings
paste("The value of x is", x)

# Creating a logical variable
z <- TRUE

# Using built-in functions
sqrt(x)
sum(1, 2, 3)

# Creating a vector
v <- c(1, 2, 3, 4, 5)

# Subsetting a vector
v[1]
v[3:5]
v[v > 3]

# Creating a matrix
m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
m
# Subsetting a matrix
m[1, 2]
m[2, ]
m[, 3]

# Creating a data frame
df <- data.frame(name = c("Alice", "Bob", "Charlie"), age = c(25, 30, 35), gender = c("F", "M", "M"))

# Subsetting a data frame
df$name
df[2, ]
df[df$age > 30, ]

# Load Packages
library(tidyverse)
library(signal)
library(rstatix)
library(ggplot2)
# Load the iris dataset
data(iris)
view(iris)
# Determine the data types of the variables in the dataset
str(iris)

# Convert the Species column to a factor
iris$Species <- as.factor(iris$Species)

# Create a barplot of the number of each species
barplot(table(iris$Species), xlab = "Species", ylab = "Count", main = "Number of Each Species")

# Create a barplot of the number of each species using ggplot
ggplot(iris, aes(x = Species)) +
  geom_bar(aes(color = Species, fill = factor(Species)) )+
  ggtitle("Number of Each Species")

# Create a scatterplot of Sepal.Length vs. Sepal.Width using ggplot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
  geom_point() +
  ggtitle("Sepal Length vs. Sepal Width")

# Create a new column that categorizes Sepal.Length as short, medium, or long
iris$sepal_length_cat <- cut(iris$Sepal.Length, breaks = c(0, 5, 6, Inf), labels = c("short", "medium", "long"))

# Create a scatterplot of Sepal.Length vs. Sepal.Width, color-coded by sepal_length_cat
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = sepal_length_cat)) +
  geom_point() +
  labs(title = "Scatterplot of Sepal.Length vs. Sepal.Width, color-coded by sepal_length_cat")



# Load the mtcars dataset
data(mtcars)

# Create a subset of cars with automatic transmissions
auto_cars <- subset(mtcars, am == 0)

# Create a scatterplot of weight and miles per gallon for automatic cars
plot(auto_cars$wt, auto_cars$mpg, xlab = "Weight (in 1000 pounds)", ylab = "Miles per Gallon", main = "Weight vs. MPG (Automatic Cars)")

# Create a scatterplot of weight and miles per gallon for all cars using ggplot
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  ggtitle("Weight vs. MPG (All Cars)")
# Load the iris dataset and ggplot2 package



# Load the airquality dataset
data(airquality)

head(airquality)

# Calculate summary statistics for multiple columns
summary(airquality[c("Ozone", "Solar.R", "Wind", "Temp", "Month")])

# Convert the Month column to a date
airquality$Date <- as.Date(paste( "2022",airquality$Month,airquality$Day, sep = "-"), format = "%Y-%m-%d")

# Create a line plot of Ozone levels over time
with(airquality, plot(Date, Ozone, xlab = "Date", ylab = "Ozone Level", main = "Ozone Levels Over Time"))

#Create a line plot of Ozone levels over time using ggplot
ggplot(airquality, aes(x = Date, y = Ozone, color = Month)) +
  geom_point() +
  labs(x = "Date", y = "Ozone Level", title = "Ozone Levels Over Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# Save the plot as a PNG file
ggsave("ozone_levels.png")

# Create a ggplot object
ggplot(airquality, aes(x = Date, y = Ozone, color = factor(Month))) +
  # Add a scatterplot layer with different colors for different months
  geom_point() +
  # Add a line layer
  geom_line() +
  # Add labels and title
  labs(x = "Date", y = "Ozone Level", title = "Ozone Levels Over Time by Month") +
  # Change the color palette
  scale_color_hue(h = c(0, 360))+
  # Save the plot as a PNG file
  ggsave("graph/ozone_levels.png", width = 6, height = 4, dpi = 300)


### Another way 

# Convert the Month column to a factor and set the levels in order
airquality$Month <- factor(airquality$Month, levels = c("5", "6", "7", "8", "9"))

# Set the colors for each month
colors <- c("red", "orange", "yellow", "green", "blue")

# Create a ggplot object
ggplot(airquality, aes(x = Date, y = Ozone, color = Month)) +
  # Add a scatterplot layer with different colors for different months
  geom_point() +
  # Add labels and title
  labs(x = "Date", y = "Ozone Level", title = "Ozone Levels Over Time by Month") +
  # Set the colors for each month
  scale_color_manual(values = colors) 
  # Save the plot as a PNG file
  ggsave("ozone_levels.png", width = 6, height = 4, dpi = 300)

  
  
  
source('summarySE.R')
str(mtcars)
iris_mean_se_sd <- summarySE(iris, measurevar="Sepal.Length", groupvars=c("Species"))

sum(is.na(mtcars))
airquality <- na.omit(airquality)

library(tidyverse)
mtcars <- mtcars %>% ungroup() %>% 
  mutate(cyl = factor(cyl),
         hp  = factor(hp),
         mpg = factor(mpg),
         am  = factor(am))

air_mean_se_sd <- summarySE(mtcars, measurevar="mpg", groupvars=c("cyl","hp","am"))




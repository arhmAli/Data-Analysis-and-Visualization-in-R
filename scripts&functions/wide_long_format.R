# Load required packages
library(tidyr)

# 1. gather() function
# Convert wide-format data to long-format
# Example using built-in airquality dataset
head(airquality)
airquality_long <- gather(airquality, key = "variable", value = "value", -c(Ozone, Solar.R, Month, Day))
head(airquality_long)

# 2. spread() function
# Convert long-format data to wide-format
# Example using built-in PlantGrowth dataset
head(PlantGrowth)
PlantGrowth_wide <- spread(PlantGrowth, key = "group", value = "weight")
head(PlantGrowth_wide)

# 3. separate() function
# Split a column into multiple columns based on a separator
# Example using built-in mtcars dataset
head(mtcars)
mtcars_sep <- separate(mtcars, col = "carb", into = c("a", "b"), sep = 1)
head(mtcars_sep)

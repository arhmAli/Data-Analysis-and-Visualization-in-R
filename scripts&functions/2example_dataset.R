# Create the example dataset
example_data <- data.frame(
  numeric_col = c(1, 2, 3),
  integer_col = c(4L, 5L, 6L),
  logical_col = c(TRUE, FALSE, TRUE),
  character_col = c("hello", "world", "R"),
  factor_col = factor(c("A", "B", "A")),
  date_col = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")),
  datetime_col = as.POSIXct(c("2022-01-01 12:00:00", "2022-01-02 12:00:00", "2022-01-03 12:00:00")),
  complex_col = complex(real = c(1, 2, 3), imaginary = c(4, 5, 6)),
  raw_col = as.raw(c(0x01, 0x02, 0x03)),
  list_col = list(c(1, 2, 3), c("a", "b", "c"), c(TRUE, FALSE, TRUE)),
  matrix_col = matrix(1:6, nrow = 3),
  data_frame_col = data.frame(a = 1:3, b = c("x", "y", "z"))
)

# Print the dataset
example_data

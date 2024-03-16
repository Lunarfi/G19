# Load necessary libraries
library(readr)

# Function to read CSV files from a directory and categorize them into different data frames
read_and_categorize_csv <- function(directory) {
  # Get list of CSV files in the directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize a list to store data frames
  data_frames <- list(
    Category = NULL,
    Customer = NULL,
    Orders = NULL,
    Payment = NULL,
    Product = NULL,
    Promotion = NULL,
    Sales = NULL,
    Settlement = NULL,
    Supplier = NULL
  )
  
  # Loop through each CSV file
  for (csv_file in csv_files) {
    # Read CSV file into a data frame
    data <- read.csv(csv_file)
    
    # Extract file name without extension
    file_name <- tools::file_path_sans_ext(basename(csv_file))
    
    # Determine which data frame to store the data
    if (grepl("Customer", file_name, ignore.case = TRUE)) {
      data_frames$Customer <- rbind(data_frames$Customer, data)
      # Output variable names
      cat("Variables in Customer data frame:\n")
      print(names(data_frames$Customer))
    } else if (grepl("Category", file_name, ignore.case = TRUE)) {
      data_frames$Category <- rbind(data_frames$Category, data)
      # Output variable names
      cat("Variables in Category data frame:\n")
      print(names(data_frames$Category))
    } else if (grepl("Order", file_name, ignore.case = TRUE)) {
      data_frames$Orders <- rbind(data_frames$Orders, data)
      # Output variable names
      cat("Variables in Orders data frame:\n")
      print(names(data_frames$Orders))
    } else if (grepl("Payment", file_name, ignore.case = TRUE)) {
      data_frames$Payment <- rbind(data_frames$Payment, data)
      # Output variable names
      cat("Variables in Payment data frame:\n")
      print(names(data_frames$Payment))
    } else if (grepl("Product", file_name, ignore.case = TRUE)) {
      data_frames$Product <- rbind(data_frames$Product, data)
      # Output variable names
      cat("Variables in Product data frame:\n")
      print(names(data_frames$Product))
    } else if (grepl("Promotion", file_name, ignore.case = TRUE)) {
      data_frames$Promotion <- rbind(data_frames$Promotion, data)
      # Output variable names
      cat("Variables in Promotion data frame:\n")
      print(names(data_frames$Promotion))
    } else if (grepl("Sale", file_name, ignore.case = TRUE)) {
      data_frames$Sales <- rbind(data_frames$Sales, data)
      # Output variable names
      cat("Variables in Sales data frame:\n")
      print(names(data_frames$Sales))
    } else if (grepl("Settlement", file_name, ignore.case = TRUE)) {
      data_frames$Settlement <- rbind(data_frames$Settlement, data)
      # Output variable names
      cat("Variables in Settlement data frame:\n")
      print(names(data_frames$Settlement))
    } else if (grepl("Supplier", file_name, ignore.case = TRUE)) {
      data_frames$Supplier <- rbind(data_frames$Supplier, data)
      # Output variable names
      cat("Variables in Supplier data frame:\n")
      print(names(data_frames$Supplier))
    }
  }
  
  # Return the list of data frames
  return(data_frames)
}

# Directory containing CSV files
directory <- "Data_upload"

# Read CSV files from the directory and categorize them into data frames
data_frames <- read_and_categorize_csv(directory)

# Access each data frame by its name
Category <- data_frames$Category
Customer <- data_frames$Customer
Orders <- data_frames$Orders
Payment <- data_frames$Payment
Product <- data_frames$Product
Promotion <- data_frames$Promotion
Sales <- data_frames$Sales
Settlement <- data_frames$Settlement
Supplier <- data_frames$Supplier

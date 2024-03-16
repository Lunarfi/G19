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


library(RSQLite)
library(readr)

my_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"mydatabase.db")


library(lubridate)

valid_cat = 1
valid_cus = 1
valid_ord = 1
valid_pay = 1
valid_prd = 1
valid_prm = 1
valid_sal = 1
valid_set = 1
valid_sup = 1

#CATEGORY
for (i in 1:nrow(Category))
  #check primary key exists
  if (is.na(Category$category_id[i])) { 
    print(paste("Table: Category - Error: category_id is NULL on row",i))
    valid_cat = 0
  }

#check character length
if (nchar(Category$category_id[i]) > 6) { 
  print(paste("Table: Category - Error: category_id more than 6 characters on row",i))
  valid_cat = 0
} 

#check primary key is unique
if(length(unique(Category$category_id)) != nrow(Category)) {
  print(paste("Table: Category - Error: duplicated category_id"))
  valid_cat = 0
}

if (valid_cat == 1) {
  print("Table: Category - Status: OK")
} else {print("Table: Category - Status: ERROR")}




#CUSTOMER
for (i in 1:nrow(Customer))
{
  #check primary key exists
  if (is.na(Customer$customer_id[i])) { 
    print(paste("Table: Customer - Error: customer_id is NULL on row",i))
    valid_cus = 0
  }
  
  #check character length
  if (nchar(Customer$customer_id[i]) > 6) { 
    print(paste("Table: Customer - Error: customer_id more than 6 characters on row",i))
    valid_cus = 0
  }
  
  #characters limit
  Customer$customer_first_name[i] <- substring(Customer$customer_first_name[i],1,50)
  Customer$customer_last_name[i] <- substring(Customer$customer_last_name[i],1,50)
  Customer$customer_email[i] <- substring(Customer$customer_email[i],1,200)
}

#check primary key is unique
if(length(unique(Customer$customer_id)) != nrow(Customer)) {
  print(paste("Table: Customer - Error: duplicated customer_id"))
  valid_cus = 0
}

#check email is unique
if(length(unique(Customer$customer_email)) != nrow(Customer)) {
  print(paste("Table: Customer - Error: duplicated customer_email"))
  valid_cus = 0
}

#fix data type
Customer$customer_phone <- as.numeric(Customer$customer_phone)
Customer$registration_date <- mdy(Customer$registration_date)


#final check result
if (valid_cus == 1) {
  print("Table: Customer - Status: OK")
} else {print("Table: Customer - Status: ERROR")}

#ORDER
for (i in 1:nrow(Orders))
{
  #check primary key exists
  if (is.na(Orders$order_id[i])) { 
    print(paste("Table: Orders - Error: order_id is NULL on row",i))
    valid_ord = 0
  }
  
  #check foreign key exists
  if (is.na(Orders$product_id[i])) { 
    print(paste("Table: Orders - Error: product_id is NULL on row",i))
    valid_ord = 0
  }
  
  if (is.na(Orders$customer_id[i])) { 
    print(paste("Table: Orders - Error: customer_id is NULL on row",i))
    valid_ord = 0
  }
  
  #check character length
  if (nchar(Orders$order_id[i]) > 6) { 
    print(paste("Table: Orders - Error: order_id more than 6 characters on row",i))
    valid_ord = 0
  }
  
  if (nchar(Orders$product_id[i]) > 6) { 
    print(paste("Table: Orders - Error: product_id more than 6 characters on row",i))
    valid_ord = 0
  }
  
  if (nchar(Orders$customer_id[i]) > 6) { 
    print(paste("Table: Orders - Error: customer_id more than 6 characters on row",i))
    valid_ord = 0
  }
}

#check primary key is unique
if(length(unique(Orders$order_id)) != nrow(Orders)) {
  print(paste("Table: Orders - Error: duplicated order_id"))
  valid_ord = 0
}

#fix data type
Orders$order_quantity <- as.numeric(Orders$order_quantity)
Orders$rating_score <- as.numeric(Orders$rating_score)
Orders$order_date <- mdy(Orders$order_date)
Orders$order_approval_date <- mdy(Orders$order_approval_date)
Orders$order_delivery_date <- mdy(Orders$order_delivery_date)
Orders$rating_date <- mdy(Orders$rating_date)


#final check result
if (valid_ord == 1) {
  print("Table: Orders - Status: OK")
} else {print("Table: Orders - Status: ERROR")}


#PAYMENT
for (i in 1:nrow(Payment))
{
  #check primary key exists
  if (is.na(Payment$payment_id[i])) { 
    print(paste("Table: Payment - Error: payment_id is NULL on row",i))
    valid_pay = 0
  }
  
  #check foreign key exists
  if (is.na(Payment$order_id[i])) { 
    print(paste("Table: Payment - Error: order_id is NULL on row",i))
    valid_pay = 0
  }
  
  #check character length
  if (nchar(Payment$payment_id[i]) > 6) { 
    print(paste("Table: Payment - Error: payment_id more than 6 characters on row",i))
    valid_pay = 0
  }
  
  if (nchar(Payment$order_id[i]) > 6) { 
    print(paste("Table: Payment - Error: order_id more than 6 characters on row",i))
    valid_pay = 0
  }
}

#check primary key is unique
if(length(unique(Payment$payment_id)) != nrow(Payment)) {
  print(paste("Table: Payment - Error: duplicated payment_id"))
  valid_pay = 0
}

#fix data type
Payment$Payment_date <- mdy(Payment$Payment_date)

#final check result
if (valid_pay == 1) {
  print("Table: Payment - Status: OK")
} else {print("Table: Payment - Status: ERROR")}


#PRODUCT
for (i in 1:nrow(Product))
{
  #check primary key exists
  if (is.na(Product$product_id[i])) { 
    print(paste("Table: Product - Error: product_id is NULL on row",i))
    valid_prd = 0
  }
  
  #check foreign key exists
  if (is.na(Product$category_id[i])) { 
    print(paste("Table: Product - Error: category_id is NULL on row",i))
    valid_prd = 0
  }
  
  if (is.na(Product$supplier_id[i])) { 
    print(paste("Table: Product - Error: supplier_id is NULL on row",i))
    valid_prd = 0
  }
  
  #check character length
  if (nchar(Product$product_id[i]) > 6) { 
    print(paste("Table: Product - Error: product_id more than 6 characters on row",i))
    valid_prd = 0
  }
  
  if (nchar(Product$category_id[i]) > 6) { 
    print(paste("Table: Product - Error: category_id more than 6 characters on row",i))
    valid_prd = 0
  }
  
  if (nchar(Product$supplier_id [i]) > 6) { 
    print(paste("Table: Product - Error: suplier_id more than 6 characters on row",i))
    valid_prd = 0
  }
}

#check primary key is unique
if(length(unique(Product$product_id)) != nrow(Product)) {
  print(paste("Table: Product - Error: duplicated product_id"))
  valid_prd = 0
}

#fix data type
Product$price <- as.numeric(Product$price)
Product$discount_rate <- as.numeric(Product$discount_rate)

#final check result
if (valid_prd == 1) {
  print("Table: Product - Status: OK")
} else {print("Table: Product - Status: ERROR")}


#PROMOTION
for (i in 1:nrow(Promotion))
{
  #check primary key exists
  if (is.na(Promotion$promotion_id[i])) { 
    print(paste("Table: Promotion - Error: promotion_id is NULL on row",i))
    valid_prm = 0
  }
  
  #check foreign key exists
  if (is.na(Promotion$supplier_id[i])) { 
    print(paste("Table: Promotion - Error: supplier_id is NULL on row",i))
    valid_prm = 0
  }
  
  #check character length
  if (nchar(Promotion$promotion_id[i]) > 6) { 
    print(paste("Table: Promotion - Error: promotion_id more than 6 characters on row",i))
    valid_prm = 0
  }
  
  if (nchar(Promotion$supplier_id[i]) > 6) { 
    print(paste("Table: Promotion - Error: supplier_id more than 6 characters on row",i))
    valid_prm = 0
  }
}

#check primary key is unique
if(length(unique(Promotion$promotion_id)) != nrow(Promotion)) {
  print(paste("Table: Promotion - Error: duplicated promotion_id"))
  valid_prm = 0
}

#fix data type
Promotion$promotion_fees <- as.numeric(Promotion$promotion_fees)
Promotion$promotion_start_date <- mdy(Promotion$promotion_start_date)
Promotion$promotion_end_date <- mdy(Promotion$promotion_end_date)

#final check result
if (valid_prm == 1) {
  print("Table: Promotion - Status: OK")
} else {print("Table: Promotion - Status: ERROR")}


#SALES
for (i in 1:nrow(Sales))
{
  #check primary key exists
  if (is.na(Sales$sale_id[i])) { 
    print(paste("Table: Sales - Error: sale_id is NULL on row",i))
    valid_sal = 0
  }
  
  #check foreign key exists
  if (is.na(Sales$supplier_id[i])) { 
    print(paste("Table: Sales - Error: supplier_id is NULL on row",i))
    valid_sal = 0
  }
  
  if (is.na(Sales$product_id[i])) { 
    print(paste("Table: Sales - Error: product_id is NULL on row",i))
    valid_sal = 0
  }
  
  #check character length
  if (nchar(Sales$sale_id[i]) > 6) { 
    print(paste("Table: Sales - Error: sale_id more than 6 characters on row",i))
    valid_sal = 0
  }
  
  if (nchar(Sales$supplier_id[i]) > 6) { 
    print(paste("Table: Sales - Error: supplier_id more than 6 characters on row",i))
    valid_sal = 0
  }
  
  if (nchar(Sales$product_id[i]) > 6) { 
    print(paste("Table: Sales - Error: product_id more than 6 characters on row",i))
    valid_sal = 0
  }
}

#check primary key is unique
Sales <- Sales[!duplicated(Sales$sale_id) & !duplicated(Sales$sale_id, fromLast = TRUE), ]

if(length(unique(Sales$sale_id)) != nrow(Sales)) {
  print(paste("Table: Sales - Error: duplicated sale_id"))
  valid_sal = 0
}

#correct duplicate values
Sales$sale_date <- mdy(Sales$sale_date)
#final check result
if (valid_sal == 1) {
  print("Table: Sales - Status: OK")
} else {
  print("Table: Sales - Status: ERROR")
}



#SETTLEMENT
for (i in 1:nrow(Settlement))
{
  #check primary key exists
  if (is.na(Settlement$settlement_id[i])) { 
    print(paste("Table: Settlement - Error: settlement_id is NULL on row",i))
    valid_set = 0
  }
  
  #check foreign key exists
  if (is.na(Settlement$sale_id[i])) { 
    print(paste("Table: Settlement - Error: sale_id is NULL on row",i))
    valid_set = 0
  }
  
  #check character length
  if (nchar(Settlement$settlement_id[i]) > 6) { 
    print(paste("Table: Settlement - Error: settlement_id more than 6 characters on row",i))
    valid_set = 0
  }
  
  if (nchar(Settlement$sale_id[i]) > 6) { 
    print(paste("Table: Settlement - Error: sale_id more than 6 characters on row",i))
    valid_set = 0
  }
}

#check primary key is unique
if(length(unique(Settlement$settlement_id)) != nrow(Settlement)) {
  print(paste("Table: Settlement - Error: duplicated settlement_id"))
  valid_set = 0
}

#fix data type
Settlement$settlement_date <- mdy(Settlement$settlement_date)

#final check result
if (valid_set == 1) {
  print("Table: Settlement - Status: OK")
} else {print("Table: Settlement - Status: ERROR")}



#SUPPLIER
for (i in 1:nrow(Supplier))
{
  #check primary key exists
  if (is.na(Supplier$supplier_id[i])) { 
    print(paste("Table: Supplier - Error: supplier_id is NULL on row",i))
    valid_sup = 0
  }
  
  #check character length
  if (nchar(Supplier$supplier_id[i]) > 6) { 
    print(paste("Table: Supplier - Error: supplier_id more than 6 characters on row",i))
    valid_sup = 0
  }
  
  #characters limit
  Supplier$seller_first_name[i] <- substring(Supplier$seller_first_name[i],1,50)
  Supplier$seller_last_name[i] <- substring(Supplier$seller_last_name[i],1,50)
  Supplier$seller_email[i] <- substring(Supplier$seller_email[i],1,200)
  Supplier$seller_address[i] <- substring(Supplier$seller_address[i],1,200)
  Supplier$seller_city[i] <- substring(Supplier$seller_city[i],1,50)
  
  #rate convertion to numeric
  #Supplier$platform_rate[i] <- parse_number(Supplier$platform_rate[i])
  #Supplier$tax_rate[i] <- parse_number(Supplier$tax_rate[i])
}

#check primary key is unique
if(length(unique(Supplier$supplier_id)) != nrow(Supplier)) {
  print(paste("Table: Supplier - Error: duplicated supplier_id"))
  valid_sup = 0
}

#check email is unique
if(length(unique(Supplier$seller_email)) != nrow(Supplier)) {
  print(paste("Table: Supplier - Error: duplicated seller_email"))
  valid_sup = 0
}

#fix data type
Supplier$seller_phone <- as.numeric(Supplier$seller_phone)
Supplier$registration_date <- mdy(Supplier$registration_date)
Supplier$platform_rate <- as.numeric(Supplier$platform_rate)
Supplier$tax_rate <- as.numeric(Supplier$tax_rate)


#final check result
if (valid_sup == 1) {
  print("Table: Supplier - Status: OK")
} else {print("Table: Supplier - Status: ERROR")}



if (valid_cat == 1) {
  RSQLite::dbWriteTable(my_connection, "Category", Category, append = TRUE)}

if (valid_cus == 1) {
  RSQLite::dbWriteTable(my_connection, "Customer", Customer, append = TRUE)}

if (valid_ord == 1) {
  RSQLite::dbWriteTable(my_connection, "Orders", Orders, append = TRUE)}

if (valid_pay == 1) {
  RSQLite::dbWriteTable(my_connection, "Payment", Payment, append = TRUE)}

if (valid_prd == 1) {
  RSQLite::dbWriteTable(my_connection, "Product", Product, append = TRUE)}

if (valid_prm == 1) {
  RSQLite::dbWriteTable(my_connection, "Promotion", Promotion, append = TRUE)}

if (valid_sal == 1) {
  RSQLite::dbWriteTable(my_connection, "Sales", Sales, append = TRUE)}

if (valid_set == 1) {
  RSQLite::dbWriteTable(my_connection, "Settlement", Settlement, append = TRUE)}

if (valid_sup == 1) {
  RSQLite::dbWriteTable(my_connection, "Supplier", Supplier, append = TRUE)}


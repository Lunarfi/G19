library(RSQLite)
library(readr)

my_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"mydatabase.db")

monthly_order <- "
  SELECT 
    STRFTIME('%m-%Y', date(order_date * 3600 * 24, 'unixepoch')) AS month_,
    COUNT(DISTINCT order_id) as total_order,
    COUNT(DISTINCT customer_id) as total_customer,
    COUNT(DISTINCT o.product_id) as total_product,
    SUM(order_quantity) as total_quantity
  FROM Orders o 
  LEFT JOIN Product p on o.product_id = p.product_id
  GROUP BY 1 
  ORDER BY 1 
"
monthly_finance <- "
  SELECT  
    STRFTIME('%m-%Y', date(order_date * 3600 * 24, 'unixepoch')) AS month_,
    SUM(order_quantity * p.price * (1-p.discount_rate/100)) as total_gmv,
    SUM(order_quantity * p.price * (1-p.discount_rate/100-s.tax_rate/100)) as total_nmv,
    SUM(order_quantity * p.price * (s.platform_rate/100)) as total_platform_fee,
    SUM(order_quantity * p.price * (1-p.discount_rate/100-s.tax_rate/100-s.platform_rate/100)) as total_seller_settlement
  FROM Orders o 
  LEFT JOIN Product p on o.product_id = p.product_id
  LEFT JOIN Supplier s on p.supplier_id = s.supplier_id
GROUP BY 1 
ORDER BY 1 
"

top_seller <- "
  SELECT  
    s.supplier_id,
    COUNT(DISTINCT order_id) as total_order,
    SUM(order_quantity) as total_quantity,
    SUM(order_quantity * p.price * (1-p.discount_rate/100-s.tax_rate/100-s.platform_rate/100)) as total_seller_settlement
  FROM Orders o 
  LEFT JOIN Product p on o.product_id = p.product_id
  LEFT JOIN Supplier s on p.supplier_id = s.supplier_id
GROUP BY 1
ORDER BY 2 DESC
LIMIT 10
"

top_product <- "
  SELECT  
    p.product_id,
    p.product_name,
    COUNT(DISTINCT order_id) as total_order,
    SUM(order_quantity) as total_quantity,
    SUM(order_quantity * p.price * (1-p.discount_rate/100)) as total_gmv
  FROM Orders o 
  LEFT JOIN Product p on o.product_id = p.product_id
GROUP BY 1,2
ORDER BY 3 DESC
LIMIT 10
"

print("Monthly Order")
dbGetQuery(my_connection,monthly_order)
print("Monthly Finance")
dbGetQuery(my_connection,monthly_finance)
print("Top Seller")
dbGetQuery(my_connection,top_seller)
print("Top Product")
dbGetQuery(my_connection,top_product)

library(RSQLite)
library(readr)

my_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"mydatabase.db")

#check primary key duplication
dup_cat <- "
  WITH
  check_dup AS (
  SELECT category_id, ROW_NUMBER() OVER (PARTITION BY category_id) as id_rnk 
  from Category
  )
  
  DELETE FROM Category
  WHERE category_id in 
    (SELECT category_id FROM check_dup WHERE id_rnk > 1)
"

dup_cus <- "
  WITH
  check_dup AS (
  SELECT customer_id, ROW_NUMBER() OVER (PARTITION BY customer_id) as id_rnk 
  from Customer
  )
  
  DELETE FROM Customer
  WHERE customer_id in 
    (SELECT customer_id FROM check_dup WHERE id_rnk > 1)
"

dup_ord <- "
  WITH
  check_dup AS (
  SELECT order_id, ROW_NUMBER() OVER (PARTITION BY order_id) as id_rnk 
  from Orders
  )
  
  DELETE FROM Orders
  WHERE order_id in 
    (SELECT order_id FROM check_dup WHERE id_rnk > 1)
"

dup_pay <- "
  WITH
  check_dup AS (
  SELECT payment_id, ROW_NUMBER() OVER (PARTITION BY payment_id) as id_rnk 
  from Payment
  )
  
  DELETE FROM Payment
  WHERE payment_id in 
    (SELECT payment_id FROM check_dup WHERE id_rnk > 1)
"

dup_prd <- "
  WITH
  check_dup AS (
  SELECT product_id, ROW_NUMBER() OVER (PARTITION BY product_id) as id_rnk 
  from Product
  )
  
  DELETE FROM Product
  WHERE product_id in 
    (SELECT product_id FROM check_dup WHERE id_rnk > 1)
"

dup_prm <- "
  WITH
  check_dup AS (
  SELECT promotion_id, ROW_NUMBER() OVER (PARTITION BY promotion_id) as id_rnk 
  from Promotion
  )
  
  DELETE FROM Promotion
  WHERE promotion_id in 
    (SELECT promotion_id FROM check_dup WHERE id_rnk > 1)
"

dup_sal <- "
  WITH
  check_dup AS (
  SELECT sale_id, ROW_NUMBER() OVER (PARTITION BY sale_id) as id_rnk 
  from Sales
  )
  
  DELETE FROM Sales
  WHERE sale_id in 
    (SELECT sale_id FROM check_dup WHERE id_rnk > 1)
"

dup_set <- "
  WITH
  check_dup AS (
  SELECT settlement_id, ROW_NUMBER() OVER (PARTITION BY settlement_id) as id_rnk 
  from Settlement
  )
  
  DELETE FROM Settlement
  WHERE settlement_id in 
    (SELECT settlement_id FROM check_dup WHERE id_rnk > 1)
"

dup_sup <- "
  WITH
  check_dup AS (
  SELECT supplier_id, ROW_NUMBER() OVER (PARTITION BY supplier_id) as id_rnk 
  from Supplier
  )
  
  DELETE FROM Supplier
  WHERE supplier_id in 
    (SELECT supplier_id FROM check_dup WHERE id_rnk > 1)
"

dbExecute(my_connection, dup_cat)
dbExecute(my_connection, dup_cus)
dbExecute(my_connection, dup_ord)
dbExecute(my_connection, dup_pay)
dbExecute(my_connection, dup_prd)
dbExecute(my_connection, dup_prm)
dbExecute(my_connection, dup_sal)
dbExecute(my_connection, dup_set)
dbExecute(my_connection, dup_sup)

#check referential integrity
ri_ord <- "
  WITH 
  check_ref AS (
  SELECT 
    o.order_id,
    c.customer_id,
    p.product_id
  FROM Orders o 
  LEFT JOIN Customer c on o.customer_id = c.customer_id
  LEFT JOIN Product p on o.product_id = p.product_id
  )
  
  DELETE FROM Orders
  WHERE order_id in 
    (SELECT order_id FROM check_ref WHERE customer_id IS NULL OR product_id IS NULL)
"

ri_pay <- "
  WITH 
  check_ref AS (
  SELECT 
    p.payment_id,
    o.order_id
  FROM Payment p
  LEFT JOIN Orders o on p.order_id = o.order_id
  )
  
  DELETE FROM Payment
  WHERE payment_id in 
    (SELECT payment_id FROM check_ref WHERE order_id IS NULL)
"

ri_prd <- "
  WITH 
  check_ref AS (
  SELECT 
    p.product_id,
    c.category_id,
    s.supplier_id
  FROM Product p
  LEFT JOIN Category c on p.category_id = c.category_id
  LEFT JOIN Supplier s on p.supplier_id = s.supplier_id
  )
  
  DELETE FROM Product
  WHERE product_id in 
    (SELECT product_id FROM check_ref WHERE category_id IS NULL OR supplier_id IS NULL)
"

ri_sal <- "
  WITH 
  check_ref AS (
  SELECT 
    s.sale_id,
    u.supplier_id,
    p.product_id
  FROM Sales s
  LEFT JOIN Supplier u on s.supplier_id = u.supplier_id
  LEFT JOIN Product p on s.product_id = p.product_id
  )
  
  DELETE FROM Sales
  WHERE sale_id IN
    (SELECT sale_id FROM check_ref WHERE supplier_id IS NULL OR product_id IS NULL)
"

ri_set <- "
  WITH 
  check_ref AS (
  SELECT 
    s.settlement_id,
    l.sale_id
  FROM Settlement s
  LEFT JOIN Sales l on s.sale_id = l.sale_id
  )
  
  DELETE FROM Settlement
  WHERE settlement_id in 
    (SELECT settlement_id FROM check_ref WHERE sale_id IS NULL)
"

ri_prm <- "
  WITH 
  check_ref AS (
  SELECT 
    p.promotion_id,
    s.supplier_id
  FROM Promotion p 
  LEFT JOIN Supplier s on p.supplier_id = s.supplier_id
  )
  
  DELETE FROM Promotion
  WHERE promotion_id in 
    (SELECT promotion_id FROM check_ref WHERE supplier_id IS NULL)
"

dbExecute(my_connection, ri_ord)
dbExecute(my_connection, ri_pay)
dbExecute(my_connection, ri_prd)
dbExecute(my_connection, ri_sal)
dbExecute(my_connection, ri_set)
dbExecute(my_connection, ri_prm)

#check date constraint between 2 tables

#check and fix customer registration date is before first order date
dc_cus <- "
  WITH 
  check_order AS (
  SELECT 
    c.customer_id,
    MIN(o.order_date) as first_order_date
  FROM Customer c 
  LEFT JOIN Orders o on c.customer_id = o.customer_id 
  WHERE o.order_date < c.registration_date
  GROUP BY 1
  ORDER BY 1 
  )
  
  UPDATE Customer 
  SET registration_date = (SELECT first_order_date FROM check_order)
  WHERE customer_id = (SELECT customer_id FROM check_order)
"

#check and fix supplier registration date is before first order date
dc_sup <- "
  WITH 
  check_order AS (
  SELECT 
    s.supplier_id,
    min(o.order_date) as first_order_date
  FROM Orders o 
  LEFT JOIN Product p on o.product_id = p.product_id
  LEFT JOIN Supplier s on p.supplier_id = s.supplier_id
  WHERE o.order_date < s.registration_date
  GROUP BY 1 
  ORDER BY 1 
  )
  
  UPDATE Supplier
  SET registration_date = (SELECT first_order_date FROM check_order)
  WHERE supplier_id = (SELECT supplier_id FROM check_order)
"

#check sale date is equal to order date
dc_sal <- "
  WITH 
  check_sal AS (
  SELECT DISTINCT
    s.sale_id,
    o.order_id
  FROM Sales s
  LEFT JOIN Orders o on s.product_id = o.product_id AND s.sale_date = o.order_date
  WHERE o.order_id IS NULL
  )
  
  DELETE FROM Sales
  WHERE sale_id in 
    (SELECT sale_id FROM check_sal) --Delete invalid sales that cannot be mapped to the order
"

#check payment date is after order date
dc_pay <- "
  WITH 
  check_pay AS (
  SELECT 
    p.payment_id,
    MAX(o.order_date) as last_order_date
  FROM Payment p 
  LEFT JOIN Orders o on p.order_id = o.order_id
  WHERE p.payment_date < o.order_date
  GROUP BY 1
  ORDER BY 1 
  )
  
  UPDATE Payment
  SET payment_date = (SELECT last_order_date FROM check_pay)
  WHERE payment_id = (SELECT payment_id FROM check_pay)
"

#check settlement date is after sale date
dc_set <- "
  WITH 
  check_set AS (
  SELECT 
    p.settlement_id,
    MAX(s.sale_date) as last_sale_date
  FROM Settlement p 
  LEFT JOIN Sales s on p.sale_id = s.sale_id
  WHERE p.settlement_date < s.sale_date
  GROUP BY 1
  ORDER BY 1 
  )
  
  UPDATE Settlement
  SET settlement_date = (SELECT last_sale_date FROM check_set)
  WHERE settlement_id = (SELECT settlement_id FROM check_set)
"

#check supplier registration date is before first promotion date
dc_prm <- "
  WITH 
  check_prm AS 
  (
  SELECT 
    s.supplier_id,
    min(p.promotion_start_date) AS first_promo_date
  FROM Supplier s
  LEFT JOIN Promotion p on s.supplier_id = p.supplier_id
  WHERE p.promotion_start_date < s.registration_date
  GROUP BY 1 
  ORDER BY 1 
  )
  
  UPDATE Supplier
  SET registration_date = (SELECT first_promo_date FROM check_prm)
  WHERE supplier_id = (SELECT supplier_id FROM check_prm)
"

dbExecute(my_connection, dc_cus)
dbExecute(my_connection, dc_sup)
dbExecute(my_connection, dc_pay)
dbExecute(my_connection, dc_sal)
dbExecute(my_connection, dc_set)
dbExecute(my_connection, dc_prm)



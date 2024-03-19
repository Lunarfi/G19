install.packages("tidyverse", type="source")
install.packages("writexl")

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
this_filename_date <- as.character(Sys.Date())

this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))

library(DBI)
my_connection <- dbConnect(RSQLite::SQLite(), dbname = "mydatabase.db")


category <- dbGetQuery(my_connection, "SELECT * FROM Category")
customer <- dbGetQuery(my_connection, "SELECT * FROM Customer")
order <- dbGetQuery(my_connection, "SELECT * FROM Orders")
payment <- dbGetQuery(my_connection, "SELECT * FROM Payment")
product <- dbGetQuery(my_connection, "SELECT * FROM Product")
promotion <- dbGetQuery(my_connection, "SELECT * FROM Promotion")
sale <- dbGetQuery(my_connection, "SELECT * FROM Sales")
settlement <- dbGetQuery(my_connection, "SELECT * FROM Settlement")
supplier <- dbGetQuery(my_connection, "SELECT * FROM Supplier")


supplier_gross_df <- supplier %>%
  inner_join(product, by = "supplier_id")


supplier_gross_df <- supplier_gross_df %>%
  mutate(
    final_seller_price = round(price - (price * ((tax_rate/100) + (discount_rate/100) +(platform_rate/100))), 2))%>%mutate(seller= paste0(seller_first_name," ", seller_last_name))

supplier_gross_df <- supplier_gross_df %>% merge(order, by = "product_id")
supplier_gross_df$fullp <- supplier_gross_df$order_quantity*supplier_gross_df$final_seller_price


seller_details <- supplier_gross_df %>% group_by(supplier_id, seller) %>% summarize(total_settlement = sum(fullp))

seller_details <- seller_details %>% arrange(desc(total_settlement))
print(seller_details)

write_xlsx(seller_details, path = paste0("figures/tables/supplier-settlement",
                                         this_filename_date,"_",
                                         this_filename_time,".xlsx"),col_names = TRUE)

merged_order <- order %>%
  inner_join(product, customer, by = "product_id")


final_price_df <- merged_order %>%
  mutate(
    final_price = round(price - (price * (discount_rate / 100)), 2)
  )


final_price_df$full_price <-  (final_price_df$final_price*final_price_df$order_quantity)
lm_order_payment <-  final_price_df %>%
  inner_join(payment, by = "order_id")%>%
  select(order_id, payment_method, full_price)%>%
  group_by(payment_method)%>%
  summarise(total_per_payment_method = sum(full_price))

library(ggplot2)

ggplot(lm_order_payment, aes(x = payment_method, y = total_per_payment_method, fill = payment_method)) +
  geom_col() +  # Use geom_col() for pre-summarized data
  labs(title = "Total Revenue by Payment Method",
       x = "Payment Method",
       y = "Total Revenue") +
  theme_minimal()+
  scale_fill_brewer(palette = )
ggsave(plot = last_plot(),paste0("figures/barplots/payment_methods",
                                 this_filename_date,"_",
                                 this_filename_time,".png"))


ggplot(settlement, aes(x = settlement_type, fill = settlement_type)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Bar Chart of Settlement Types",
       x = "Settlement Type",
       y = "Count") +
  scale_fill_brewer(palette = )
ggsave(plot = last_plot(),paste0("figures/barplots/settlement_types",
                                 this_filename_date,"_",
                                 this_filename_time,".png"))

review_order_db <- order

review_order_plot_data <- review_order_db%>% 
  select(order_id, rating_score, order_quantity)%>%
  na.omit()

ggplot(review_order_plot_data, aes(x = rating_score, y = order_quantity)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Regression Plot of Order Score and Order Quantity",
       x = "rating score 1-5",
       y = "number of items ordered")
ggsave(plot = last_plot(),paste0("figures/regression_plot/score-quantity",
                                 this_filename_date,"_",
                                 this_filename_time,".png"))



merge2 <- order %>%
  full_join(product, by = "product_id")

merge2$final_price  <- round(merge2$price* (1- (merge2$discount_rate/100)))

merge2$fullprice <- (merge2$final_price)*(merge2$order_quantity)


cus_order <- merge2%>% full_join(customer, by = "customer_id")



final_price_df <- cus_order %>%
  mutate(final_price = round(price - (price * (discount_rate / 100)), 2)) %>%
  select(order_id,customer_first_name, customer_last_name, fullprice)

final_price_df$name <- paste(final_price_df$customer_first_name, final_price_df$customer_last_name, sep = " ") 

final_price_df$customer_first_name <- NULL
final_price_df$customer_last_name <- NULL


rev_per_cm <- final_price_df %>% group_by(name)%>% summarize(sum(fullprice))
rev_per_cm$revenue <- rev_per_cm$`sum(fullprice)`
rev_per_cm$`sum(fullprice)`<- NULL
rev_per_cm <- rev_per_cm %>% arrange(desc(revenue))

library(writexl)

write_xlsx(rev_per_cm, path = paste0("figures/tables/revenue_per_customer",
                                     this_filename_date,"_",
                                     this_filename_time,".xlsx"),col_names = TRUE)


print(head(rev_per_cm))


ms_products <- merge2 %>% group_by(product_id)%>%
  summarize(revenue = sum(fullprice), quantity = sum(order_quantity))

ms_product_name <- ms_products %>% merge(product, by = "product_id")


ms_product123 <- ms_product_name %>% merge(category, by = "category_id" )%>%
  select(product_name,revenue, quantity, price, discount_rate )




ms_product_name$final_price <- ms_product_name$price * (1-((ms_product_name$discount_rate)/100))

ms_product_df <- ms_product_name %>% arrange(desc(revenue)) %>% select( product_name,final_price,quantity, revenue)

write_xlsx(ms_product_df, path = paste0("figures/tables/top-selling-products",
                                        this_filename_date,"_",
                                        this_filename_time,".xlsx"),col_names = TRUE)


print(head(ms_product_name))

lmporsche <- lm(quantity~discount_rate , data = ms_product_name)
summary(lmporsche)
ggplot(data = ms_product_name, mapping = aes(y = quantity, x = discount_rate))+
  geom_point()+ geom_smooth(method = lm, se = TRUE)



ggsave(plot = last_plot(),paste0("figures/regression_plot/discount-quantity",
                                 this_filename_date,"_",
                                 this_filename_time,".png"))

lm_gt3 <- lm(quantity~price, data = ms_product_name )
summary(lm_gt3)


ggplot(data = ms_product_name, mapping = aes(y = quantity, x = price))+
  geom_point()+ geom_smooth(method = lm, se = TRUE)
ggsave(paste0("figures/regression_plot/price-quantity",
              this_filename_date,"_",
              this_filename_time,".png"))


dbDisconnect(my_connection)


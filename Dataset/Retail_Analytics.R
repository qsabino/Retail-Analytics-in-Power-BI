library(dplyr)
library(lubridate)
library(stringi)

set.seed(123)

# Generate Products
n_products <- 50
products <- data.frame(
  Product_SKU = 1:n_products,
  Product_Full_Description = paste("Product", 1:n_products, "full desc"),
  Product_Gender = sample(c("Male", "Female", "Unisex"), n_products, replace = TRUE),
  Product_Category = sample(c("Furniture", "Office Supplies", "Sports", "Electronics", "Clothing"), n_products, replace = TRUE),
  Product_Name = paste("Product", 1:n_products),
  Product_Size = sample(c("S","M","L","XL"), n_products, replace = TRUE),
  Product_Color = sample(c("Black","White","Blue","Green","Red"), n_products, replace = TRUE)
)

# Generate Retailers
n_retailers <- 30
retailers <- data.frame(
  Retailer_ID = 1:n_retailers,
  Retailer_Channel = sample(c("Online", "In-Store", "Distributor"), n_retailers, replace = TRUE),
  Retailer_Name = paste("Retailer", 1:n_retailers),
  City = paste("City", sample(1:20, n_retailers, replace = TRUE)),
  Region = sample(c("Africa", "North America", "South America", "Europe", "Asia"), n_retailers, replace = TRUE),
  Area = sample(c("Urban","Suburban","Rural"), n_retailers, replace = TRUE),
  Country = sample(c("USA","UK","India","Brazil","Germany"), n_retailers, replace = TRUE),
  Distance_from_Warehouse = round(runif(n_retailers, 10, 500), 1)
)

# Generate Orders
n_orders <- 500
orders <- data.frame(
  Order_ID = 1001:(1000 + n_orders),
  Order_Date = sample(seq(as.Date('2021-01-01'), as.Date('2024-12-31'), by="day"), n_orders, replace = TRUE),
  Retailer_ID = sample(retailers$Retailer_ID, n_orders, replace = TRUE),
  Product_SKU = sample(products$Product_SKU, n_orders, replace = TRUE),
  Product_Price = round(runif(n_orders, 20, 1000),2),
  Product_Cost = round(runif(n_orders, 10, 800),2),
  Order_Quantity = sample(1:10, n_orders, replace = TRUE),
  Customer_ID = sample(customers$Customer_ID, n_orders, replace = TRUE) # Added Customer_ID
)

# Calculate sales, COGS, discount, profit
orders <- orders %>%
  mutate(
    #Sale_Amount = Product_Price * Order_Quantity,
    #Cost_of_Goods_Sold = Product_Cost * Order_Quantity,
    Product_Discount = round(Product_Price * Order_Quantity * runif(n_orders, 0, 0.3),2),
    #Profit = Sale_Amount - Cost_of_Goods_Sold - Product_Discount,
    #Profit_Margin = round(Profit / Sale_Amount,2)
  )

# Generate Returns
library(dplyr)
library(lubridate)

set.seed(123)  # for reproducibility

n_returns <- 50  # number of return records to create

returns <- data.frame(
  Order_ID = sample(orders$Order_ID, n_returns, replace = TRUE),
  Return_Year = sample(2021:2024, n_returns, replace = TRUE),
  Return_Month = sample(1:12, n_returns, replace = TRUE),
  Return_Day = sample(1:28, n_returns, replace = TRUE),
  Hour = sample(0:23, n_returns, replace = TRUE),
  Minute = sample(0:59, n_returns, replace = TRUE),
  Second = sample(0:59, n_returns, replace = TRUE)
) %>%
  mutate(
    ReturnDate = ISOdatetime(Return_Year, Return_Month, Return_Day,
                             Hour, Minute, Second, tz = "UTC")
  )

# ---- Enrich with order data ----
returns <- returns %>%
  left_join(orders, by = "Order_ID") %>%
  rowwise() %>%
  mutate(
    # Random partial returns: between 1 and full order quantity
    Return_Quantity = sample(1:Order_Quantity, 1)
  ) %>%
  ungroup() %>%
  # ---- Keep Return_Amount if you still want to pre-calc it ----
# mutate(Return_Amount = Return_Quantity * (Product_Price - Product_Discount)) %>%
select(
  Order_ID,
  Product_SKU,
  Return_Quantity,
  # Return_Amount,   # ← keep this only if you decide to pre-calc it
  Return_Year,
  Return_Month,
  Return_Day,
  ReturnDate
)

cat("✅ Fact_Returns.csv generated successfully!\n")

# Save to CSV (for Power BI import)
write.csv(products, "Dim_Products.csv", row.names = FALSE)
write.csv(retailers, "Dim_Retailers.csv", row.names = FALSE)
write.csv(orders, "Fact_Orders.csv", row.names = FALSE)
write.csv(returns, "Fact_Returns.csv", row.names = FALSE)

# Assuming products, retailers, orders are already loaded
# products, retailers, orders

# 1. Customers Table
n_customers <- 200
customers <- data.frame(
  Customer_ID = 1:n_customers,
  Customer_Name = paste("Customer", 1:n_customers),
  Age_Group = sample(c("18-25","26-35","36-45","46-60","60+"), n_customers, replace = TRUE),
  Gender = sample(c("Male","Female","Unisex"), n_customers, replace = TRUE),
  Region = sample(c("Africa", "North America", "South America", "Europe", "Asia"), n_customers, replace = TRUE),
  Channel = sample(c("Online","In-Store"), n_customers, replace = TRUE),
  Loyalty_Score = round(runif(n_customers, 0, 100),1)
)

# Link Customers to Orders
orders <- orders %>%
  mutate(Customer_ID = sample(customers$Customer_ID, nrow(orders), replace = TRUE))

# 2. Promotions / Campaigns Table
n_promotions <- 30
promotions <- data.frame(
  Promotion_ID = 1:n_promotions,
  Product_SKU = sample(products$Product_SKU, n_promotions, replace = TRUE),
  Start_Date = sample(seq(as.Date('2021-01-01'), as.Date('2024-12-01'), by="day"), n_promotions, replace = TRUE),
  End_Date = sample(seq(as.Date('2021-01-02'), as.Date('2024-12-31'), by="day"), n_promotions, replace = TRUE),
  Discount_Percentage = round(runif(n_promotions, 5, 50),1),
  Campaign_Name = paste("Promo", 1:n_promotions)
) %>%
  mutate(
    End_Date = ifelse(End_Date < Start_Date, Start_Date + sample(5:30, n_promotions, replace = TRUE), End_Date),
    End_Date = as.Date(End_Date, origin = "1970-01-01")
  )

# 3. Shipping / Delivery fact table
library(dplyr)
library(lubridate)

set.seed(123)  # for reproducibility

shippings <- orders %>%
  select(Order_ID, Order_Date) %>%
  mutate(
    Ship_Date = Order_Date + sample(1:5, nrow(.), replace = TRUE),
    Delivery_Duration = sample(2:10, nrow(.), replace = TRUE),
    Delivery_Date = Ship_Date + Delivery_Duration,
    Shipping_Method = sample(c("Standard", "Express", "Same-Day"), nrow(.), replace = TRUE),
    Carrier = sample(c("FedEx", "UPS", "DHL", "USPS"), nrow(.), replace = TRUE),
    Shipping_Cost = round(runif(nrow(.), 5, 50), 2),
    Delivery_Status = sample(c("On-Time", "Delayed", "Returned"), nrow(.), replace = TRUE, prob = c(0.7, 0.25, 0.05)),
    Warehouse_ID = sample(warehouses$Warehouse_ID, nrow(.), replace = TRUE)  # Added Warehouse_ID
  )


# 4. Warehouses Table
warehouses <- data.frame(
  Warehouse_ID = 1:n_warehouses,
  Warehouse_Location = paste("Warehouse", 1:n_warehouses),
  Region = sample(c("Africa", "North America", "South America", "Europe", "Asia"), n_warehouses, replace = TRUE),
  Capacity = sample(1000:5000, n_warehouses, replace = TRUE)
)

# 5. Calendar / Time Table
all_dates <- seq(as.Date("2021-01-01"), as.Date("2024-12-31"), by="day")
calendar <- data.frame(
  Date = all_dates,
  Year = year(all_dates),
  Month = month(all_dates),
  Quarter = quarter(all_dates),
  Weekday = weekdays(all_dates),
  Is_Holiday = sample(c(TRUE, FALSE), length(all_dates), replace = TRUE, prob=c(0.1,0.9))
)

# Save tables to CSV for Power BI
write.csv(customers, "Dim_Customers.csv", row.names = FALSE)
write.csv(promotions, "Dim_Promotions.csv", row.names = FALSE)
write.csv(shippings, "Fact_Shippings.csv", row.names = FALSE)
write.csv(warehouses, "Dim_Warehouses.csv", row.names = FALSE)
write.csv(calendar, "Dim_Date.csv", row.names = FALSE)

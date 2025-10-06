# 02_eda.R
# Exploratory Data Analysis (EDA) for Online Retail II

library(data.table)
library(ggplot2)
library(lubridate)

# Load cleaned dataset
retail <- fread("data/processed/retail_clean.csv")

# -------------------------------
# 1. Basic dataset summary
# -------------------------------
cat("Number of rows: ", nrow(retail), "\n")
cat("Number of unique customers: ", uniqueN(retail$CustomerID), "\n")
cat("Number of unique products: ", uniqueN(retail$StockCode), "\n")
cat("Date range: ", min(retail$InvoiceDate), " to ", max(retail$InvoiceDate), "\n")

# -------------------------------
# 2. Top products by frequency
# -------------------------------
top_products <- retail[, .N, by = Description][order(-N)][1:20]
print(top_products)

ggplot(top_products, aes(x = reorder(Description, N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Most Purchased Products",
       x = "Product", y = "Purchase Count")

# -------------------------------
# 3. Top customers by spending
# -------------------------------
customer_spending <- retail[, .(TotalSpent = sum(Amount)), by = CustomerID][order(-TotalSpent)][1:20]
print(customer_spending)

ggplot(customer_spending, aes(x = reorder(CustomerID, TotalSpent), y = TotalSpent)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 Customers by Spending",
       x = "CustomerID", y = "Total Spent")

# -------------------------------
# 4. Sales over time
# -------------------------------
retail[, InvoiceDate := as.Date(InvoiceDate)]
daily_sales <- retail[, .(Revenue = sum(Amount)), by = InvoiceDate]

ggplot(daily_sales, aes(x = InvoiceDate, y = Revenue)) +
  geom_line(color = "purple") +
  labs(title = "Daily Revenue Trend",
       x = "Date", y = "Revenue")

# -------------------------------
# 5. Basket size distribution
# -------------------------------
basket_size <- retail[, .N, by = InvoiceNo]
ggplot(basket_size, aes(x = N)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Basket Size Distribution (Items per Invoice)",
       x = "Items per Invoice", y = "Count of Invoices")

# -------------------------------
# 6. RFM summary (Recency, Frequency, Monetary)
# -------------------------------
snapshot_date <- max(retail$InvoiceDate) + 1

rfm <- retail[, .(
  Recency = as.numeric(difftime(snapshot_date, max(InvoiceDate), units = "days")),
  Frequency = uniqueN(InvoiceNo),
  Monetary = sum(Amount)
), by = CustomerID]

summary(rfm)
fwrite(rfm, "data/processed/rfm_summary.csv")

cat("✅ EDA complete. Outputs saved: plots + rfm_summary.csv\n")

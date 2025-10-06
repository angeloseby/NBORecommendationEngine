# 01_data_prep.R
# Data Ingestion & Cleaning for Online Retail II dataset

library(data.table)
library(lubridate)
library(readxl)

# Path to dataset (download Online Retail II from UCI and place under data/raw/)
file_path <- "data/raw/online_retail_II.xlsx"

# Read both sheets (Year 2009-2010 and 2010-2011)
retail1 <- as.data.table(read_excel(file_path, sheet = "Year 2009-2010"))
retail2 <- as.data.table(read_excel(file_path, sheet = "Year 2010-2011"))
retail <- rbindlist(list(retail1, retail2), fill = TRUE)

# --- Rename columns for consistency ---
# Original names: Invoice, StockCode, Description, Quantity, InvoiceDate,
# Price, Customer ID, Country
setnames(retail, old = c("Invoice", "StockCode", "Description", "Quantity",
                         "InvoiceDate", "Price", "Customer ID", "Country"),
         new = c("InvoiceNo", "StockCode", "Description", "Quantity",
                 "InvoiceDate", "UnitPrice", "CustomerID", "Country"))

# --- Cleaning ---
# 1. Remove cancellations (InvoiceNo starting with 'C')
retail <- retail[!grepl("^C", InvoiceNo)]

# 2. Remove missing Customer IDs
retail <- retail[!is.na(CustomerID)]

# 3. Remove negative or zero quantities
retail <- retail[Quantity > 0]

# 4. Convert InvoiceDate to datetime
retail[, InvoiceDate := ymd_hms(InvoiceDate)]

# 5. Standardize item descriptions (lowercase + trim spaces)
retail[, Description := trimws(tolower(Description))]

# 6. Add total transaction amount column
retail[, Amount := Quantity * UnitPrice]

# Save cleaned dataset
fwrite(retail, "data/processed/retail_clean.csv")

cat("✅ Data cleaning complete. Cleaned file saved in data/processed/retail_clean.csv\n")


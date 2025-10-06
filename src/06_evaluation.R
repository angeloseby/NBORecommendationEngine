# 06_evaluation.R
# Evaluation Metrics for Next Best Offer (NBO) Engine

library(data.table)
library(jsonlite)

# -------------------------------
# 1. Load Data
# -------------------------------
retail <- fread("data/processed/retail_clean.csv")
rules <- readRDS("data/processed/association_rules.rds")
source("src/05_nbo_engine.R") # load compute_nbo_score()

# Convert date to proper Date type
retail[, InvoiceDate := as.Date(InvoiceDate)]

# -------------------------------
# 2. Train-Test Split (Temporal)
# -------------------------------
# Split by date: first 80% of timeline → train, rest 20% → test
date_cutoff <- as.Date(quantile(as.numeric(retail$InvoiceDate), 0.8), origin = "1970-01-01")
train <- retail[InvoiceDate <= date_cutoff]
test <- retail[InvoiceDate > date_cutoff]

cat("Train period:", min(train$InvoiceDate), "to", max(train$InvoiceDate), "\n")
cat("Test period:", min(test$InvoiceDate), "to", max(test$InvoiceDate), "\n")

# -------------------------------
# 3. Prepare helper: get next-order items per customer
# -------------------------------
get_next_order_items <- function(customer_id) {
  orders <- test[CustomerID == customer_id, unique(InvoiceNo)]
  if (length(orders) == 0) return(character(0))
  first_order <- orders[1]
  return(unique(test[InvoiceNo == first_order, Description]))
}

# -------------------------------
# 4. Evaluation Metrics
# -------------------------------

precision_at_k <- function(recommended, actual, k = 5) {
  if (length(actual) == 0) return(NA)
  hits <- sum(recommended[1:k] %in% actual)
  return(hits / k)
}

recall_at_k <- function(recommended, actual, k = 5) {
  if (length(actual) == 0) return(NA)
  hits <- sum(recommended[1:k] %in% actual)
  return(hits / length(actual))
}

# -------------------------------
# 5. Evaluate on sample of customers
# -------------------------------
set.seed(123)
sample_customers <- sample(unique(test$CustomerID), 50)

results <- data.table(CustomerID = sample_customers)

results[, `:=` (
  Precision = NA_real_,
  Recall = NA_real_
)]

for (i in seq_len(nrow(results))) {
  cid <- results$CustomerID[i]
  
  actual <- get_next_order_items(cid)
  recs <- compute_nbo_score(cid, top_k = 5)
  
  if (is.null(recs) || !"Item" %in% names(recs) || nrow(recs) == 0 || anyNA(recs$Item[1])) next
  
  results$Precision[i] <- precision_at_k(recs$Item, actual, 5)
  results$Recall[i] <- recall_at_k(recs$Item, actual, 5)
}

# Remove NA rows
results <- na.omit(results)

# -------------------------------
# 6. Aggregate metrics
# -------------------------------
eval_summary <- data.table(
  Metric = c("Precision@5", "Recall@5"),
  Value = c(mean(results$Precision), mean(results$Recall))
)

print(eval_summary)

# Save results
fwrite(results, "data/processed/evaluation_results.csv")
fwrite(eval_summary, "data/processed/evaluation_summary.csv")

cat("✅ Evaluation complete. Metrics saved in data/processed/evaluation_summary.csv\n")

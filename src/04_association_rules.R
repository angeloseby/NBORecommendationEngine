# 04_association_rules.R
# Association Rule Mining using Apriori Algorithm

library(data.table)
library(arules)
library(arulesViz)

# -------------------------------
# 1. Load cleaned data
# -------------------------------
retail <- fread("data/processed/retail_clean.csv")

# Select only required columns
retail <- retail[, .(InvoiceNo, Description, CustomerID)]

# Remove duplicates (same item twice in same invoice)
retail <- unique(retail)

# -------------------------------
# 2. Prepare data for transactions
# -------------------------------
# Each InvoiceNo = one transaction containing multiple items
transactions_list <- split(retail$Description, retail$InvoiceNo)

# Convert list to "transactions" object
transactions <- as(transactions_list, "transactions")

cat("✅ Transactions created: ", length(transactions), "\n")

# -------------------------------
# 3. Run Apriori Algorithm
# -------------------------------
# Support = frequency threshold; Confidence = reliability of rule
rules <- apriori(
  transactions,
  parameter = list(supp = 0.001, conf = 0.3, minlen = 2, maxlen = 10)
)


# Summary of rules
summary(rules)

# -------------------------------
# 4. Sort & Inspect top rules
# -------------------------------
rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_sorted, 10))

# -------------------------------
# 5. Remove redundant rules
# -------------------------------
redundant <- is.redundant(rules_sorted)
rules_pruned <- rules_sorted[!redundant]

cat("✅ Rules after pruning: ", length(rules_pruned), "\n")

# -------------------------------
# 6. Save rules
# -------------------------------
saveRDS(rules_pruned, "data/processed/association_rules.rds")

# Convert to data.frame for analysis
rules_df <- as(rules_pruned, "data.frame")
fwrite(rules_df, "data/processed/association_rules.csv")

# -------------------------------
# 7. Visualization
# -------------------------------
# Top 20 rules by lift
top20 <- head(rules_pruned, 20)
plot(top20, method = "graph", engine = "igraph")

# Support vs Confidence scatter
plot(rules_pruned, measure = c("support", "confidence"), shading = "lift", jitter = 0)

top10_df <- as(head(rules_pruned, 10), "data.frame")
fwrite(top10_df, "data/processed/top10_rules.csv")

cat("✅ Association rule mining complete.\n")

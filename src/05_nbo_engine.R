library(data.table)
library(arules)
library(jsonlite)
library(here)  # added here package for path handling

# -------------------------------
# 1. Load required data using here()
# -------------------------------
retail <- fread(here("data", "processed", "retail_clean.csv"))
rfm <- fread(here("data", "processed", "rfm_segmented.csv"))
rules <- readRDS(here("data", "processed", "association_rules.rds"))

# -------------------------------
# 2. Compute global popularity of items
# -------------------------------
item_popularity <- retail[, .N, by = Description]
item_popularity[, Popularity := N / sum(N)]
item_popularity[, N := NULL]

# -------------------------------
# 3. Prepare helper functions
# -------------------------------

# Function to get recent items purchased by a customer
get_recent_items <- function(customer_id, n = 5) {
  cust_data <- retail[CustomerID == customer_id]
  if (nrow(cust_data) == 0) return(character(0))
  recent_items <- cust_data[order(-InvoiceDate)][, head(unique(Description), n)]
  return(recent_items)
}

# Function to extract RHS candidates from rules given LHS items
get_candidate_rules <- function(lhs_items) {
  subset(rules, lhs %in% lhs_items)
}

# Function to compute NBO score for each item
compute_nbo_score <- function(customer_id, top_k = 5) {
  lhs_items <- get_recent_items(customer_id)
  if (length(lhs_items) == 0) return(data.table(Offer = "No purchase history", Score = NA))
  
  # Candidate rules matching customer's items
  matched_rules <- subset(rules, 
                          Reduce(`|`, lapply(lhs_items, function(x) lhs %pin% x)))
  if (length(matched_rules) == 0) return(data.table(Offer = "No related rules", Score = NA))
  
  rule_df <- as(matched_rules, "data.frame")
  
  # Extract RHS items
  rhs_items <- unique(unlist(LIST(rhs(matched_rules))))
  
  # Merge with popularity + segment data
  cust_segment <- rfm[CustomerID == customer_id, Segment]
  if (length(cust_segment) == 0) cust_segment <- NA
  
  # Basic scoring
  scores <- data.table(Item = rhs_items)
  scores[, RuleScore := sapply(Item, function(i) {
    rel_rules <- subset(matched_rules, rhs %pin% i)
    if (length(rel_rules) == 0) return(0)
    max(quality(rel_rules)$confidence * quality(rel_rules)$lift)
  })]
  
  # Add popularity
  scores <- merge(scores, item_popularity, by.x = "Item", by.y = "Description", all.x = TRUE)
  scores[is.na(Popularity), Popularity := 0]
  
  # Weighted scoring formula
  alpha <- 0.6; beta <- 0.4
  scores[, Score := alpha * RuleScore + beta * Popularity]
  
  # Top K results
  top_scores <- head(scores[order(-Score)], top_k)
  return(top_scores)
}

# -------------------------------
# 4. Demo output for random customer
# -------------------------------
sample_customer <- sample(unique(retail$CustomerID), 1)
cat("🔹 Generating NBO for Customer:", sample_customer, "\n")

nbo_result <- compute_nbo_score(sample_customer, top_k = 5)
print(nbo_result)

# Save a sample JSON output
json_output <- toJSON(list(
  customer = sample_customer,
  recommendations = nbo_result
), pretty = TRUE, auto_unbox = TRUE)

write(json_output, file = here("data", "processed", "sample_nbo_output.json"))

cat("✅ NBO Engine complete. Sample output saved: data/processed/sample_nbo_output.json\n")

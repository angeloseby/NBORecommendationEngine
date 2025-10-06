# 03_rfm_segmentation.R
# Customer Segmentation using RFM (Recency, Frequency, Monetary)

library(data.table)
library(ggplot2)
library(lubridate)
library(cluster)
library(factoextra)

# Load RFM summary from previous step
rfm <- fread("data/processed/rfm_summary.csv")

# -------------------------------
# 1. Scale RFM values
# -------------------------------
rfm_scaled <- scale(rfm[, .(Recency, Frequency, Monetary)])

# -------------------------------
# 2. Determine optimal number of clusters (Elbow method)
# -------------------------------
wss <- sapply(1:10, function(k){
  kmeans(rfm_scaled, centers = k, nstart = 20)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)", ylab = "Within-cluster sum of squares",
     main = "Elbow Method for K-Means")

# -------------------------------
# 3. Apply K-Means clustering
# -------------------------------
set.seed(123)
k <- 4   # choose from elbow method or silhouette
seg_k <- kmeans(rfm_scaled, centers = k, nstart = 50)

# Add segment labels to RFM data
rfm[, Segment := seg_k$cluster]

# -------------------------------
# 4. Analyze segment profiles
# -------------------------------
segment_profiles <- rfm[, .(
  AvgRecency = mean(Recency),
  AvgFrequency = mean(Frequency),
  AvgMonetary = mean(Monetary),
  Count = .N
), by = Segment]

print(segment_profiles)

# Save segmentation results
fwrite(rfm, "data/processed/rfm_segmented.csv")
fwrite(segment_profiles, "data/processed/segment_profiles.csv")

# -------------------------------
# 5. Visualize segments
# -------------------------------
fviz_cluster(seg_k, data = rfm_scaled,
             geom = "point", ellipse.type = "norm",
             main = "Customer Segments (RFM Clustering)")

cat("✅ Segmentation complete. Results saved: rfm_segmented.csv & segment_profiles.csv\n")


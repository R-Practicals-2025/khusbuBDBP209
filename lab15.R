# (V) Linear Regression
x_vals <- c(0.5, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
y_vals <- c(0.87, 7.12, 14.01, 24.37, 39.058, 58.62, 83.92)

# Fit a 3rd degree polynomial
poly_fit <- lm(y_vals ~ x_vals + I(x_vals^2) + I(x_vals^3))

# (2) Coefficients
cat("Coefficient of x:", coef(poly_fit)["x_vals"], "\n")
cat("Coefficient of x^2:", coef(poly_fit)["I(x_vals^2)"], "\n")
cat("Coefficient of x^3:", coef(poly_fit)["I(x_vals^3)"], "\n")

# (5) Plot original points
plot(x_vals, y_vals, main = "Degree 3 Polynomial Fit",
     xlab = "x", ylab = "y", pch = 19, col = "blue")

# Add fitted curve
x_grid <- seq(min(x_vals), max(x_vals), length.out = 100)
y_predicted <- predict(poly_fit, newdata = data.frame(x_vals = x_grid))
lines(x_grid, y_predicted, col = "red", lwd = 2)

legend("topleft", legend = c("Data Points", "Fitted Line"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA,1))


# (VI) Multiple Linear Regression

# (i) Load dataset
library(datasets)
tree_data <- trees

# Convert units
tree_data$Girth <- tree_data$Girth * 0.0254
tree_data$Height <- tree_data$Height * 0.3048
tree_data$Volume <- tree_data$Volume * 0.028317

head(tree_data)

# (ii) Scatter plot matrix
library(lattice)
splom(tree_data, xlab = "Scatter Plot Matrix")

# Observations: (same as before)

# (iii) Response vector y
response_y <- tree_data$Volume

# (iv) Intercept vector
intercept_vec <- rep(1, nrow(tree_data))

# (v) Build X matrix
X_mat <- cbind(intercept_vec, tree_data$Girth, tree_data$Height)

# (vi) Solve for beta
beta_estimates <- solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% response_y

# Print coefficients
cat("β0 (Intercept):", beta_estimates[1], "\n")
cat("β1 (Girth coef):", beta_estimates[2], "\n")
cat("β2 (Height coef):", beta_estimates[3], "\n")

# (vii) Prediction
new_inputs <- cbind(1, c(0.3, 0.4, 0.5), c(20, 21, 22))
predicted_volume_manual <- new_inputs %*% beta_estimates
cat("Predicted volumes (manual matrix):", predicted_volume_manual, "\n")

# (viii) Using lm()
linear_model <- lm(Volume ~ Girth + Height, data = tree_data)
summary(linear_model)

cat("Coefficients from lm():\n")
print(coef(linear_model))

# (ix) Predict using predict()
newdata_df <- data.frame(Girth = c(0.3, 0.4, 0.5), Height = c(20, 21, 22))
predicted_volume_lm <- predict(linear_model, newdata = newdata_df)

cat("Predicted volumes (lm predict()):\n")
print(predicted_volume_lm)


# (VII) Non-linear Regression

# (1) Data
xdata <- c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2,
           2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0)

ydata <- c(-0.09, 0.59, 0.42, 2.03, 3.43, 1.84, 5.30, 4.40, 6.67, 7.40,
           7.34, 8.76, 10.25, 10.93, 13.78, 14.21, 17.82, 21.83, 23.04,
           24.25, 27.48)

data_frame <- data.frame(xdata, ydata)

# (3) Fit model
nonlin_model <- nls(ydata ~ alpha * xdata^pwr, data = data_frame, start = list(alpha = 1, pwr = 2))

# (4) Summary
summary(nonlin_model)

# (5) Degrees of freedom
cat("Degrees of freedom:", df.residual(nonlin_model), "\n")

# (6) Fitted curve
x_sequence <- seq(1, 3, by = 0.1)
alpha_hat <- coef(nonlin_model)["alpha"]
pwr_hat <- coef(nonlin_model)["pwr"]
y_fitted <- alpha_hat * x_sequence^pwr_hat

plot(xdata, ydata, main = "Non-linear Fit", xlab = "x", ylab = "y", pch = 16, col = "blue")
lines(x_sequence, y_fitted, col = "red", lwd = 2)
legend("topleft", legend = c("Data Points", "Fitted Curve"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA,1))


# (VIII) Clustering Methods

# (1) Hierarchical Clustering
library(tidyverse)
library(dplyr)
library(RColorBrewer)

spellman_data <- read.csv("spellman-wide.csv")
dim(spellman_data)

spellman_data[1:5, 1:8]

# Correlation and distance
corr_matrix <- spellman_data %>%
  select(-time, -expt) %>%
  cor(use = "pairwise.complete.obs")

dist_matrix <- as.dist(1 - corr_matrix)

# Clustering
hc_model <- hclust(dist_matrix, method = "complete")
plot(hc_model, cex = 0.1)

# Dendrogram without labels
library(dendextend)
dend_obj <- as.dendrogram(hc_model)
plot(dend_obj, leaflab = "none")

# Cutting into clusters
gene_clusters <- cutree(dend_obj, k = 4)
table(gene_clusters)

gene_clusters[1:6]

# Colored branches
colored_tree <- color_branches(hc_model, k = 4)
plot(colored_tree, leaflab = "none")

colored_tree8 <- color_branches(hc_model, k = 8)
plot(colored_tree8, leaflab = "none")
table(cutree(hc_model, k = 8))

# Gene names and cluster IDs
gene_cluster_df <- data.frame(gene = names(gene_clusters), cluster = gene_clusters)

gene_cluster_df %>% filter(gene == "YALO22C")

# Genes in cluster 3
genes_in_cluster3 <- gene_cluster_df %>%
  filter(cluster == 3) %>%
  pull(gene)

genes_in_cluster3

# Heatmap for cluster
spellman_long <- spellman_data %>%
  gather(gene, expression, -expt, -time)

color_palette <- rev(brewer.pal(8, "RdBu"))

spellman_long %>%
  filter(gene %in% genes_in_cluster3 & expt == "alpha") %>%
  ggplot(aes(x = time, y = gene)) +
  geom_tile(aes(fill = expression)) +
  scale_fill_gradientn(colors = color_palette, limits = c(-2, 2)) +
  theme(axis.text.y = element_text(size = 6))

# (xii) Subtree and heatmap
subtrees_cut <- cut(dend_obj, h = 1.48)
subtree3 <- subtrees_cut$lower[[3]]

subtree3 %>%
  set("labels_cex", 0.45) %>%
  set("labels_col", "red") %>%
  plot(horiz = TRUE)

# Heatmap
library(gplots)
alpha_expt <- filter(spellman_data, expt == "alpha")

alpha_matrix <- alpha_expt %>%
  select(-time, -expt) %>%
  as.matrix()

rownames(alpha_matrix) <- alpha_expt$time
alpha_matrix_t <- t(alpha_matrix)

heatmap.2(alpha_matrix_t,
          Rowv = subtree3,
          Colv = NULL,
          dendrogram = "row",
          breaks = seq(-2, 2, length.out = 9),
          col = color_palette,
          trace = "none",
          density.info = "none",
          xlab = "Time (minutes)")


# (2) K-means Clustering
library(tidyverse)
library(ggplot2)
library(dplyr)

airbnb_data <- read.csv("listings_airbnb.csv")
cat("Rows:", nrow(airbnb_data), "\n")
print(names(airbnb_data))

# Scatter plot
ggplot(airbnb_data, aes(number_of_reviews, price, color = room_type, shape = room_type)) +
  geom_point(alpha = 0.25) +
  xlab("Number of Reviews") +
  ylab("Price")

# Normalize
airbnb_data[, c("price", "number_of_reviews")] <- scale(airbnb_data[, c("price", "number_of_reviews")])

# Subset
airbnb_subset <- airbnb_data[, c("price", "minimum_nights", "number_of_reviews")]
head(airbnb_subset)

# K-means clustering
set.seed(23)
km_result <- kmeans(na.omit(airbnb_subset), centers = 3, nstart = 20)
print(km_result)

# Scree plot
n_clust <- 10
wss_values <- numeric(n_clust)

for (k in 1:n_clust) {
  km_out <- kmeans(na.omit(airbnb_subset), centers = k, nstart = 20)
  wss_values[k] <- km_out$tot.withinss
}

wss_tbl <- tibble(clusters = 1:n_clust, wss = wss_values)

scree_plot <- ggplot(wss_tbl, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab("Number of Clusters")

scree_plot +
  geom_hline(yintercept = wss_values,
             linetype = 'dashed',
             col = c(rep('black', 4), 'red', rep('black', 5)))

# Final clustering
set.seed(23)
final_km <- kmeans(na.omit(airbnb_subset), centers = 5, nstart = 20)

airbnb_clustered <- na.omit(airbnb_subset)
airbnb_clustered$cluster_id <- factor(final_km$cluster)

ggplot(airbnb_clustered, aes(number_of_reviews, price, color = cluster_id)) +
  geom_point(alpha = 0.25) +
  xlab("Number of Reviews") +
  ylab("Price")

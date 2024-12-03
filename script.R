setwd("C:/Users/imed/Desktop/SID/ANAD")
library(dplyr)
pv <- read.csv("tp_pca/data/pv.csv")
sit <- read.csv("tp_pca/data/sit.csv")
siq <- read.csv("tp_pca/data/siq.csv")
sil <- read.csv("tp_pca/data/sil.csv")
sid <- read.csv("tp_pca/data/sid.csv")

all_data <- bind_rows(
  sit %>% select(Matricule, Spécialité),
  siq %>% select(Matricule, Spécialité),
  sil %>% select(Matricule, Spécialité),
  sid %>% select(Matricule, Spécialité)
)
print(all_data)

data <- merge(pv, all_data, by = "Matricule")
print(data)

new_data <- data %>% select(-Groupe_S2, -Ne_S2, -Rang_S2, -Moy_S2)
head(new_data)
pca_data <- new_data %>% select(-Matricule,-Spécialité)
head(pca_data)

library(FactoMineR)

# Step 1: Replace commas with dots
pca_data[] <- lapply(pca_data, function(x) gsub(",", ".", x))

# Step 2: Convert columns to numeric
pca_data[] <- lapply(pca_data, as.numeric)
pca_result <- PCA(pca_data)

principal_components <- as.data.frame(pca_result$ind$coord[, 1:2])
head(principal_components)


vis_data <- cbind(principal_components, Spécialité = new_data$Spécialité)
head(vis_data)

library(ggplot2)
ggplot(vis_data, aes(x = Dim.1, y = Dim.2, color = Spécialité)) +
  geom_point(size = 3) +  # Add points with specified size
  labs(
    title = "PCA Projection by Spécialité",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Spécialité"
  ) +
  theme_minimal() +  # Use a clean theme
  theme(
    legend.position = "right",  # Place legend on the right
    text = element_text(size = 12)  # Adjust text size
  )

columns_to_check <- c("MCSI", "BDD", "SEC", "CPROJ", "PROJ", "LANG2", "ARCH", "SYS2","RES2")

# Step 1: Replace commas with dots only in the specified columns
new_data[columns_to_check] <- lapply(new_data[columns_to_check], function(x) gsub(",", ".", x))

# Step 2: Convert the specified columns to numeric
new_data[columns_to_check] <- lapply(new_data[columns_to_check], as.numeric)
head(new_data)

# Step 3: Define the outlier detection function (using IQR method)
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  x < lower_bound | x > upper_bound
}

# Step 4: Detect and remove rows with outliers
outlier_mask <- apply(new_data[columns_to_check], 1, function(row) {
  any(detect_outliers(row))
})
cleaned_data <- new_data[!outlier_mask, ]
head(cleaned_data)

num_rows <- nrow(cleaned_data)

# Print the number of rows
print(num_rows)

pca_data <- cleaned_data %>% select(-Matricule,-Spécialité)
pca_result <- PCA(pca_data)

principal_components <- as.data.frame(pca_result$ind$coord[, 1:2])
head(principal_components)


vis_data <- cbind(principal_components, Spécialité = cleaned_data$Spécialité)
head(vis_data)

library(ggplot2)
ggplot(vis_data, aes(x = Dim.1, y = Dim.2, color = Spécialité)) +
  geom_point(size = 3) +  # Add points with specified size
  labs(
    title = "PCA Projection by Spécialité",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Spécialité"
  ) +
  theme_minimal() +  # Use a clean theme
  theme(
    legend.position = "right",  # Place legend on the right
    text = element_text(size = 12)  # Adjust text size
  )


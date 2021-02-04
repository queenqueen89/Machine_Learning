#' ---
#' title: "CHAPTER 12: K-MEANS CLUSTERING"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "March 8th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 12 - Copy")

# Install the packages needed for this code file
# install.packages(c("stringr","factoextra","cluster","gridExtra"))

library(tidyverse)

#----------------------------------------------------------------
#' #1. Collect the Data
#----------------------------------------------------------------
# Load and preview the mall customers dataset.
mallcustomers <- read_csv("mallcustomers.csv")
glimpse(mallcustomers)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# We have a couple of things we need to do to prepare the data.
# The first is to convert the Gender feature to a factor.
# The second is to convert the income feature to a number.
# This requires that we remove both the USD and the "," in the data.
library(stringr)
mallcustomers <- mallcustomers %>%
  mutate(Income = str_replace_all(Income," USD","")) %>%
  mutate(Income = str_replace_all(Income,",","")) %>%
  mutate(Income = as.numeric(Income))

summary(mallcustomers)

# We intend to segment based on Income and SpendingScore only.
# So, we remove everything else and normalize our features.
mallcustomers_scaled <- mallcustomers %>%
  select(-CustomerID, -Gender, -Age) %>%
  scale()

# What does the data now look like?
summary(mallcustomers_scaled)

#----------------------------------------------------------------
#' #3. "Train" the Model
#----------------------------------------------------------------
# We are now ready to attempt to cluster the data.
# First, we need to identify the optimal k using the elbow, siklhouette and gap statistic.
library(factoextra)
library(cluster)
library(gridExtra)

# Elbow Method
p1 <- fviz_nbclust(mallcustomers_scaled, kmeans, method = "wss") + geom_point(
  shape = 1,
  x = 6,
  y = 60,
  colour = "red",
  size = 8,
  stroke = 1.5
) + ggtitle("Elbow Method")

# Silhouette Method
p2 <- fviz_nbclust(mallcustomers_scaled, kmeans, method = "silhouette") + geom_point(
  shape = 1,
  x = 6,
  y = 0.53,
  colour = "red",
  size = 8,
  stroke = 1.5
) + ggtitle("Silhouette Method")

# Gap Statistic
p3 <- fviz_nbclust(mallcustomers_scaled, kmeans, method = "gap_stat") + geom_point(
  shape = 1,
  x = 6,
  y = 0.57,
  colour = "red",
  size = 8,
  stroke = 1.5
) + ggtitle("Gap Statistic")

grid.arrange(p1, p2, p3, nrow = 3)

# We set the value for k to 6 and choose to use 25 different initial configurations.
set.seed(1234)
k_clust <- kmeans(mallcustomers_scaled, centers = 6, nstart = 25)

fviz_cluster(
  k_clust,
  data = mallcustomers_scaled,
  main = "Mall Customers Segmented by Income and Spending Score",
  repel = TRUE,
  ggtheme = theme_minimal()
) + theme(text = element_text(size = 14))

#----------------------------------------------------------------
#' #4. "Evaluate" the Model's Performance
#----------------------------------------------------------------
# To further evaluate our results, we need to look at how attributes vary by cluster.
mallcustomers %>%
  mutate(cluster = k_clust$cluster) %>%
  mutate(Male = ifelse(Gender == "Male", 1, 0)) %>%
  mutate(Female = ifelse(Gender == "Female", 1, 0)) %>%
  select(cluster, Male, Female, Age) %>%
  group_by(cluster) %>%
  summarise_all("mean")


source("load.R")

# # Simulate data
# n <- c(1000, 10000)
# p <- c(5, 15)
# k <- c(3, 10)
# settings <- expand.grid(n = n, p = p, k = k)
# 
# sim_data <- gen_mix_gauss(num_clusters = 15, n = 1000, p = 10)
# data0 <- sim_data$data


# Real dataset
data0 <- read.table("cloud.txt")
k <- 10

# k-means with random initialisation
km_rand <- kmeans(x = data0, centers = k)

# k-means++
kpp_centers <- kmeanspp(data0, k = k)
km_pp <- kmeans(x = data0, centers = kpp_centers)

# afk_ms2
afkm_centers <- afk_kms2(data0, k = k, m = 50)
km_afk <- kmeans(x = data0, centers = afkm_centers)

# Compare performance
c(km_rand$tot.withinss, km_pp$tot.withinss, km_afk$tot.withinss) / nrow(data0)

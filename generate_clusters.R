#' Generate clusters using mixture of gaussian distribution
#' @param num_clusters integer; number of clusters.
#' @param n integer; number of data points.
#' @param p integer; dimension of the data.
#' @param mu matrix; num_clusters x p matrix, each row specifies the mean vector.
#' @param sigma list; should have length = num_clusters, each element is a covariance matrix.
gen_mix_gauss <- function(num_clusters = 3, n = 100, p = 2, 
                          mu, sigma) {
  if (missing(mu))
    mu <- matrix(rnorm(num_clusters * p), nrow = num_clusters, ncol = p)
  
  if (missing(sigma))
    sigma <- purrr::map(
      .x = seq(num_clusters), 
      .f = function(i) {
        sigma <- matrix(rnorm(p^2), p, p)
        sigma %*% t(sigma)
      })
  
  if ((nrow(mu) != num_clusters) | (length(sigma) != num_clusters))
    stop("Wrong input for the mean vector or the covariance matrix.")
  
  cluster_counts <- num_clusters %>% 
    sample(n, replace = T) %>%
    table() %>% as.vector()
  
  coord <- purrr::map(
    .x = seq_along(cluster_counts), 
    .f = ~mvtnorm::rmvnorm(cluster_counts[.x], mu[.x,], sigma[[.x]])
  ) %>% 
    do.call(rbind, .)
  
  cluster <- rep(seq(num_clusters), cluster_counts)
  
  list(data = coord, cluster = cluster, mu = mu, sigma = sigma)
}
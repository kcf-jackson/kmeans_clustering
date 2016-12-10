#' Initialise centers for kmeans cluster.
#' @param data0 matrix / dataframe; the data.
#' @param k; integer; number of clusters.
#' @param m integer; length of chain.
#' @return matrix; the centers of clusters.
afk_kms2 <- function(data0, k, m) {
  if (k <= 1) stop("The number of clusters should at least be two.")
  data0 %<>% data.frame()
  n <- nrow(data0)
  
  centers <- data0 %>% dplyr::sample_n(1)
  dist_vec <- data0 %>% compute_sqdist(centers)
  q <- 0.5 * dist_vec / sum(dist_vec) + 1/(2*n)
  
  for (i in 2:k) {
    sampled_x_index <- sample(n, 1, prob = q)
    x <- data0[sampled_x_index, ]
    d_x <- compute_sqdist(data0[sampled_x_index, ], centers)
    for (j in 2:m) {
      sampled_y_index <- sample(n, 1, prob = q)
      y <- data0[sampled_y_index, ]
      d_y <- compute_sqdist(data0[sampled_y_index, ], centers)
      if ((d_y * q[sampled_x_index]) / (d_x * q[sampled_y_index]) > runif(1)) {
        x <- y
        d_x <- d_y
      }
    }
    centers %<>% rbind(x)
  }
  
  centers
}


compute_sqdist <- function(mat0, mat1, dist = l2_distance) {
  row_seq <- seq(nrow(mat0))
  purrr::map_dbl(
    .x = row_seq, 
    .f = ~mat0[.x, ] %>% fun_vec_and_mat(mat1, dist) %>% `^`(2) %>% min()
  )
}

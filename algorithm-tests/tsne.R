

library(tidyverse)
# library(rgl)
# library(Rsolnp)

n <- 60
noise_sd <- 1
d <- tibble(
  x = rep(1:4, each = n/4) + rnorm(n, 0, noise_sd),
  y = rep(c(1,-2,3,-4), each = n/4) + rnorm(n, 0, noise_sd),
  z = rep(c(-1,-2,3,4), each = n/4) + rnorm(n, 0, noise_sd)
) %>%
  as.matrix
categ <- rep(1:4, each = n/4)

hd_sim_ij <- function(xi, xj, sigma) {
  exp(-(xi-xj) %*% (xi-xj) / (2 * sigma^2))
}

hd_prob_ij <- function(df, i, j, sims, sigma = 1) {
  N <- nrow(df)
  df <- as.matrix(df)
  xi <- df[i, ]
  xj <- df[j, ]
  sum_sim_ik <- sum(sims[i, -i])
  pj_i <- hd_sim_ij(xi, xj, sigma) / sum_sim_ik
  sum_sim_jk <- sum(sims[j, -j])
  pi_j <- hd_sim_ij(xi, xj, sigma) / sum_sim_jk
  (pj_i + pi_j) / (2 * N)
}

hd_calc_prob <- function(df, sigma = 1){
  N <- nrow(df)
  sims <- matrix(0, nrow = N, ncol = N)
  for (i in 1:N) {
    for (j in 1:N) {
      sims[i,j] <- hd_sim_ij(df[i, ], df[j, ], sigma)
    }
  }
  prob <- matrix(0, nrow = N, ncol = N)
  for (i in 1:N) {
    for (j in 1:N) {
      if (i != j)
        prob[i,j] <- hd_prob_ij(df, i, j, sims, sigma)
    }
  }
  prob
}

ld_sim_ij <- function(yi, yj){
  ydiff <- yi - yj
  1 / (1 + ydiff %*% ydiff)
}
ld_sim_ij <- compiler::cmpfun(ld_sim_ij)

ld_calc_sim <- function(df) {
  N <- nrow(df)
  # sims <- sapply(1:N, function(i){
  #   sapply(1:N, function(j){
  #     ld_sim_ij(df[i, ], df[j, ])
  #   })
  # })
  sims <- matrix(0, nrow = N, ncol = N)
  for (i in 1:N) {
    xi <- df[i, ]
    sims[i, ] <- sapply(1:N, function(j) ld_sim_ij(xi, df[j, ]))
    # for (j in 1:N) {
    #   sims[i,j] <- ld_sim_ij(df[i, ], df[j, ])
    # }
  }
  sims
}

ld_calc_prob <- function(df){
  N <- nrow(df)
  prob <- ld_calc_sim(df)
  diag(prob) <- 0
  prob / sum(prob)
}

x <- ld_calc_prob(d)
# diag(x) <- 0
sum(x)

x <- hd_calc_prob(d)
# diag(x) <- 0
sum(x)


KLdiv <- function(p, q, eps = 1e-4) {
  if (any(q < 0) || any(p < 0))
    stop('All elements must be non-negative')
  p[p == 0] <- eps
  q[q == 0] <- eps
  sum(p * (log(p) - log(q)))
}

cost_fun_gen <- function(df, ld_ndim = 2, sigma = 1, eps = 1e-4) {
  p <- hd_calc_prob(df, sigma = sigma)
  function(y) {
    y <- matrix(y, nrow = nrow(df), ncol = ld_ndim)
    q <- ld_calc_prob(y)
    KLdiv(p, q, eps = eps)
  }
}

cf_grad_gen <- function(df, ld_ndim = 2) {
  N <- nrow(df)
  p <- hd_calc_prob(df, sigma = sigma)
  function(y) {
    y <- matrix(y, nrow = N, ncol = ld_ndim)
    q <- ld_calc_prob(y)
    g <- matrix(0, nrow = N, ncol = ncol(y))
    for (i in 1:nrow(g)) {
      for (j in 1:N) {
        g[i, ] <- g[i, ] + (p[i,j] - q[i,j]) * (y[i, ] - y[j, ]) * ld_sim_ij(y[i, ], y[j, ])
      }
    }
    4 * as.vector(g)
  }
}

grad <- function(fun, eps = 0.001) {
  gg <- function(x) {
    fx <- fun(x)
    sapply(seq_along(x), function(i){
      y <- x
      y[i] <- y[i] + eps
      (fun(y) - fx) / eps
    })
  }
  compiler::cmpfun(gg)
}

gd <- function(par, fun, step = 0.1, gr_tol = 1e-5, maxit = 1000, verbose = T) {
  gr <- grad(fun)
  g <- gr(par)
  g_new <- g
  for (k in 1:maxit) {
    g_norm <- sqrt(g %*% g)
    if (g_norm < gr_tol)
      break
    if (verbose){
      cat(sprintf('k: %d\tf: %.3f\t||g||: %.3f\n', k, fun(par), g_norm))
      # print(k)
    }
    g <- g_new
    par <- par - step * g
    g_new <- gr(par)
  }
  list(
    par = par,
    grad = g,
    step = step,
    gr_tol = gr_tol,
    maxit = maxit
  )
}

#############################

ld_ndim <- 2
sigma <- 1

set.seed(1234)
theta <- rnorm(ld_ndim * nrow(d))
cf <- cost_fun_gen(d, ld_ndim = ld_ndim, sigma = sigma)
cfg <- cf_grad_gen(d, ld_ndim = ld_ndim)
# cf(theta)
# cfg(theta)
# grad(cf)(theta)
# cfg(theta) - grad(cf)(theta)
# opt <- optim(theta, cf, method = 'BFGS', control = list(trace = 1, maxit = 10, REPORT = 1))
# opt <- optim(theta, cf, grad(cf), method = 'BFGS', control = list(trace = 1, maxit = 100, REPORT = 1))
system.time(
  opt <- optim(theta, cf, cfg, method = 'BFGS', control = list(trace = 1, maxit = 300, REPORT = 1))
)
# opt <- gd(theta, cf, step = 1, maxit = 20, verbose = T)

# opt <- solnp(theta, cf, control = list(trace = 5))

# plot3d(d[,1], d[,2], d[,3], col=categ)

matrix(opt$par, nrow = nrow(d), ncol = ld_ndim) %>%
  as_tibble %>%
  mutate(categ = factor(categ)) %>%
  ggplot(aes(V1, V2, col = categ)) +
  geom_point(size = 5) +
  viridis::scale_color_viridis(discrete = T)


##############################
# Now a real package

library(tsne)

system.time(
  opt <- tsne(d, k = ld_ndim)
)

opt %>%
  as_tibble %>%
  mutate(categ = factor(categ)) %>%
  ggplot(aes(V1, V2, col = categ)) +
  geom_point(size = 5) +
  viridis::scale_color_viridis(discrete = T)


library("microbenchmark")
library("profvis")
library("ggplot2")
library(magrittr)

# ------------------------------------------------------------------------
# ch 1
# ------------------------------------------------------------------------

df = data.frame(v = 1:4, name = letters[1:4])
microbenchmark(df[3, 2], df[3, "name"], df$name[3], times = 1) %>%
  autoplot()

profvis(
  expr = {

    # Stage 1: load packages
    # library("rnoaa") # not necessary as data pre-saved
    library("ggplot2")

    # Stage 2: load and process data
    out = readRDS("extdata/out-ice.Rds")
    df = dplyr::rbind_all(out, id = "Year")

    # Stage 3: visualise output
    ggplot(df, aes(long, lat, group = paste(group, Year))) +
      geom_path(aes(colour = Year))
    ggsave("figures/icesheet-test.png")
  },
  interval = 0.01, prof_output = "ice-prof"
)


# ------------------------------------------------------------------------
# ch 2
# ------------------------------------------------------------------------

# 1: Create large dataset
X = as.data.frame(matrix(rnorm(1e8), nrow = 1e7))
# 2: Find the median of each column using a single core
r1 = lapply(X, median)
# 3: Find the median of each column using many cores
r2 = parallel::mclapply(X, median)


# ------------------------------------------------------------------------
# ch 3
# ------------------------------------------------------------------------


monte_carlo = function(N) {
  hits = 0
  for (i in seq_len(N)) {
    u1 = runif(1)
    u2 = runif(1)
    if (u1 ^ 2 > u2)
      hits = hits + 1
  }
  return(hits / N)
}
monte_carlo_vec = function(N) sum(runif(N)^2 > runif(N))/N

microbenchmark(monte_carlo(1000), monte_carlo_vec(1000)) %>%
  autoplot()


# invisible
regression_plot = function(x, y, ...) {
  # Plot and pass additional arguments to default plot method
  plot(x, y, ...)

  # Fit regression model
  model = lm(y ~ x)

  # Add line of best fit to the plot
  abline(model)
  invisible(model)
}

regression_plot(1:10,1:10)
x = regression_plot(1:10,1:10)
x

hist(1:10)
x=hist(1:10)
x

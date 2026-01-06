correlation_threshold <- function(N, alpha = 0.01, two_tailed = TRUE) {
  df <- N - 2
  if (two_tailed) {
    t_critical <- qt(1 - alpha/2, df)
  } else {
    t_critical <- qt(1 - alpha, df)
  }
  r_critical <- t_critical / sqrt(t_critical^2 + df)
  return(r_critical)
}

correlation_threshold(66,two_tailed = FALSE,alpha = 0.05)

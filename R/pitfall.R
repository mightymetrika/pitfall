pitfall <- function(d, p, arglist, method = "difference") {

  # Compute both distributions
  dout <- do.call(d, args = arglist)
  pout <- do.call(p, args = arglist)

  # Compare distributions based on selected method
  comparison <- switch(method,
                       "difference" = dout - pout,
                       "ratio" = dout / pout,
                       "kl" = {
                         # Kullback-Leibler divergence
                         # Need to ensure distributions are normalized
                         dout_norm <- dout / sum(dout)
                         pout_norm <- pout / sum(pout)
                         sum(dout_norm * log(dout_norm / pout_norm), na.rm = TRUE)
                       },
                       "wasserstein" = {
                         # Simple 1D Wasserstein distance implementation
                         # Assuming equally spaced points
                         abs(cumsum(dout) - cumsum(pout))
                       }
  )

  return(list(
    correct = dout,
    pitfall = pout,
    comparison = comparison,
    method = method
  ))
}

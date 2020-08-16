esr_backtest2 <- function(r, q, e, alpha, version, B = 0) {
  if (missing(q) & version %in% c(2)) {
    stop('You need to supply VaR forecast `q` for backtest version ', version)
  }
  
  if (missing(q)) {
    data <- data.frame(r = r, e = e)
  } else {
    data <- data.frame(r = r, q = q, e = e)
  }
  
  # Set the details for the selected version of the backtest
  if (version == 1) {
    model <- r ~ e
    h0 <- c(NA, NA, 0, 1)
    one_sided <- FALSE
  } else if (version == 2) {
    model <- r ~ q | e
    h0 <- c(NA, NA, 0, 1)
    one_sided <- FALSE
  } else if (version == 3) {
    model <- I(r - e) ~ e | 1
    h0 <- c(NA, NA, 0)
    one_sided <- TRUE
  } else {
    stop('This is a non-supported backtest version!')
  }
  
  # Fit the model
  fit0 <- esreg::esreg(model, data = data, alpha = alpha, g1 = 2, g2 = 1)
  cov0 <- esreg::vcovA(fit0)
  s0 <- fit0$coefficients - h0
  mask <- !is.na(h0)
  
  # Compute the asymptotic test statistic and p-value
  if (version %in% c(1, 2)) {
    t0 <- as.numeric(s0[mask] %*% solve(cov0[mask, mask]) %*% s0[mask])
    pv0_1s <- NA
    pv0_2s <- 1 - stats::pchisq(t0, sum(mask))
  } else if (version %in% c(3)) {
    t0 <- as.numeric(s0[mask] / sqrt(cov0[mask, mask]))
    pv0_1s <- stats::pnorm(t0)
    pv0_2s <- 2 * (1 - stats::pnorm(abs(t0)))
  }
  
  # Compute the bootstrap p-values
  if (B > 0) {
    n <- length(r)
    idx <- matrix(sample(1:n, n*B, replace=TRUE), nrow=n)
    bs_estimates <- apply(idx, 2, function(id) {
      tryCatch({
        fitb <- esreg::esreg(model, data = data[id,], alpha = alpha, g1 = 2, g2 = 1, early_stopping = 0)
        sb <- fitb$coefficients - fit0$coefficients
        covb <- esreg::vcovA(fitb,
                             sparsity = cov_config$sparsity,
                             sigma_est = cov_config$sigma_est,
                             misspec = cov_config$misspec)
        list(sb = sb, covb = covb)
      }, error=function(e) NA)
    })
    idx_na <- is.na(bs_estimates)
    share_na <- mean(idx_na)
    if (share_na >= 0.05) stop('More than 5% of the bootstrap replications failed!')
    
    bs_estimates <- bs_estimates[!idx_na]
    
    if (version %in% c(1, 2)) {
      tb <- sapply(bs_estimates, function(x) {
        as.numeric(x$sb[mask] %*% solve(x$covb[mask, mask]) %*% x$sb[mask])
      })
      tb <- tb[!is.na(tb)]
      pvb_2s <- mean(tb >= t0)
      pvb_1s <- NA
    } else if (version %in% c(3)) {
      tb <- sapply(bs_estimates, function(x) {
        x$sb[mask] / sqrt(x$covb[mask, mask])
      })
      tb <- tb[!is.na(tb)]
      pvb_2s <- mean(abs(t0) <= abs(tb))
      pvb_1s <- mean(tb <= t0)
    }
  } else {
    pvb_2s <- NA
    pvb_1s <- NA
  }
  
  # Return results
  ret <- list(
    pvalue_twosided_asymptotic = pv0_2s,
    pvalue_twosided_bootstrap = pvb_2s
  )
  
  if (version %in% c(3)) {
    ret['pvalue_onesided_asymptotic'] <- pv0_1s
    ret['pvalue_onesided_bootstrap'] <- pvb_1s
  }
  
  ret
}
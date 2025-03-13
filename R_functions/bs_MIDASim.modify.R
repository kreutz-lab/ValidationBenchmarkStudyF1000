# Bug fix of original function in MIDASim package

bs_MIDASim.modify <- function (fitted, lib.size = NULL, mean.rel.abund = NULL, gengamma.mu = NULL, 
          sample.1.prop = NULL, taxa.1.prop = NULL, ...) 
{
  if (fitted$mode == "parametric") {
    arg <- as.numeric(!c(is.null(lib.size), is.null(mean.rel.abund), 
                         is.null(gengamma.mu), is.null(sample.1.prop), is.null(taxa.1.prop)))
    fitted$n.sample <- n.sample <- ifelse(arg[1] == 0, fitted$n.sample, 
                                          length(lib.size))
    n.taxa <- fitted$n.taxa
    stopifnot(`Specify either mean.rel.abund or gengamma.mu, not both. ` = (sum(arg[2:3]) < 
                                                                              2))
    fitted$mu.est = -fitted$mu.est
    if (arg[2] == 1) {
      fitted$mean.rel.abund = mean.rel.abund
      fitted$mu.est = mu.of.sigma.Q(sigma = fitted$sigma.est, 
                                    Q = fitted$Q.est, m1 = fitted$mean.rel.abund)
    }
    else if (arg[3] == 1) {
      gengamma.mu = -gengamma.mu
      est = est.mu(gengamma.mu, fitted$sigma.est, fitted$Q.est)
      fitted$mu.est = gengamma.mu + log(sum(est))
    }
    if (arg[1] == 1) {
      fitted$lib.size <- lib.size
      fitted$n.sample <- length(lib.size)
    }
    fitted$prob01.mat = get.prob01.mat(fitted$mu.est, fitted$sigma.est, 
                                       fitted$Q.est, fitted$lib.size)
    sample.1 = rowSums(fitted$prob01.mat)
    taxa.1 = colSums(fitted$prob01.mat)
    fitted$sample.1.prop = sample.1/n.taxa
    fitted$taxa.1.prop = taxa.1/n.sample
    Ztotal = sum(fitted$prob01.mat)
    fitted$mu.est = -fitted$mu.est
  }
  else {
    stopifnot(`The argument gengamma.mu is not applicable in nonparametric mode ` = (is.null(gengamma.mu)))
    dots <- list(...)
    if ("SCAM" %in% names(dots) && dots$SCAM == T) {
      mean.rel.abund.1 = dots$mean.rel.abund.1
      arg <- as.numeric(!c(is.null(lib.size), is.null(mean.rel.abund), 
                           is.null(mean.rel.abund.1), is.null(taxa.1.prop)))
      obs.n.sample <- fitted$n.sample
      fitted$n.sample <- n.sample <- ifelse(arg[1] == 0, 
                                            fitted$n.sample, length(lib.size))
      n.taxa <- fitted$n.taxa
      obs.taxa.1.prop <- fitted$taxa.1.prop
      obs.lib.size <- fitted$lib.size
      obs.mean.rel.abund <- fitted$mean.rel.abund
      obs.sample.1.prop <- fitted$sample.1.prop
      obs.sample.1.ct <- obs.sample.1.prop * n.taxa
      stopifnot(`Only providing mean relative abundances among non-zero samples is not allowed` = (!identical(arg[2:4], 
                                                                                                              c(0, 1, 0))))
      if (arg[2] == 1 & arg[4] == 1) {
        stopifnot(`mean relative abundances must be greater or equal to proportion of non-zero cells` = (sum(mean.rel.abund > 
                                                                                                               taxa.1.prop) == 0))
      }
      if (arg[2] == 1 & arg[3] == 1) {
        stopifnot(`mean relative abundances must be smaller or equal to mean relative abundances among non-zero samples` = (sum(mean.rel.abund > 
                                                                                                                                  mean.rel.abund.1) == 0))
      }
      if (sum(arg[2:4]) == 3) {
        stopifnot(`mean relative abundances must be equal to the product of mean relative abundances among non-zero samples and proportion of non-zero cells` = (identical(mean.rel.abund, 
                                                                                                                                                                           mean.rel.abund.1 * taxa.1.prop)))
      }
      stopifnot(`length of specified relative abundance-like quantities does not align with the original data` = all(c(length(mean.rel.abund), 
                                                                                                                       length(mean.rel.abund.1), length(taxa.1.prop)) %in% 
                                                                                                                       c(0, n.taxa)))
      if (arg[1] == 1) {
        fitted$lib.size <- lib.size
        xvar <- log10(obs.lib.size)
        scamfit.non0 <- scam::scam(log10(obs.sample.1.ct) ~ 
                                     s(xvar, bs = "mpi"))
        sample.1.ct <- 10^(predict(scamfit.non0, newdata = data.frame(xvar = log10(lib.size))))
        sample.1.ct <- pmin(sample.1.ct, n.taxa)
        fitted$sample.1.prop <- sample.1.ct/n.taxa
      }
      if (sum(arg[2:4]) > 0) {
        if (arg[4] == 1) {
          fitted$taxa.1.prop <- taxa.1.prop
          if (arg[2] == 1) {
            fitted$mean.rel.abund <- mean.rel.abund
            mean.rel.abund.1 <- mean.rel.abund/taxa.1.prop
          }
          else if (arg[3] == 0) {
            xvar <- log10(obs.taxa.1.prop)
            suppressWarnings(scamfit.rel <- scam::scam(obs.mean.rel.abund ~ 
                                                         s(xvar, bs = "mpi"), family = "binomial"))
            fitted$mean.rel.abund <- predict(scamfit.rel, 
                                             newdata = data.frame(xvar = log10(taxa.1.prop)), 
                                             type = "response")
            mean.rel.abund.1 <- fitted$mean.rel.abund/taxa.1.prop
          }
        }
        else if (arg[3] == 1) {
          fitted$taxa.1.prop <- mean.rel.abund/mean.rel.abund.1
          fitted$mean.rel.abund <- mean.rel.abund
        }
        else {
          fitted$mean.rel.abund <- mean.rel.abund
          xvar <- log10(obs.mean.rel.abund)
          yvar <- obs.taxa.1.prop
          suppressWarnings(scamfit.prop.1 <- scam::scam(yvar ~ 
                                                          s(xvar, bs = "mpi"), family = "binomial"))
          fitted$taxa.1.prop <- predict(scamfit.prop.1, 
                                        newdata = data.frame(xvar = log10(mean.rel.abund)), 
                                        type = "response")
          mean.rel.abund.1 <- mean.rel.abund/fitted$taxa.1.prop
        }
        for (j in 1:n.taxa) {
          equa <- function(x) mean(fitted$rel.abund.1[[j]]^x) - 
            mean.rel.abund.1[j]
          a <- pracma::fzero(fun = equa, x = c(-0.1, 
                                               1000))$x
          fitted$rel.abund.1[[j]] <- fitted$rel.abund.1[[j]]^a
        }
      }
      if (arg[1] == 1 && sum(arg[2:4]) == 1) {
        fitted$lib.size <- lib.size
        x1 <- log10(rep(obs.lib.size, n.taxa))
        x2 <- rep(obs.mean.rel.abund, each = obs.n.sample)
        pred.x2 <- rep(fitted$mean.rel.abund, each = n.sample)
        y <- as.vector(fitted$mat01)
        scamfit.0 <- scam::scam(y ~ s(x1) + s(x2), family = "binomial", 
                                optimizer = "efs")
        Ztotal <- sum(predict(scamfit.0, data.frame(x1 = log10(rep(fitted$lib.size, 
                                                                   n.taxa)), x2 = pred.x2), type = "response"))
      }
      else if (arg[1] == 1 && sum(arg[2:4]) == 0) {
        Ztotal = sum(sample.1.ct)
      }
      else {
        Ztotal = sum(fitted$taxa.1.prop) * n.sample
      }
    }
    else {
      arg <- as.numeric(!c(is.null(lib.size), is.null(mean.rel.abund), 
                           is.null(sample.1.prop), is.null(taxa.1.prop)))
      fitted$n.sample <- n.sample <- ifelse(arg[1] == 0, 
                                            fitted$n.sample, length(lib.size))
      n.taxa <- fitted$n.taxa
      if (arg[1] == 1) {
        stopifnot(`If library sizes are changed in nonparametric mode, both 'sample.1.prop' and 'taxa.1.prop' arguments must be provided. ` = (sum(arg[3:4]) == 
                                                                                                                                                 2))
        fitted$lib.size = lib.size
      }
      if (arg[2] == 1) 
        fitted$mean.rel.abund = mean.rel.abund
      if (arg[3] == 1) 
        fitted$sample.1.prop = sample.1.prop
      if (arg[4] == 1) 
        fitted$taxa.1.prop = taxa.1.prop
      stopifnot(`Ensure that 'sample.1.prop' is synchronized with 'taxa.1.prop' with respect to the total number of non-zero entries across the entire table, meaning that the product of 'sample.1.prop' and 'n.taxa' should equal the product of 'taxa.1.prop' and 'n.sample'. ` = (all.equal(sum(fitted$taxa.1.prop) * 
                                                                                                                                                                                                                                                                                                  fitted$n.sample, sum(fitted$sample.1.prop) * 
                                                                                                                                                                                                                                                                                                  fitted$n.taxa)))
      mean.rel.abund.1 = sapply(fitted$rel.abund.1, mean)
      if (all.equal(fitted$mean.rel.abund, mean.rel.abund.1 * 
                    fitted$taxa.1.prop) != TRUE) {
        mean.rel.abund.1 = fitted$mean.rel.abund/fitted$taxa.1.prop
        for (j in 1:n.taxa) {
          equa <- function(x) mean(fitted$rel.abund.1[[j]]^x) - 
            mean.rel.abund.1[j]
          a <- 1 # ck
          try(a <- pracma::fzero(fun = equa, x = c(-0.1, 1000))$x, silent = T) # CK
          fitted$rel.abund.1[[j]] <- fitted$rel.abund.1[[j]]^a
        }
      }
      Ztotal = sum(fitted$taxa.1.prop,na.rm=T) * n.sample
    }
  }
  tmp <- MIDASim:::check_taxa(taxa.1.prop = fitted$taxa.1.prop, n.sample = n.sample, 
                    Ztotal = Ztotal)
  fitted$one.id <- tmp$one.id
  fitted$zero.id <- tmp$zero.id
  Ztotal <- tmp$Ztotal
  if (length(fitted$one.id) + length(fitted$zero.id) > 0) {
    ids.left <- (1:n.taxa)[-union(fitted$one.id, fitted$zero.id)]
    n.rm <- length(fitted$one.id)
    fitted$theta <- qnorm(fitted$taxa.1.prop[ids.left])
    fitted$ids.left <- ids.left
  }
  else {
    n.rm <- 0
    fitted$theta <- qnorm(fitted$taxa.1.prop)
    fitted$ids.left <- (1:n.taxa)
  }
  tmp = MIDASim:::solver_theta_eta(theta0 = fitted$theta, eta0 = rep(0, 
                                                           n.sample), Ztotal = Ztotal, sample.1.prop = fitted$sample.1.prop, 
                         taxa.1.prop = fitted$taxa.1.prop, ids.left = fitted$ids.left, 
                         n.sample = n.sample, n.rm = n.rm)
  fitted$theta <- tmp[["theta0"]]
  fitted$eta <- tmp[["eta0"]]
  return(fitted)
}
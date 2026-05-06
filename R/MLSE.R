# MLSE.R

pcm_MLSE <- function(ps, mu, sigma, pi, alpha, tails
                     # small_p_bin_cutoff = pcm_env$small_p_bin_cutoff,
                     # MLSEh = pcm_env$MLSEh,
                     # small_rcond = pcm_env$small_rcond
                     ) {
  # Adapted from MLSE in MATLAB Cupid dGeneric

  # Use Fisher Information to estimate the standard errors & covariance matrix
  #  of the current parameter estimates with respect to the values in ps.
  # This function should be called after optim(ps) has been used to get ML
  #  estimates for the parameters based on the values in ps.
  # Notes:
  #   SEs may have twice as many terms as expected if some solutions have imaginary components.

  Shift2 <- function(ParmLoc, ShiftVal) {
    # Recompute LnLikelihood of ps with shifted values of two parameters.
    # The numbers of the 2 to-be-shifted parameters are in ParmLoc,
    # and the 2 shifts are in ShiftVal.
    NewParms <- OrigParms;
    NewParms[ParmLoc[1]] <- NewParms[ParmLoc[1]] + ShiftVal[1];
    NewParms[ParmLoc[2]] <- NewParms[ParmLoc[2]] + ShiftVal[2];
    FNew <- -nll(ps, NewParms[1], NewParms[2], pi = NewParms[3], alpha = alpha, tails = tails)
                 # small_p_bin_cutoff = pcm_env$small_p_bin_cutoff);
    return(FNew)
  } # FNew

  MLSEh <- pcm_env$MLSEh
  NParms <- 3 # mu, sigma, pi
  OrigParms <- c(mu, sigma, pi)
  # Compute Ln likelihood at the original parameters:
  F0 <- -nll(ps, mu, sigma, pi = pi, alpha = alpha, tails = tails)
             # small_p_bin_cutoff = small_p_bin_cutoff)
  # Allocate space for the k*k Fisher information matrix.
  FI <- matrix(0, nrow = NParms, ncol = NParms)
  for (iParm in 1:NParms) {
    UpParms <- OrigParms
    UpParms[iParm] <- OrigParms[iParm] + MLSEh
    Fup <- -nll(ps, UpParms[1], UpParms[2], pi = UpParms[3], alpha = alpha, tails = tails)
                # small_p_bin_cutoff = small_p_bin_cutoff)
    DownParms <- OrigParms
    DownParms[iParm] <- OrigParms[iParm] - MLSEh
    Fdown <- -nll(ps, DownParms[1], DownParms[2], pi = DownParms[3], alpha = alpha, tails = tails)
                  # small_p_bin_cutoff = small_p_bin_cutoff)
    FI[iParm,iParm] <- -(Fup - 2*F0 + Fdown) / MLSEh^2

    for (jParm in (iParm+1):NParms) {
      if ( (jParm <= NParms) && (jParm != iParm) ) {
        Fup2    <- Shift2(c(iParm, jParm),c( MLSEh,  MLSEh) )
        Fdown2  <- Shift2(c(iParm, jParm),c(-MLSEh, -MLSEh) )
        Fupdown <- Shift2(c(iParm, jParm),c( MLSEh, -MLSEh) )
        Fdownup <- Shift2(c(iParm, jParm),c(-MLSEh,  MLSEh) )
        FI[iParm,jParm] <- -(Fup2 + Fdown2 - Fupdown - Fdownup ) / (4*MLSEh^2)
        FI[jParm,iParm] <- FI[iParm,jParm]  # Copy upper triangular into lower triangular.
      }
    } # for jParm
  } # for iParm
  # Set up output matrices in case try block fails
  Cov <- matrix(NaN, nrow = NParms, ncol = NParms)
  SE <- rep(NaN, NParms)
  # rcond estimates the reciprocal condition number of a matrix in the 1-norm, measuring its nearness to singularity. It returns a scalar between \(0\) and \(1.0\), where a value near \(0\) indicates a badly conditioned (nearly singular) matrix, and a value near \(1.0\) indicates a well-conditioned matrix
  thisrcond <- Matrix::rcond(FI)
  if (thisrcond < pcm_env$small_rcond) {
    warning( paste0("Nearly singular information matrix (i.e., rcond <= ",small_rcond,")") )
  }
  # This may generate a warning, because FI may not be invertible.
  FI  # NEWJEFF
  CovReal <- solve(FI);  # Covariance is the inverse of the information matrix.
  SEReal <- sqrt(diag(CovReal));  # WAS TRANSPOSE in MATLAB, not needed in R
  # SEReal <- t(SEReal)  # transpose
  for (iParm in 1:NParms) {
    SE[iParm] <- SEReal[iParm]
    for (jParm in iParm:NParms) {
      Cov[iParm,jParm] <- CovReal[iParm,jParm]
      Cov[jParm,iParm] <- CovReal[iParm,jParm]
    } # for jParm
  } # for iParm
  return( list(SE = SE, Cov = Cov) )

} # function MLSE

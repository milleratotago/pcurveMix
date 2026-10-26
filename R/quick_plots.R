# quick_plots.R

get_p_seq <- function(alpha) {
  ps <- seq(from = 0.001, to = alpha, length.out = 100) # Magic constant
  return(ps)
}

#' Make a simple plot of observed p histogram and
#'   predicted pdf density
#' @param fit List output of fit_p_curve function
#' @param p_seq Sequence of p values at which to compute predictions
#'  (default NULL)
#' @returns Figure object made by ggplot
#' @export
make_pdf_plot <- function(fit, p_seq = NULL) {
  if ( is.null(p_seq) ) p_seq <- get_p_seq(fit$alpha)
  ps <- fit$check_ps_list$ps_in_bounds
  pred_pdfs <- pdf(p_seq, mu = fit$mu, sigma = fit$sigma, pi = fit$pi,
                   alpha = fit$alpha) # _cutoff)  # compute predicted
  pdf_df <- data.frame(p = ps)
  pred_pdfs_df <- data.frame(p = p_seq, pdf = pred_pdfs)
  pdf_plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = ps, y = ggplot2::after_stat(density)), binwidth = 0.02) +
    ggplot2::geom_line(ggplot2::aes(x = p_seq, y = pred_pdfs), color = "red") +
    ggplot2::labs(title = "Observed (black) vs predicted (red) PDFs",
         x = "p value",
         y = "density")
  return( list(pred_pdfs = pred_pdfs_df, pdf_plot = pdf_plot) )
}

#' Make a simple plot of observed p cumulative histogram and
#'   predicted cdf
#' @inheritParams make_pdf_plot
#' @returns Figure object made by ggplot
#' @importFrom rlang .data
#' @export
make_cdf_plot <- function(fit, p_seq = NULL) {
  if ( is.null(p_seq) ) p_seq <- get_p_seq(fit$alpha)
  ps <- fit$check_ps_list$ps_in_bounds
  pred_cdfs <- cdf(p_seq, mu = fit$mu, sigma = fit$sigma, pi = fit$pi,
                   alpha = fit$alpha) # _cutoff)  # compute predicted
  cdf_df <- data.frame(p = ps)
  pred_cdfs_df <- data.frame(p = p_seq, cdf = pred_cdfs)
  cdf_plot <- ggplot2::ggplot() +
    ggplot2::stat_ecdf(data = cdf_df, ggplot2::aes(x = .data$p), geom = "step") +  # empirical cdf
    ggplot2::geom_line(ggplot2::aes(x = p_seq, y = pred_cdfs), color = "red") +  # predicted cdf
    ggplot2::labs(title = "Observed (black) vs predicted (red) CDFs",
         x = "p value",
         y = "cumulative proportion")
  return( list(pred_cdfs = pred_cdfs_df, cdf_plot = cdf_plot) )
}

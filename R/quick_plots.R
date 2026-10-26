# quick_plots.R

get_p_seq <- function(alpha) {
  ps <- seq(from = 0.001, to = alpha, length.out = 100) # Magic constant
  return(ps)
}

#' Make a simple plot of observed p histogram and
#'   predicted pdf density
#' @inheritParams pcurveMix-package
#' @returns Figure object made by ggplot
#' @export
make_pdf_plot <- function(fit_results_list, p_seq_pdf = NULL) {
  if ( is.null(p_seq_pdf) ) p_seq_pdf <- get_p_seq(fit_results_list$alpha)
  ps <- fit_results_list$check_ps_list$ps_in_bounds
  pred_pdfs <- pdf(p_seq_pdf, mu = fit_results_list$mu, sigma = fit_results_list$sigma, pi = fit_results_list$pi,
                   alpha = fit_results_list$alpha) # _cutoff)  # compute predicted
  pdf_df <- data.frame(p = ps)
  pred_pdfs_df <- data.frame(p = p_seq_pdf, pdf = pred_pdfs)
  pdf_plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = ps, y = ggplot2::after_stat(density)), binwidth = 0.02) +
    ggplot2::geom_line(ggplot2::aes(x = p_seq_pdf, y = pred_pdfs), color = "red") +
    ggplot2::labs(title = "Observed (black) vs predicted (red) PDFs",
         x = "p value",
         y = "density")
  return( list(pred_pdfs = pred_pdfs_df, pdf_plot = pdf_plot) )
}

#' Make a simple plot of observed p cumulative histogram and
#'   predicted cdf
#' @inheritParams pcurveMix-package
#' @returns Figure object made by ggplot
#' @importFrom rlang .data
#' @export
make_cdf_plot <- function(fit_results_list, p_seq_cdf = NULL) {
  if ( is.null(p_seq_cdf) ) p_seq_cdf <- get_p_seq(fit_results_list$alpha)
  ps <- fit_results_list$check_ps_list$ps_in_bounds
  pred_cdfs <- cdf(p_seq_cdf, mu = fit_results_list$mu, sigma = fit_results_list$sigma, pi = fit_results_list$pi,
                   alpha = fit_results_list$alpha) # _cutoff)  # compute predicted
  cdf_df <- data.frame(p = ps)
  pred_cdfs_df <- data.frame(p = p_seq_cdf, cdf = pred_cdfs)
  cdf_plot <- ggplot2::ggplot() +
    ggplot2::stat_ecdf(data = cdf_df, ggplot2::aes(x = .data$p), geom = "step") +  # empirical cdf
    ggplot2::geom_line(ggplot2::aes(x = p_seq_cdf, y = pred_cdfs), color = "red") +  # predicted cdf
    ggplot2::labs(title = "Observed (black) vs predicted (red) CDFs",
         x = "p value",
         y = "cumulative proportion")
  return( list(pred_cdfs = pred_cdfs_df, cdf_plot = cdf_plot) )
}

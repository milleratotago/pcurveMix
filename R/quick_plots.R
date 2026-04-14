# quick_plots.R

#' Make a simple plot of observed p histogram and
#'   predicted pdf density
#' @param fit List output of fit_p_curve function
#' @param p_seq Sequence of p values at which to compute predictions
#' @param show_plot Display the plot after making it (default = TRUE)
#' @returns Figure object made by ggplot
#' @export
quick_pdf_plot <- function(fit, p_seq = pcm_env$p_seq_pdf, show_plot = TRUE) {
  ps <- fit$check_ps_list$ps_in_bounds
  p_seq <- p_seq[p_seq <= fit$alpha]
  pred_pdfs <- pdf(p_seq, mu = fit$mu, sigma = fit$sigma, pi = fit$pi,
                   alpha = fit$alpha) # _cutoff)  # compute predicted
  pdf_df <- data.frame(p = ps)
  pdf_plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = ps, y = ggplot2::after_stat(density)), binwidth = 0.02) +
    ggplot2::geom_line(ggplot2::aes(x = p_seq, y = pred_pdfs), color = "red") +
    ggplot2::labs(title = "Observed (black) vs predicted (red) PDFs",
         x = "p value",
         y = "density")
  # The plot is shown automatically if the function is made during
  # knitting, so check whether knitting is in progress to avoid getting
  # duplicate plots in that case.
  if (show_plot && !isTRUE(getOption('knitr.in.progress'))) print(pdf_plot)
  return(pdf_plot)
}

#' Make a simple plot of observed p cumulative histogram and
#'   predicted cdf
#' @inheritParams quick_pdf_plot
#' @returns Figure object made by ggplot
#' @importFrom rlang .data
#' @export
quick_cdf_plot <- function(fit, p_seq = pcm_env$p_seq_cdf, show_plot = TRUE) {
  ps <- fit$check_ps_list$ps_in_bounds
  p_seq <- p_seq[p_seq <= fit$alpha]
  pred_cdfs <- cdf(p_seq, mu = fit$mu, sigma = fit$sigma, pi = fit$pi,
                   alpha = fit$alpha) # _cutoff)  # compute predicted
  cdf_df <- data.frame(p = ps)
  cdf_plot <- ggplot2::ggplot() +
    ggplot2::stat_ecdf(data = cdf_df, ggplot2::aes(x = .data$p), geom = "step") +  # empirical cdf
    ggplot2::geom_line(ggplot2::aes(x = p_seq, y = pred_cdfs), color = "red") +  # predicted cdf
    ggplot2::labs(title = "Observed (black) vs predicted (red) CDFs",
         x = "p value",
         y = "cumulative proportion")
  if (show_plot && !isTRUE(getOption('knitr.in.progress'))) print(cdf_plot)
  return(cdf_plot)
}

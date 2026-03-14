# utils.R

# Function to compute a case identifier for use in switch statements.
# Possible identifiers are strings:
#  uncond_2t_h1
case_id <- function(alpha = 1, tails = 2, pi = 1) {

  if (alpha == 1) {
    s <- "uncond"
  } else if (alpha < 1 && alpha > 0) {
    s <- "cond"
  } else {
    stop("alpha must be in the range 0--1")
  }

  if (tails == 2) {
    s <- paste0(s,"_2t")
  } else if (tails == 1) {
    s <- paste0(s,"_1t")
  } else {
    stop("tails must be 1 or 2")
  }

  if (pi == 1) {
    s <- paste0(s,"_h1")
  } else if (pi < 1 && pi > 0) {
    s <- paste0(s,"_mix")
  } else {
    stop("pi must be in the range 0--1")
  }

    return(s)
}

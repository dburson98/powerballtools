#' Powerball draws from 2010 onward
#'
#' A dataset of Powerball lottery drawings starting in 2010.
#'
#' @format A data frame with one row per drawing and the following columns:
#' \describe{
#'   \item{draw_date}{Date of the drawing (Date).}
#'   \item{n1}{First white ball (integer).}
#'   \item{n2}{Second white ball (integer).}
#'   \item{n3}{Third white ball (integer).}
#'   \item{n4}{Fourth white ball (integer).}
#'   \item{n5}{Fifth white ball (integer).}
#'   \item{powerball}{Red Powerball number (integer).}
#'   \item{multiplier}{Power Play multiplier (numeric).}
#' }
#' @source Original CSV: "Lottery Powerball Winning Numbers Beginning 2010".
"pb_draws"

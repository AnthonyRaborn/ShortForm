#' A simulated data set based on a standardized test.
#'
#' Simulated response patterns, abilities, and outcomes based on a unidimensional state-issued standardized test.
#'
#'  @format A data frame of 1000 rows (observations) and 58 columns (variables):
#'  \describe{
#'  \item{Outcome}{a binary external criterion variable correlated with TrueAbility}
#'  \item{TrueAbility}{the simulated true ability parameter used to generate response patterns}
#'  \item{Item1-Item56}{binary reponses to items generated using the TrueAbility parameters and simulated 3PL item parameters generated from the distribution of parameters estimated from a state-issued standardized test}
#'  }
"simulated_test_data"

#' Model syntax for the example in the \code{\link{antcolony.lavaan}} function.
#'
#' A character vector containing the model syntax used for the one factor example in the \code{\link{antcolony.lavaan}}.
#'
#' @format A character vector.
"exampleAntModel"

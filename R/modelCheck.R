#' An S4 class for the modelCheck object
#'
#' @slot model.output A `lavaan` object.
#' @slot warnings A `character` vector of any warnings.
#' @slot errors A `character` vector of any errors.
#' @slot model.syntax A `character` vector of the modelCheck model syntax.
#'
#' @importFrom methods new show
#'
#' @return An S4 object of class `ACO`.
#' @export
#'
setClass('modelCheck',
         slots =
           list(
             model.output = 'lavaan',
             warnings = 'character',
             errors = 'character',
             model.syntax = 'character'
           )
)
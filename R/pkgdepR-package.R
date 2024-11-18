
#' @details
#' The only function you're likely to need from pkgdepR is \link[pkgdepR]{deps}, which returns an object of class \code{pkgdepR}.
#' 
#' An object of class \code{pkgdepR} is a list with three named objects:
##' \describe{
##'  \item{\strong{\code{funs}}}{: a data frame describing the functions. Contains columns \code{id}, \code{label}, \code{package}, \code{exported}, \code{group}, and \code{name}.}
##'  \item{\strong{\code{links}}}{: a data frame containing the linkages between functions. Contains columns \code{from} and \code{to}.}
##'  \item{\strong{\code{pkg}}}{: a character vector containing the packages explored.}
##' }
#' @keywords internal
"_PACKAGE"
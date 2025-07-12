#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Pipe assignment operator
#'
#' Re-export of the magrittr compound assignment pipe operator \code{\%<>\%}.
#' See \code{\link[magrittr:compound]{magrittr::\%<>\%}} for details.
#'
#' @name %<>%
#' @rdname pipe_operators
#' @export
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using magrittr semantics.
#' @return The result of assigning `rhs(lhs)` to `lhs`.
NULL

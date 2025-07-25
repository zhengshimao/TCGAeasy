#' Extract Text from XML Node
#'
#' A helper function to extract text content from XML nodes using XPath.
#'
#' @param doc An XML document parsed by \code{xml2::read_xml()}.
#' @param xpath A character string representing the XPath query.
#' @param return_all return all result or not. if TRUE, return all results. if FALSE, return unique result.
#'
#' @return A character vector of node text values. Returns \code{NA} if the node is not found.
#'
#' @importFrom xml2 xml_find_all xml_text
#' @keywords internal
#' @export
get_text <- function(doc, xpath, return_all = TRUE) {
  node <- xml2::xml_find_all(doc, xpath)
  if (any(is.na(node))) return(NA)

  if(return_all){
    return(xml2::xml_text(node))
  }else{
    return(unique(xml2::xml_text(node)))
  }
}

#' Extract Integer from XML Node
#'
#' A helper function to extract integer values from XML nodes using XPath.
#'
#' @param doc An XML document parsed by \code{xml2::read_xml()}.
#' @param xpath A character string representing the XPath query.
#' @param return_all return all result or not. if TRUE, return all results. if FALSE, return unique result.
#'
#' @return An integer vector of node values. Returns \code{NA} if the node is not found.
#'
#' @importFrom xml2 xml_find_all xml_integer
#' @keywords internal
#' @export
#'
get_integer <- function(doc, xpath, return_all = TRUE) {
  node <- xml2::xml_find_all(doc, xpath)
  if (any(is.na(node))) return(NA)

  if(return_all){
    return(xml2::xml_integer(node))
  }else{
    return(unique(xml2::xml_integer(node)))
  }

}

#' Extract Numeric from XML Node
#'
#' A helper function to extract numeric values from XML nodes using XPath.
#'
#' @param doc An XML document parsed by \code{xml2::read_xml()}.
#' @param xpath A character string representing the XPath query.
#' @param return_all return all result or not. if TRUE, return all results. if FALSE, return unique result.
#'
#' @return An numeric vector of node values. Returns \code{NA} if the node is not found.
#'
#' @importFrom xml2 xml_find_all
#' @keywords internal
#' @export
#'
get_numeric <- function(doc, xpath, return_all = TRUE) {
  node <- xml2::xml_find_all(doc, xpath)
  if (any(is.na(node))) return(NA)

  if(return_all){
    return(as.numeric(xml2::xml_text(node)))
  }else{
    return(unique(as.numeric(xml2::xml_text(node))))
  }

}

#' @title Iterative garbage collection.
#'
#' @description
#' Performs garbage collection until free memory idicators show no change.
#' same with `WGCNA::collectGarbage()`.
#'
#' @return
#' No return value.
#'
#' @export
#'
#' @examples
#' collectGarbage()
collectGarbage <- function(){
  while (gc()[2, 4] != gc()[2, 4] | gc()[1, 4] != gc()[1, 4]) {
  }
}

#' Check if a string is numeric-like
#'
#' This function checks whether the input string represents a numeric value,
#' allowing optional leading minus sign and decimal point.
#' Note: This is a simple check and does not guarantee strict numeric formatting.
#'
#' @param x A character string to test.
#'
#' @return A logical value. TRUE if the string looks like a number, FALSE otherwise.
#'
#' @export
#' @examples
#' is_numeric_str("123")      # TRUE
#' is_numeric_str("-45.6")    # TRUE
#' is_numeric_str("12.3.4")   # TRUE (not strictly valid number)
#' is_numeric_str("abc")      # FALSE
is_numeric_str <- function(x) grepl("^-?[0-9.]+$", x)

# # 1.整理admin：方法一
#   list_admin <- tibble::tibble(
#     bcr       = get_text(node_admin, xpath = ".//admin:bcr"),
#     file_uuid = get_text(node_admin, xpath = ".//admin:file_uuid"),
#     batch_number =       get_text(node_admin, xpath = ".//admin:batch_number"),
#     project_code =       get_text(node_admin, xpath = ".//admin:project_code"),
#     disease_code = get_text(node_admin, xpath = ".//admin:disease_code"),
#     day_of_dcc_upload = get_integer(node_admin, xpath = ".//admin:day_of_dcc_upload"),
#     month_of_dcc_upload = get_integer(node_admin, xpath = ".//admin:month_of_dcc_upload"),
#     year_of_dcc_upload = get_integer(node_admin, xpath = ".//admin:year_of_dcc_upload"),
#     patient_withdrawal = get_text(node_admin, xpath = ".//admin:patient_withdrawal")
#   )


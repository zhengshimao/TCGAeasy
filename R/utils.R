#' Extract Text from XML Node
#'
#' A helper function to extract text content from XML nodes using XPath.
#'
#' @param doc An XML document parsed by \code{xml2::read_xml()}.
#' @param xpath A character string representing the XPath query.
#'
#' @return A character vector of node text values. Returns \code{NA} if the node is not found.
#'
#' @importFrom xml2 xml_find_all xml_text
#' @keywords internal
#' 
get_text <- function(doc, xpath) {
  node <- xml2::xml_find_all(doc, xpath)
  if (is.na(node)) return(NA)
  xml2::xml_text(node)
}

#' Extract Integer from XML Node
#'
#' A helper function to extract integer values from XML nodes using XPath.
#'
#' @param doc An XML document parsed by \code{xml2::read_xml()}.
#' @param xpath A character string representing the XPath query.
#'
#' @return An integer vector of node values. Returns \code{NA} if the node is not found.
#'
#' @importFrom xml2 xml_find_all xml_integer
#' @keywords internal
get_integer <- function(doc, xpath) {
  node <- xml2::xml_find_all(doc, xpath)
  if (is.na(node)) return(NA)
  xml2::xml_integer(node)
}

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


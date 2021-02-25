#' Get Release Country
#'
#' Get the country codes of the release countries by ISRC code.
#'
#' @param isrc A vector of iscr codes
#' @importFrom dplyr case_when
#' @return A vector of ISO-3166 country codes.
#' @export

get_release_country <- function ( isrc ) {
  isrc <- substr(as.character(isrc),1,2)
  dplyr::case_when (
    isrc %in% c("QM", "QZ", "US") ~  "US",
    isrc %in% c("BR", "BX") ~  "BR",
    TRUE ~ isrc )
}


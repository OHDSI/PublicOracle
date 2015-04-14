#' Simple query
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @examples
#' query("vocabulary/concept/19059796")
#'
#' @export
query <- function(content) {
  publicApi <- "http://api.ohdsi.org/WebAPI/" # TODO Store somewhere else
  rjson::fromJSON(RCurl::getURL(paste(publicApi, content, sep = "")))
}

#' Simple query to OHDSI WebAPI
#'
#' @param content WebAPI query text
#' @param urlApi  URL of an OHDSI WebAPI server; missing defaults to the public OHDSI server
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#'
#' @examples
#' \donttest{
#' query("vocabulary/concept/19059796")
#' }
#'
#' @export
query <- function(content, urlApi = getPublicOhdsiUrl()) {
   rjson::fromJSON(RCurl::getURL(paste(urlApi, content, sep = "")))
}

#' Get the public OHDSI WebAPI URL
#'
#' @return URL to the public OHDSI WebAPI
#'
#' @export
getPublicOhdsiUrl <- function() {
  return("http://api.ohdsi.org/WebAPI/")
}

#' Create connection to public OHDSI database
#'
#' @return An established connection to the public OHDSI server
#'
#' @importFrom DatabaseConnector createConnectionDetails
#'
#' @examples
#' \donttest{
#' conn <- createPublicOhdsiConnection()
#' }
#'
#' @export
createPublicOhdsiConnection <- function() {

  connectionDetails <- DatabaseConnector::createConnectionDetails(
                        dbms = "postgresql",
                        server = "laertes.ohdsi.org/vocabularyv5",
                        user = "webapi",
                        password = "auHpR9J3Z2DKv4YEj",
                        port = 5432,
                        schema = "unrestricted")

  conn <- DatabaseConnector::connect(connectionDetails)

  if (is.null(conn)) {
    stop("Failed to connect to db server.")
  }
  return(conn)
}

#' Get table names from public OHDSI database
#'
#' @param conn  An established OHDSI database connection; NULL defaults to the public OHDSI server
#'
#' @examples
#' \donttest{
#' getPublicOhdsiTableNames()
#' }
#'
#' @export
getPublicOhdsiTableNames <- function(conn = NULL) {
  query <-
    "SELECT table_name
     FROM information_schema.tables
     WHERE table_schema='public'
     AND table_type='BASE TABLE';"

  return(queryPublicOhdsiDatabase(conn,
                                  query))
}

#' Get field names from a public OHDSI table
#'
#' @param conn  An established OHDSI database connection; NULL defaults to the public OHDSI server
#' @param tableName  An OMOP CDM table name
#'
#' @examples
#' \donttest{
#' getPublicOhdsiColumnNames(tableName = "concept")
#' }
#'
#' @export
getPublicOhdsiColumnNames <- function(conn = NULL,
                                      tableName) {
  parameterizedSql <-
    "SELECT column_name,* FROM information_schema.columns
     WHERE table_name = '@tableName'
     AND table_schema='public'
     ORDER by ordinal_position"

  return(queryPublicOhdsiDatabase(conn,
                                  parameterizedSql,
                                  tableName = tableName))
}

#' Retrieve OMOP concept information by ID
#'
#' @param conn  An established OHDSI database connection; NULL defaults to the public OHDSI server
#' @param conceptIds  A vector of OMOP concept IDs
#'
#' @examples
#' \donttest{
#' getConceptInformation(conceptIds = c(19059796,705755))
#' }
#'
#' @export
getConceptInformation <- function(conn = NULL,
                                  conceptIds) {

  listIds <- paste(conceptIds,collapse=",")
  parameterizedSql <-
    "SELECT *
     FROM concept
     WHERE concept_id in (@listIds)"

  return(queryPublicOhdsiDatabase(conn,
                                  parameterizedSql,
                                  listIds = listIds))
}

#' Find common ancestors for OMOP concept IDs
#'
#' @param conn  An established OHDSI database connection; NULL defaults to the public OHDSI server
#' @param conceptIds  A vector of OMOP concept IDs
#'
#' @examples
#' \donttest{
#' findCommonAncestors(conceptIds = c(703470, 705755, 738156))
#' }
#'
#' @export
findCommonAncestors <- function(conn = NULL,
                                conceptIds) {

  if (length(conceptIds) < 2) stop("Must provide at least two OMOP concept IDs")

  listIds <- paste(conceptIds, collapse = ",")

  countIds <- length(conceptIds)

  parameterizedSql <-
    "select ancestor_concept_id,
            c.concept_name,
            c.vocabulary_id,
            min(min_levels_of_separation) minimumDistance
     from concept_ancestor ca
     join concept c on ca.ancestor_concept_id = c.concept_id
     where ca.descendant_concept_id in (@listIds)
     group by c.concept_name, ca.ancestor_concept_id, c.vocabulary_id
     having count(*) = @countIds
     order by min(min_levels_of_separation)"

  return(queryPublicOhdsiDatabase(conn,
                                  parameterizedSql,
                                  countIds = countIds,
                                  listIds = listIds))
}

#' Find ATC ancestors for OMOP concept IDs
#'
#' @param conn  An established OHDSI database connection; NULL defaults to the public OHDSI server
#' @param conceptIds  A vector of OMOP concept IDs
#'
#' @examples
#' \donttest{
#' findAncestors(conceptIds = c(703470, 705755, 738156))
#' }
#'
#' @export
findAncestors <- function(conn = NULL,
                             conceptIds) {

  listIds <- paste(conceptIds, collapse = ",")

  parameterizedSql <-
    "select ancestor_concept_id,
            c.concept_name,
            c.vocabulary_id,
            descendant_concept_id,
            min_levels_of_separation,
            max_levels_of_separation
     from concept_ancestor ca
     join concept c on ca.ancestor_concept_id = c.concept_id
     where ca.descendant_concept_id in (@listIds)"

  return(queryPublicOhdsiDatabase(conn,
                                  parameterizedSql,
                                  listIds = listIds))
}



#' @importFrom SqlRender renderSql
#' @importFrom DatabaseConnector querySql
#' @importFrom DBI dbDisconnect
#' @export
queryPublicOhdsiDatabase <- function(conn = NULL,
                                     parameterizedSql, ...) {
  # Set-up connection if necessary
  release <- FALSE
  if (is.null(conn)) {
    conn <- createPublicOhdsiConnection()
    release <- TRUE
  }

  # Render and query database
  renderedSql <- SqlRender::renderSql(parameterizedSql, ...)
  result <- DatabaseConnector::querySql(conn, renderedSql$sql)

  # Release connection if necessary and return result
  if (release) DBI::dbDisconnect(conn)
  return (result)
}

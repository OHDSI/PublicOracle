#' Simple query
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @examples
#' query("vocabulary/concept/19059796")
#'
#' @export
query <- function(content, urlApi = getPublicOhsdiUrl()) {
   rjson::fromJSON(RCurl::getURL(paste(urlApi, content, sep = "")))
}

#' Get the public OHDSI WebAPI URL
#'
#' @export
getPublicOhdsiUrl <- function() {
  return("http://api.ohdsi.org/WebAPI/")
}

#' Create connection to public OHDSI database
#'
#' @importFrom DatabaseConnector createConnectionDetails
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

# conn <- createPublicOhdsiConnection()
#
# text <- "SELECT
# A.concept_id                 Class_Concept_Id,
# A.concept_name               Class_Name,
# A.concept_code               Class_Code,
# A.concept_class_id           Classification_id,
# A.vocabulary_id              Class_vocabulary_id,
# V.vocabulary_name            Class_vocabulary_name,
# CA.min_levels_of_separation  Levels_of_Separation
# FROM concept_ancestor   CA,
# concept                A,
# concept                D,
# vocabulary             V
# WHERE  CA.descendant_concept_id = D.concept_id
# AND    CA.ancestor_concept_id = A.concept_id
# AND    A.vocabulary_id = V.vocabulary_id
# AND    D.concept_id = 1545999;"  # atorvastatin info
#
# quertSql(conn, text)

#' Get table names from public OHDSI database
#'
#' @importFrom DatabaseConnector querySql
#' @importFrom DBI dbDisconnect
#' @export
getPublicOhdsiTableNames <- function(conn = NULL) {
  release <- FALSE
  if (is.null(conn)) {
    conn <- createPublicOhdsiConnection()
    release <- TRUE
  }
  result <- DatabaseConnector::querySql(conn,
"SELECT table_name
 FROM information_schema.tables
 WHERE table_schema='public'
 AND table_type='BASE TABLE';")

  if (release) DBI::dbDisconnect(conn)

  return (result)
}

#' Find common ancestors for OMOP concept IDs
#'
#' @importFrom SqlRender renderSql
#' @importFrom DatabaseConnector querySql
#' @importFrom DBI dbDisconnect
#'
#' @examples
#' findCommonAncestors(conceptIds = c(703470, 705755, 738156))
#'
#' @export
findCommonAncestors <- function(conn = NULL,
                               conceptIds) {

  if (length(conceptIds) < 2) stop("Must provide at least two OMOP concept IDs")

  release <- FALSE
  if (is.null(conn)) {
    conn <- createPublicOhdsiConnection()
    release <- TRUE
  }

  listIds <- paste(conceptIds, collapse = ",")
  listIds <- paste("(", listIds, ")", sep = "")

  countIds <- length(conceptIds)

  parameterizedSql <-
    "select ancestor_concept_id,
            c.concept_name,
            c.vocabulary_id,
            min(min_levels_of_separation) minimumDistance
     from concept_ancestor ca
     join concept c on ca.ancestor_concept_id = c.concept_id
     where ca.descendant_concept_id in @listIds
     group by c.concept_name, ca.ancestor_concept_id, c.vocabulary_id
     having count(*) = @countIds
     order by min(min_levels_of_separation)"

  renderedSql <- SqlRender::renderSql(parameterizedSql, listIds = listIds,
                           countIds = countIds)

  result <- DatabaseConnector::querySql(conn, renderedSql$sql)

  if (release) DBI::dbDisconnect(conn)

  return (result)
}

require(alveo)


#' Return the prompts for a given component
#'
#' @param component The name of the target component e.g. digits
#' @return A data frame containing the prompts and item numbers for this component
#' @export
#'
prompts <- function(component) {

  query <- "
  PREFIX dcterms:<http://purl.org/dc/terms/>
  PREFIX austalk:<http://ns.austalk.edu.au/>

 SELECT DISTINCT ?componentname ?itemid ?prompt WHERE {
  ?comp a austalk:Component .
  ?comp austalk:shortname '%s' .
  ?comp austalk:name ?componentname .
  ?item dcterms:isPartOf ?comp .
  ?item austalk:id ?itemid .
  ?item austalk:prompt ?prompt .
}"

  query <- sprintf(query, component)

  client <- alveo::RestClient()

  result <- client$sparql(query, "austalk")

  return(result2df(result))
}



#' Find the component names
#'
#' @return A vector of component names
#' @export
#'
components <- function() {

  query <- "
  PREFIX austalk:<http://ns.austalk.edu.au/>

  SELECT DISTINCT ?shortname ?name  WHERE {
  ?comp a austalk:Component .
  ?comp austalk:shortname ?shortname .
  ?comp austalk:name ?name .
  }
  "

  client <- alveo::RestClient()

  result <- client$sparql(query, "austalk")

  return(result2df(result))
}

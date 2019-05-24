require(alveo)

#' Get a list of speakers from Austalk
#'
#' @return A dataframe containing speaker metadata
#' @export
#'
"speakers" <- function() {

  metaList <- c('pob_town', 'pob_state', 'pob_country',
                'cultural_heritage','ses','professional_category',
              'education_level', 'other_languages',
              'mother_cultural_heritage','mother_professional_category','mother_education_level',
              'mother_pob_state','mother_pob_town','mother_pob_country',
              'father_cultural_heritage', 'father_professional_category',
              'father_education_level','father_pob_state', 'father_pob_town','father_pob_country',
              'hobbies_details','has_vocal_training','is_smoker',
              'has_speech_problems','has_piercings','has_health_problems','has_hearing_problems',
              'has_dentures','is_student','is_left_handed','has_reading_problems'
              )

  prefixes <- "
PREFIX dcterms:<http://purl.org/dc/terms/>
PREFIX foaf:<http://xmlns.com/foaf/0.1/>
  PREFIX austalk:<http://ns.austalk.edu.au/>
  PREFIX olac:<http://www.language-archives.org/OLAC/1.1/>
  PREFIX ausnc:<http://ns.ausnc.org.au/schemas/ausnc_md_model/>
  PREFIX geo:<http://www.w3.org/2003/01/geo/wgs84_pos#>

"

  select <- "SELECT ?speaker ?age ?city ?gender ?b_state ?b_town ?lat ?long "

  select <- paste(select, "?",  paste(metaList, collapse = ' ?'), sep='')

  where <- "WHERE {
        ?id a foaf:Person .
  		  ?id dcterms:identifier ?speaker .
        ?id austalk:recording_site ?recording_site .
        ?recording_site austalk:city ?city .
        ?id foaf:age ?age .
        ?id foaf:gender ?gender .
        OPTIONAL {
      		?id austalk:birthPlace ?birthPlace .
    		  ?birthPlace geo:lat ?lat .
    		  ?birthPlace geo:long ?long .
          ?birthPlace geo:state ?b_state .
          ?birthPlace geo:town ?b_town .
        }
"

  optional <- sapply(metaList,
                     function(x) {
                          return(paste("OPTIONAL {?id austalk:", x, " ?", x, "}.", sep=''))},
                    USE.NAMES = FALSE)

  where <- paste(where, paste(optional, collapse='\n'))

  query = paste(prefixes, select, where, "\n}")

  client <- alveo::RestClient()

  result <- client$sparql(query, "austalk")

  return(result2df(result))
}

#' Generate a table of the number of recordings in components for
#' all speakers
#'
#' @param components a vector of component names, eg. c('digits', 'words-1'), see `components()` for a full list
#' @return a data.frame containing speaker ids and a count of items for each component
#' @export
#'
"speakerItems" <- function(components) {

  query_template <- "
  PREFIX dcterms:<http://purl.org/dc/terms/>
  PREFIX foaf:<http://xmlns.com/foaf/0.1/>
  PREFIX austalk:<http://ns.austalk.edu.au/>
  PREFIX olac:<http://www.language-archives.org/OLAC/1.1/>
  PREFIX ausnc:<http://ns.ausnc.org.au/schemas/ausnc_md_model/>

  SELECT ?speaker (count(?item) as ?%s) WHERE {
    ?item olac:speaker ?spkr .
    ?spkr dcterms:identifier ?speaker .
    ?item austalk:componentName '%s' .
    ?item austalk:session ?session .
  } group by ?speaker
  "

  client <- alveo::RestClient()
  result <- NULL

  for (component in components) {
    # we use the component name as the output count variable name
    # as well as inserting it into the query to match the component
    # this way the result has a column named for the component
    query <- sprintf(query_template, gsub('-', '_', component), component)

    qresult <- client$sparql(query, "austalk")

    df <- result2df(qresult)

    if (is.null(result)) {
      result <- df
    } else {
      if (dim(df)[1] != 0) {
        result <- merge(result, df, by='speaker', all=TRUE)
      } else {
        message("No matches for ", component)
      }
    }
  }

  # set all NA values to a count of zero
  result[is.na(result)] = 0
  return(result)
}

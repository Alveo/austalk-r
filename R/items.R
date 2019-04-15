require(alveo)


#' Get items for a speaker from a component
#'
#' @param speakers A vector of speaker identifiers, eg. "1_10"
#' @param component The Austalk component name, eg. 'digits', see `components()` for a full list
#' @return an alveo::ItemList
#' @export
#' @example componentItems('1_10', 'digits')
#'
componentItems <- function(speakers, component) {

  query_template <- "
  PREFIX dcterms:<http://purl.org/dc/terms/>
  PREFIX foaf:<http://xmlns.com/foaf/0.1/>
  PREFIX austalk:<http://ns.austalk.edu.au/>
  PREFIX olac:<http://www.language-archives.org/OLAC/1.1/>
  PREFIX ausnc:<http://ns.ausnc.org.au/schemas/ausnc_md_model/>

  SELECT ?spkrid ?item_url ?itemid WHERE {
  BIND ('%s' as ?spkrid)
      ?spkr a foaf:Person .
      ?spkr dcterms:identifier ?spkrid .
      ?item_url a ausnc:AusNCObject .
      ?item_url dcterms:identifier ?itemid .
      ?item_url olac:speaker ?spkr .
      ?item_url austalk:componentName '%s' .
  }
  "
  items = NULL
  client <- alveo::RestClient()
  for(speaker in speakers) {
    query <- sprintf(query_template, speaker, component)
    result <- client$sparql(query, "austalk")
    items <- rbind(items, result2df(result))
  }
  return(items)
}


#' Download documents for a list of items
#'
#' @param items A dataframe containing at least an `item_url` column
#' @param destination Directory to save downloaded files
#' @param channels A vector of channel names or patterns matching files to be downloaded
#'
#' @return Files are downloaded
downloadItems <- function(items, destination, channels=c('speaker16')) {

  # make an item list out of the item urls
  item_list <- alveo::ItemList(items=items$item_url)

  # make a regex to match any of the provided channel strings
  pattern = paste(channels, collapse="|")

  print(pattern)
  item_list$download(destination, pattern=pattern)

}


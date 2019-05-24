require(alveo)


#' Get items for a speaker from a component
#'
#' @param speakers A vector of speaker identifiers, eg. "1_10"
#' @param component The Austalk component name, eg. 'digits', see `components()` for a full list
#' @return a dataframe containing item URLs, speaker id and item id
#' @export
#'
componentItems <- function(speakers, component) {

  query_template <- "
  PREFIX dcterms:<http://purl.org/dc/terms/>
  PREFIX foaf:<http://xmlns.com/foaf/0.1/>
  PREFIX austalk:<http://ns.austalk.edu.au/>
  PREFIX olac:<http://www.language-archives.org/OLAC/1.1/>
  PREFIX ausnc:<http://ns.ausnc.org.au/schemas/ausnc_md_model/>

  SELECT ?speaker ?item_url ?itemid WHERE {
  BIND ('%s' as ?speaker)
      ?spkr a foaf:Person .
      ?spkr dcterms:identifier ?speaker .
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
#' @export
downloadItems <- function(items, destination, channels=c('speaker16')) {

  # make an item list out of the item urls
  item_list <- alveo::ItemList(items=items$item_url)

  # make a regex to match any of the provided channel strings
  pattern = paste(channels, collapse="|")

  item_list$download(destination, pattern=pattern)

}



#' Get a list of items from an item list
#'
#' @param url The URL of the item list
#'
#' @return A data frame containing the item URLs
#' @export
getItemList <- function(url) {

  client <- alveo::RestClient()
  item_list <- client$get_item_list(url)
  # warn if items do not belong to austalk

  collection <- sapply(strsplit(item_list$items, '/'), function(s) s[5])

  if (!all(collection == 'austalk')) {
    stop("Item list contains items from collections other than Austalk, use alveo::get_item_list instead")
  }

  itemids <- sapply(strsplit(item_list$items, '/'), function(s) s[6]  )
  speakers <- sapply(strsplit(itemids, '_'), function(s) paste(s[1], s[2], sep='_'))
  result = data.frame( speakers=speakers, item_url=item_list$items, itemid=itemids, stringsAsFactors = FALSE)

  return(result)
}



#' Get list of items from a contribution
#'
#' @param url The URL of the contribution
#'
#' @return A data frame containing item urls
#' @export
getContribution <- function(url) {

  client <- alveo::RestClient()
  contrib <- client$get_contribution(url)

  docurls <- sapply(contrib$documents, function(s) s$url)
  itemurls <- sapply(strsplit(docurls, '/document/'), function(s) s[1])

  collection <- sapply(strsplit(itemurls, '/'), function(s) s[5])

  if (!all(collection == 'austalk')) {
    stop("Item list contains items from collections other than Austalk, use alveo::get_item_list instead")
  }

  itemids <- sapply(strsplit(itemurls, '/'), function(s) s[6]  )
  speakers <- sapply(strsplit(itemids, '_'), function(s) paste(s[1], s[2], sep='_'))
  result = data.frame( speakers=speakers, item_url=itemurls, itemid=itemids, stringsAsFactors = FALSE)
  return(result)

}


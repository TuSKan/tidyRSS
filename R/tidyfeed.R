#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @importFrom httr GET
#' @importFrom lubridate parse_date_time
#' @importFrom xml2 read_xml
#' @importFrom xml2 as_list
#' @importFrom xml2 xml_contents
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_find_first
#' @importFrom dplyr select
#' @importFrom dplyr full_join
#' @importFrom sf st_as_sf
#' @importFrom stringr str_extract
#' @importFrom purrr map
#' @author Robert Myles McDonnell, \email{robertmylesmcdonnell@gmail.com}
#' @references \url{https://en.wikipedia.org/wiki/RSS}
#' @title Extract a tidy data frame from RSS and Atom and JSON feeds
#' @description \code{tidyfeed()} downloads and parses rss feeds. The function
#' produces a tidy data frame, easy to use for further manipulation and
#' analysis.
#' @param feed (\code{character}). The urls for the feed that you want to parse.
#' @param sf . If TRUE, returns sf dataframe.
#' @inheritParams httr::GET config
#' @examples
#' \dontrun{
#' # Atom feed:
#' tidyfeed("http://journal.r-project.org/rss.atom")
#' # rss/xml:
#' tidyfeed("http://fivethirtyeight.com/all/feed")
#' # jsonfeed:
#' tidyfeed("https://daringfireball.net/feeds/json")
#' # georss:
#' tidyfeed("http://www.geonames.org/recent-changes.xml")
#' }
#' @export
tidyfeed <- function(feeds, sf = TRUE) {
  #invisible({
  #suppressWarnings({
  msg <- "Error in feed parse; please check URL.\nIf you're certain that this is a valid rss feed, please file an issue at https://github.com/RobertMyles/tidyRSS/issues. Please note that the feed may also be undergoing maintenance."

  feedDF <- list()
  callback <- function(request) {
    #cat("done:", request$url,  ": HTTP:", request$status_code, "\n")
    result <- NULL
    if (request$status_code == 200) {
      if (grepl("json", last(request$headers))) {
        result <- json_parse(rawToChar(request$content))
      } else{
        result <- rawToChar(request$content) %>% xml2::read_xml()
      }
      if ("xml_document" %in% class(result)) {
        if (grepl("http://www.w3.org/2005/Atom", xml2::xml_attr(result, "xmlns"))) {
          result <- atom_parse(result)
        } else if (grepl("http://www.georss.org/georss", xml2::xml_attr(result, "xmlns:georss"))) {
          result <- geo_parse(result)
          if (!exists('result$item_long')) {
            result <- rss_parse(result)
          } else {
            if (sf == TRUE) {
              result <- sf::st_as_sf(x = result, coords = c("item_long", "item_lat"),crs = "+proj=longlat +datum=WGS84")
            }
          }
        } else {
          result <- rss_parse(result)
        }
      }
    } else print(msg)
    feedDF[[request$url]] <<- result
  }
  pool <- curl::new_pool()
  lapply(feeds, function(feed) {
    handle <- curl::new_handle()
    curl::curl_fetch_multi(feed, done = callback, pool = pool, handle = handle)
  })

  out <- curl::multi_run(pool = pool)
  print(unlist(out))
  data.table::rbindlist(feedDF, fill = TRUE)
}

last <- function(x) {
  if (length(x) < 1) return(x)
  x[[length(x)]]
}


#' Download DB-IP data files
#'
#' This function downloads geolocation and ASN data from DB-IP website.
#'
#' @importFrom dplyr %>%
#' @return Character vector with paths to downloaded files
#' @export
download_dbip <- function() {

  dir.create("data-raw/source", showWarnings = FALSE, recursive = TRUE)

  get_clean_url <- function(page_url, pattern) {

    page <- rvest::read_html(page_url)
    all_links <- page %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")

    target_link <- grep(pattern, all_links, value = TRUE)[1]

    if (is.na(target_link)) {
      stop("Не удалось найти ссылку")
    }

    clean_url <- sub("('| ).*", "", target_link)

    return(clean_url)
  }

  city_url <- get_clean_url("https://db-ip.com/db/download/ip-to-city-lite",
                            pattern = "dbip-city-lite")

  asn_url <- get_clean_url("https://db-ip.com/db/download/ip-to-asn-lite",
                           pattern = "dbip-asn-lite")

  message("Скачивание файла с геолокацией")
  utils::download.file(city_url, "data-raw/source/geo.csv.gz", mode = "wb")

  message("Скачивание файла с ASN")
  utils::download.file(asn_url, "data-raw/source/asn.csv.gz", mode = "wb")

  message("Файлы успешно скачаны")

  return(c(geo = "data-raw/source/geo.csv.gz", asn = "data-raw/source/asn.csv.gz"))
}

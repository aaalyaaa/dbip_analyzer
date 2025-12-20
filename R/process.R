.datatable.aware <- TRUE

#' Process DB-IP data
#'
#' This function processes downloaded DB-IP files.
#'
#' @importFrom data.table setkey
#' @importFrom arrow write_parquet
#' @return Merged and cleaned data frame
#' @keywords internal
process_dbip <- function(files, output_dir = "processed") {

  options(warn = -1)
  on.exit(options(warn = 0))

  geo <- data.table::fread(
    files["geo"],
    header = FALSE,
    col.names = c("ip_start", "ip_end", "continent", "country",
                  "state", "city", "latitude", "longitude")
  )

  asn <- data.table::fread(
    files["asn"],
    header = FALSE,
    col.names = c("ip_start", "ip_end", "as_number", "as_organization")
  )

  ip_to_int <- function(ip_vector) {
    is_valid <- grepl("^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$", ip_vector)
    result <- rep(NA_integer_, length(ip_vector))

    if (any(is_valid)) {
      valid_ips <- ip_vector[is_valid]
      m <- do.call(rbind, strsplit(valid_ips, ".", fixed = TRUE))
      mode(m) <- "numeric"
      converted <- m[,1]*16777216 + m[,2]*65536 + m[,3]*256 + m[,4]
      result[is_valid] <- converted
    }

    return(result)
  }

  geo[, `:=`(start_num = ip_to_int(ip_start),
             end_num = ip_to_int(ip_end))]

  asn[, `:=`(asn_start = ip_to_int(ip_start),
             asn_end = ip_to_int(ip_end))]

  geo_clean <- geo[!is.na(start_num) & !is.na(end_num) &
                     !is.na(latitude) & !is.na(longitude)]

  asn_clean <- asn[!is.na(asn_start) & !is.na(asn_end)]

  setkey(asn_clean, asn_start, asn_end)

  result <- asn_clean[geo_clean,
                      .(ip_start = i.ip_start,
                        ip_end = i.ip_end,
                        continent = i.continent,
                        country = i.country,
                        state = i.state,
                        city = i.city,
                        latitude = i.latitude,
                        longitude = i.longitude,
                        as_number = x.as_number,
                        as_organization = x.as_organization),
                      on = .(asn_start <= start_num, asn_end >= end_num),
                      mult = "first"
  ]

  result[, `:=`(
    as_number = as.character(as_number),
    state = data.table::fifelse(state == "", NA_character_, trimws(state)),
    city = data.table::fifelse(city == "", NA_character_, trimws(city)),
    continent = data.table::fifelse(continent == "ZZ", NA_character_, trimws(continent)),
    country = data.table::fifelse(country == "ZZ", NA_character_, trimws(country)),
    as_organization = trimws(as_organization)
  )]

  result <- result[!is.na(as_number) & !is.na(as_organization)]

  return(result)
}

#' Load processed data from Parquet file
#' @description Loads data from the specified path and checks if file exists
#' @return Loaded dataframe with data
#' @noRd

load_processed_data <- function() {

  user_dir <- getwd()
  data_path <- file.path(user_dir, "processed", "dbip_data.parquet")

  cat("User directory:", user_dir, "\n")
  cat("Looking for:", data_path, "\n")

  if (!file.exists(data_path)) {
    stop(
      "Данные не найдены. Запустите run_etl()")}

  data <- arrow::read_parquet(data_path)
  return(data)
}

#' Get basic statistics about the data
#' @description Calculates basic statistics for the DB-IP dataframe
#' @return List with statistical metrics
#' @noRd
#'
get_data_stats <- function(data) {
  stats_list <- list(
    total_rows = nrow(data),
    total_continents = length(unique(data$continent)),
    total_countries = length(unique(data$country)),
    total_states = length(unique(data$state)),
    total_cities = length(unique(data$city)),
    total_as_organizations = length(unique(data$as_organization)))

  cat("Всего строк:", stats_list$total_rows, "\n")
  cat("Уникальных континентов:", stats_list$total_continents, "\n")
  cat("Уникальных стран:", stats_list$total_countries, "\n")
  cat("Уникальных регионов:", stats_list$total_states, "\n")
  cat("Уникальных городов:", stats_list$total_cities, "\n")
  cat("Уникальных AS организаций:", stats_list$total_as_organizations, "\n")

  return(stats_list)
}

#' Create interactive map of IP addresses
#' @description Generates a Leaflet map showing IP address distribution
#' @return Leaflet map with IP address markers
#' @noRd

create_ip_map <- function(data, sample_size) {

  continent_palette <- leaflet::colorFactor(
    palette = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#FFA500"),
    domain = unique(data$continent)
  )

  map <- leaflet::leaflet(data) |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      popup = ~paste(
        "<b>IP Start:</b>", ip_start, "<br>",
        "<b>IP End:</b>", ip_end, "<br>",
        "<b>Город:</b>", ifelse(is.na(city), "Н/Д", city), "<br>",
        "<b>Регион:</b>", ifelse(is.na(state), "Н/Д", state), "<br>",
        "<b>Страна:</b>", ifelse(is.na(country), "Н/Д", country), "<br>",
        "<b>Континент:</b>", ifelse(is.na(continent), "Н/Д", continent), "<br>",
        "<b>AS Number:</b>", ifelse(is.na(as_number), "Н/Д", as_number), "<br>",
        "<b>Организация:</b>", ifelse(is.na(as_organization), "Н/Д", as_organization)
      ),
      radius = 5,
      color = ~continent_palette(continent),
      fillOpacity = 0.5,
      clusterOptions = leaflet::markerClusterOptions()
    ) |>
    leaflet::addControl(
      "Карта распределения IP-адресов",
      position = "topright"
    ) |>
    leaflet::addLegend(
      "bottomright",
      pal = continent_palette,
      values = ~continent,
      title = "Континенты",
      opacity = 1
    )

  return(map)
}

#' Create chart of top AS organizations
#' @description Makes a bar chart showing top AS organizations by IP ranges
#' @return ggplot bar chart
#' @noRd

create_as_org_chart <- function(data, top_n) {
  as_org_counts <- data |>
    dplyr::filter(!is.na(as_organization) & as_organization != "") |>
    dplyr::count(as_organization, sort = TRUE) |>
    head(top_n)
  p <- ggplot2::ggplot(as_org_counts, ggplot2::aes(x = reorder(as_organization, n), y = n)) +
    ggplot2::geom_bar(stat = "identity", fill = "darkgreen") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "AS Организация",
      y = "Количество диапазонов IP"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 9),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.margin = ggplot2::margin(1, 3, 1, 1, "cm")
    )

  return(p)}

#' Create HTML table from data
#' @description Generates an HTML table showing first n rows of data
#' @return HTML table
#' @noRd

create_table <- function(data, n) {
  table_data <- head(data, n)

  table <- knitr::kable(
    table_data,
    format = "html",
    align = rep("l", ncol(table_data))
  )
  return(table)
}




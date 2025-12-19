#' Load processed data from Parquet file
#' @description Loads data from the specified path and checks if file exists
#' @return Loaded dataframe with data
#' @noRd

load_processed_data <- function() {

  user_dir <- getwd()
  data_path <- file.path(user_dir, "processed", "dbip_data.parquet")

  if (!file.exists(data_path)) {
    stop(
      "Данные не найдены. Запустите run_etl()")}
  data <- suppressMessages({
    arrow::read_parquet(data_path, quiet = TRUE)
  })

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

  grouped_data <- data |>
    dplyr::group_by(latitude, longitude) |>
    dplyr::summarise(
      count = dplyr::n(),
      ip_ranges_details = paste(
        sapply(1:n(), function(i) {
          paste0(
            "<div style='margin-bottom: 5px; padding: 3px; background: #f5f5f5; border-radius: 3px;'>",
            "<b>Диапазон ", i, ":</b><br>",
            "<span style='color: #333;'>IP Start: ", ip_start[i], "</span><br>",
            "<span style='color: #333;'>IP End: ", ip_end[i], "</span><br>",
            "<span style='color: #666; font-size: 0.9em;'>",
            "Город: ", ifelse(is.na(city[i]), "Н/Д", city[i]), " | ",
            "ASN: ", ifelse(is.na(as_number[i]), "Н/Д", as_number[i]),
            "</span>",
            "</div>"
          )
        }), collapse = ""
      ),
      cities = paste(unique(na.omit(city)), collapse = ", "),
      states = paste(unique(na.omit(state)), collapse = ", "),
      countries = paste(unique(na.omit(country)), collapse = ", "),
      continents = paste(unique(na.omit(continent)), collapse = ", "),
      as_numbers = paste(unique(na.omit(as_number)), collapse = ", "),
      organizations = paste(unique(na.omit(as_organization)), collapse = ", "),
      all_ip_starts = paste(ip_start, collapse = "|"),
      all_ip_ends = paste(ip_end, collapse = "|"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      cities = ifelse(cities == "", "Н/Д", cities),
      states = ifelse(states == "", "Н/Д", states),
      countries = ifelse(countries == "", "Н/Д", countries),
      continents = ifelse(continents == "", "Н/Д", continents),
      as_numbers = ifelse(as_numbers == "", "Н/Д", as_numbers),
      organizations = ifelse(organizations == "", "Н/Д", organizations)
    )

  continent_palette <- leaflet::colorFactor(
    palette = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#FFA500"),
    domain = unique(grouped_data$continents)
  )

  map <- leaflet::leaflet(grouped_data) |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      popup = ~paste(
        "<div style='max-height: 400px; overflow-y: auto;'>",
        "<h4 style='margin-top: 0;'>",
        ifelse(count > 1, paste("Группа из", count, "IP-диапазонов"), "IP-диапазон"),
        "</h4>",

        "<div style='margin-bottom: 10px; padding: 8px; background: #e8f4f8; border-radius: 5px;'>",
        "<b>Общая информация:</b><br>",
        "<b>Координаты:</b> ", round(latitude, 4), "°, ", round(longitude, 4), "°<br>",
        "<b>Количество диапазонов:</b> ", count, "<br>",
        "<b>Города:</b> ", cities, "<br>",
        "<b>Регионы:</b> ", states, "<br>",
        "<b>Страны:</b> ", countries, "<br>",
        "<b>Континенты:</b> ", continents, "<br>",
        "<b>AS Numbers:</b> ", as_numbers, "<br>",
        "<b>Организации:</b> ", organizations,
        "</div>",

        "<hr style='margin: 10px 0;'>",
        "<h5>Детали IP-диапазонов:</h5>",
        ip_ranges_details,
        "</div>"
      ),
      radius = ~ifelse(count > 1, 8, 5),
      color = ~continent_palette(continents),
      fillOpacity = 0.7,
      stroke = TRUE,
      weight = 1,
      label = ~paste("Координаты:", round(latitude, 4), ",", round(longitude, 4),
                     " | Диапазонов:", count),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto"
      ),
      clusterOptions = leaflet::markerClusterOptions(
        spiderfyOnMaxZoom = TRUE,
        showCoverageOnHover = TRUE,
        zoomToBoundsOnClick = TRUE
      )
    ) |>
    leaflet::addLegend(
      "bottomright",
      pal = continent_palette,
      values = ~continents,
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
      plot.margin = ggplot2::margin(1, 1, 1, 1, "cm")
    )+
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.2))
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




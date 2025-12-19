load_processed_data <- function() {
  data_path <- "processed/dbip_data.parquet"

  if (!file.exists(data_path)) {
    stop("Файл с данными не найден. Сначала запустите ETL пайплайн.")
  }
  data <- arrow::read_parquet(data_path)
  message("Данные успешно загружены. Структура данных:")
  print(str(data))
  return(data)
}

get_data_stats <- function(data) {
  stats_list <- list(
    total_rows = nrow(data),
    total_continents = length(unique(data$continent)),
    total_countries = length(unique(data$country)),
    total_states = length(unique(data$state)),
    total_cities = length(unique(data$city)),
    column_names = colnames(data)
  )

  cat("----- СТАТИСТИКА ДАННЫХ DB-IP -----\n")
  cat("Всего строк:", stats_list$total_rows, "\n")
  cat("Уникальных континентов:", stats_list$total_continents, "\n")
  cat("Уникальных стран:", stats_list$total_countries, "\n")
  cat("Уникальных регионов/штатов:", stats_list$total_states, "\n")
  cat("Уникальных городов:", stats_list$total_cities, "\n")
  cat("Колонки:", paste(stats_list$column_names, collapse = ", "), "\n")

  return(stats_list)
}

# Создание карты
create_ip_map <- function(data) {

  map <- leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      popup = ~paste("IP Range:", ip_range, "<br>",
                     "Город:", city, "<br>",
                     "Регион:", state, "<br>",
                     "Страна:", country, "<br>",
                     "Континент:", continent, "<br>",
                     "AS:", as_number, "<br>",
                     "Организация:", as_organization),
      radius = 5,
      color = "blue",
      fillOpacity = 0.5
    )

  return(map)
}

# График распределения по странам
create_country_chart <- function(data) {

  country_counts <- data %>%
    dplyr::count(country, sort = TRUE) %>%
    head(10)

  p <- ggplot2::ggplot(country_counts, ggplot2::aes(x = reorder(country, n), y = n)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Топ-10 стран по количеству IP-адресов",
      x = "Страна",
      y = "Количество IP"
    ) +
    ggplot2::theme_minimal()

  return(p)
}

# Создание таблицы
create_data_table <- function(data, n = 500) {

  table_data <- head(data, n)

  table <- DT::datatable(
    table_data,
    options = list(
      pageLength = 25,
      scrollX = TRUE
    ),
    class = 'display compact'
  )
  return(table)
}


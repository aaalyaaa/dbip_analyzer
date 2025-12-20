# Create demo data

library(data.table)
library(arrow)
library(dplyr)

source("R/download.R")
source("R/process.R")

files <- download_dbip()
full_data <- process_dbip(files, output_dir = "temp_processed")

set.seed(42)

full_data_tbl <- as_tibble(full_data)

demo_unique <- full_data_tbl %>%
  group_by(as_number) %>%
  slice(1) %>%
  ungroup()

message("First records of each ASN: ", nrow(demo_unique))

demo_second <- full_data_tbl %>%
  group_by(as_number) %>%
  filter(n() >= 2) %>%
  slice(2) %>%
  ungroup()

message("Second records available: ", nrow(demo_second))

target <- 100000

if (nrow(demo_second) >= (target - nrow(demo_unique))) {
  need_more <- target - nrow(demo_unique)
  additional <- demo_second %>% slice_sample(n = need_more)
  demo <- bind_rows(demo_unique, additional)
} else {
  demo <- bind_rows(demo_unique, demo_second)
  message("Total records: ", nrow(demo))
}

demo <- demo %>% slice_sample(prop = 1)

demo <- as.data.table(demo)

message("\nColumn order: ", paste(names(demo), collapse = ", "))
message("Total rows: ", nrow(demo))
message("Unique ASN: ", length(unique(demo$as_number)))

usethis::use_data(demo, overwrite = TRUE)

unlink("temp_processed", recursive = TRUE)

cat("\n Demo dataset created with original column order\n")

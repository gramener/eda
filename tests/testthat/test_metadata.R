library(eda)
library(yaml)
context("Meta data")

test_that("Test if bank metadata and YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = "https://raw.githubusercontent.com/gramener/eda/master/data/samples/csv_files/bank.csv")),read_yaml("https://raw.githubusercontent.com/gramener/eda/master/data/samples/yaml_files/bank.csv.yaml"))
  }
)

test_that("Test if singapore-flat-prices metadata and YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = "https://raw.githubusercontent.com/gramener/eda/master/data/samples/csv_files/singapore-flat-prices.csv")),read_yaml("https://raw.githubusercontent.com/gramener/eda/master/data/samples/yaml_files/singapore-flat-prices.csv.yaml"))
}
)

test_that("Test if bank.xlsx metadata and YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = "https://raw.githubusercontent.com/gramener/eda/master/data/samples/xlsx_files/singapore-flat-prices.xlsx")),read_yaml("https://raw.githubusercontent.com/gramener/eda/master/data/samples/yaml_files/bank.xlsx.yaml"))
}
)

test_that("Test if singapore-flat-prices.xlsx metadata and YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = "https://raw.githubusercontent.com/gramener/eda/master/data/samples/xlsx_files/singapore-flat-prices.xlsx")),read_yaml("https://raw.githubusercontent.com/gramener/eda/master/data/samples/yaml_files/singapore-flat-prices.xlsx.yaml"))
}
)



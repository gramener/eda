library(eda)
library(yaml)
context("Meta data")

test_that("Test if bank metadata YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = paste("https://github.com/gramener/eda/tree/master/data/samples/csv_files","bank.csv",sep = "/"))),read_yaml(paste("https://github.com/gramener/eda/tree/master/data/samples/yaml_files/","bank.csv",".yaml",sep = "")))
  }
)


test_that("Test if barc-hair-care-brands metadata YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = paste("https://github.com/gramener/eda/tree/master/data/samples/csv_files","barc-hair-care-brands.csv",sep = "/"))),read_yaml(paste("https://github.com/gramener/eda/tree/master/data/samples/yaml_files/","barc-hair-care-brands.csv",".yaml",sep = "")))
}
)


test_that("Test if nas metadata YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = paste("https://github.com/gramener/eda/tree/master/data/samples/csv_files","nas.csv",sep = "/"))),read_yaml(paste("https://github.com/gramener/eda/tree/master/data/samples/yaml_files/","nas.csv",".yaml",sep = "")))
}
)


test_that("Test if per-growth-by-metric metadata YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = paste("https://github.com/gramener/eda/tree/master/data/samples/csv_files","per-growth-by-metric.csv",sep = "/"))),read_yaml(paste("https://github.com/gramener/eda/tree/master/data/samples/yaml_files/","per-growth-by-metric.csv",".yaml",sep = "")))
}
)


test_that("Test if singapore-flat-prices metadata YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = paste("https://github.com/gramener/eda/tree/master/data/samples/csv_files","singapore-flat-prices.csv",sep = "/"))),read_yaml(paste("https://github.com/gramener/eda/tree/master/data/samples/yaml_files/","singapore-flat-prices.csv",".yaml",sep = "")))
}
)


test_that("Test if telecom metadata YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = paste("https://github.com/gramener/eda/tree/master/data/samples/csv_files","telecom.csv",sep = "/"))),read_yaml(paste("https://github.com/gramener/eda/tree/master/data/samples/yaml_files/","telecom.csv",".yaml",sep = "")))

}
)

test_that("Test if vas metadata YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = paste("https://github.com/gramener/eda/tree/master/data/samples/csv_files","vas.csv",sep = "/"))),read_yaml(paste("https://github.com/gramener/eda/tree/master/data/samples/yaml_files/","vas.csv",".yaml",sep = "")))
}
)

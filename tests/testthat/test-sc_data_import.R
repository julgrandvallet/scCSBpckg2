# Set the path to the test files directory
test_dir_path <- "./tests/Test_Files/"

# Test for CSV files
test_that("sc_data_import correctly processes CSV files", {
  result <- sc_data_import(paste0(test_dir_path, "csv_test.csv"))
  expect_true(inherits(result, "Matrix"))
})

# Test for LOOM files
test_that("sc_data_import correctly processes LOOM files", {
  result <- sc_data_import(paste0(test_dir_path, "loom_test.loom"))
  expect_true(inherits(result, "SingleCellLoomExperiment"))
})

# Test for TXT files (expecting an error)
test_that("sc_data_import throws an error for unsupported TXT files", {
  expect_error(sc_data_import(paste0(test_dir_path, "txt_test.txt")), "Tipo de archivo no soportado.")
})

# Test for CellRanger directory
test_that("sc_data_import correctly processes CellRanger output", {
  result <- sc_data_import(paste0(test_dir_path, "cellRanger_test/"))
  # Assuming the output class is a SingleCellExperiment or similar
  expect_true(inherits(result, "SingleCellExperiment"))

})

# Test for H5AD files
test_that("sc_data_import correctly processes H5AD files", {
  result <- sc_data_import(paste0(test_dir_path, "h5ad_test.h5ad"))
  # Assuming the output class is a SingleCellExperiment or similar
  expect_true(inherits(result, "SingleCellExperiment"))
  # Add checks for dimensions and sample values if known
  # e.g., expect_equal(dim(result), c(expected_rows, expected_columns))
})

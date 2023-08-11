test_that("sc_data_import procesa correctamente archivos CSV", {
  result <- sc_data_import("./tests/Test_Files/csv_test.csv")
  # Verificar que result es de la clase o tipo esperado
  expect_true(inherits(result, "Matrix"))
  # Otros asserts para verificar contenido, dimensiones, etc.
})






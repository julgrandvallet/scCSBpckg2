test_that("sc_data_import procesa correctamente archivos CSV", {
  result <- sc_data_import("./tests/Test_Files/csv_test.csv")
  # Verificar que result es de la clase o tipo esperado
  expect_true(inherits(result, "Matrix"))
  # Otros asserts para verificar contenido, dimensiones, etc.
})

test_that("sc_data_import arroja un error para archivos no existentes", {
  expect_error(sc_data_import("./tests/Test_Files/inexistente.csv"), "El archivo especificado no existe.")
})

test_that("sc_data_import arroja un error para directorios con más o menos de 3 archivos", {
  expect_error(sc_data_import("./tests/Test_Files/"), "El directorio debe contener exactamente 3 archivos para ser leído con read10xCounts.")
})

test_that("sc_data_import corre correctamente", {
  expect_no_error(sc_data_import("./tests/Test_Files/csv_test.csv"))
})

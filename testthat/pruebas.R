test_that("sc_data_import procesa correctamente archivos CSV", {
  result <- sc_data_import("ruta/a/tu/archivo/test.csv")
  # Verificar que result es de la clase o tipo esperado
  expect_true(inherits(result, "alguna_clase_esperada"))
  # Otros asserts para verificar contenido, dimensiones, etc.
})

test_that("sc_data_import arroja un error para archivos no existentes", {
  expect_error(sc_data_import("ruta/inexistente.csv"), "El archivo especificado no existe.")
})

test_that("sc_data_import arroja un error para directorios con más o menos de 3 archivos", {
  expect_error(sc_data_import("ruta/a/directorio/con/4/archivos"), "El directorio debe contener exactamente 3 archivos para ser leído con read10xCounts.")
})

# Y así sucesivamente para los otros tipos de archivos y comportamientos esperados

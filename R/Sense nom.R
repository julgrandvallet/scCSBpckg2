#' Title
#'
#' @param serievector
#'
#' @return
#' @export
#'
#' @examples
#'

library(scuttle)

# Función auxiliar para detectar el delimitador
detect_delim <- function(file_path) {
  first_line <- readLines(file_path, n = 1)
  delimiters <- c(",", "\t", "|", " ")

  counts <- sapply(delimiters, function(delim) {
    sum(unlist(strsplit(first_line, split = "")) == delim)
  })

  return(delimiters[which.max(counts)])
}

sc_data_import <- function(file) {
  # Verifica si el archivo existe
  if (!file.exists(file)) {
    stop("El archivo especificado no existe.")
  }

  # Extrae la extensión del archivo
  file_extension <- tools::file_ext(file)

  # Decide cómo procesar el archivo según su extensión
  switch(file_extension,
         csv = {
           detected_sep <- detect_delim(file)
           return(scuttle::readSparseCounts(file, sep = detected_sep))
         },
         tsv = {
           detected_sep <- detect_delim(file)
           return(scuttle::readSparseCounts(file, sep = detected_sep))
         },
         h5ad = {
           return(zellkonverter::readH5AD(file))
         },
         loom = {
           return(LoomExperiment::import(file, type="SingleCellLoomExperiment"))
         },
         stop("Tipo de archivo no soportado.")
  )
}
csv1 <- sc_data_import("./Test_Files/csv_test.csv")
print(csv1)

h5ad1 <- sc_data_import("/home/juliangc/R/x86_64-pc-linux-gnu-library/4.3/zellkonverter/extdata/krumsiek11_augmented_v0-8.h5ad")
print(h5ad1)

loom1 <- sc_data_import("/home/juliangc/R/x86_64-pc-linux-gnu-library/4.2/SummarizedExperiment/extdata/example.loom")
print(loom1)

txt1 <- sc_data_import("./Test_Files/txt_test.txt")
print(txt1)

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
           demo <- system.file("extdata", file, package = "zellkonverter")
           return(readH5AD(demo))
         },
         loom = {
           demo <- system.file("extdata", file, package = "LoomExperiment")
           return(import(demo, type="SingleCellLoomExperiment"))
         },
         stop("Tipo de archivo no soportado.")
  )
}
primero <- sc_data_import("/home/juliangc/Videos/copy_results/exons_TPM.csv")
print(primero)

primero_j <- sc_data_import("/Users/jmm/Desktop/Creando paquetes de R:Bioconductor para análisis transcriptómicos de célula única/1 Estructura e importe de datos/data/GSE85241_cellsystems_dataset_4donors_updated.csv")
print(primero_j)

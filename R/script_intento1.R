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

sc_data_import <- function(file) {
  # Verifica si el archivo existe
  if (!file.exists(file)) {
    stop("El archivo especificado no existe.")
  }

  # Extrae la extensión del archivo
  file_extension <- tools::file_ext(file)

  # Función auxiliar para detectar el delimitador
  detect_delim <- function(file_path) {
    first_line <- readLines(file_path, n = 1)
    delimiters <- c(",", "\t", "|", " ")

    counts <- sapply(delimiters, function(delim) {
      sum(unlist(strsplit(first_line, split = "")) == delim)
    })

    return(delimiters[which.max(counts)])
  }

  # Si es un .csv o .tsv, detecta el delimitador y luego usa scuttle::readSparseCounts para leerlo
  if (file_extension == "csv" || file_extension == "tsv") {
    detected_sep <- detect_delim(file)
    data <- scuttle::readSparseCounts(file, sep = detected_sep)
    return(data)
  } else if (file_extension == "h5ad") {
    demo <- system.file("extdata", file, package = "zellkonverter")
    sce <- readH5AD(demo)
    return(sce)
  } else if (file_extension == "loom") {
    demo <- system.file("extdata", file, package = "LoomExperiment")
    scle <- import(demo, type="SingleCellLoomExperiment")
    return(scle)
  } else {
    stop("Tipo de archivo no soportado.")
  }
}



primero <- sc_data_import("/home/juliangc/Videos/copy_results/exons_TPM.csv")

print(primero)

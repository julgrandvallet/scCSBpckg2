#' @Title scCSBpckg2
#'
#' @param serievector
#'
#' @return A matrix containing the data from the CSV, loom, h5ad scRNA files.
#'
#' @examples
#' #csv1 <- sc_data_import("./tests/Test_Files/csv_test.csv")
#' #print(csv1)

#' #loom1 <- sc_data_import("./tests/Test_Files/loom_test.loom")
#' #print(loom1)

#' #h5ad1 <- sc_data_import("./tests/Test_Files/h5ad_test.h5ad")
#' #print(h5ad1)

#' #txt <- sc_data_import("./tests/Test_Files/txt_test.txt")
#' #print(txt)

#' #tenx <- sc_data_import("./tests/Test_Files/cellRanger_test/")
#' #print(tenx)
#'


# Funcion auxiliar para detectar el delimitador
detect_delim <- function(file_path) {
  first_line <- readLines(file_path, n = 1)
  delimiters <- c(",", "\t", "|", " ")

  counts <- sapply(delimiters, function(delim) {
    sum(unlist(strsplit(first_line, split = "")) == delim)
  })

  return(delimiters[which.max(counts)])
}

#Funcion para importar datos
sc_data_import <- function(file_or_dir) {
  # Verifica si el archivo o directorio existe
  if (!file.exists(file_or_dir)) {
    stop("El archivo especificado no existe.")
  }

  # Verifica si es un directorio
  is_dir <- file.info(file_or_dir)$isdir
  if (isTRUE(is_dir)) {
    files_in_dir <- list.files(file_or_dir, recursive = FALSE)

    if (length(files_in_dir) != 3) {
      stop("El directorio debe contener exactamente 3 archivos para ser leido con read10xCounts.")
    }

    return(DropletUtils::read10xCounts(file_or_dir))
  }

  # Extrae la extension del archivo
  file_extension <- tools::file_ext(file_or_dir)

  # Decide como procesar el archivo segun su extension
  switch(file_extension,
         csv = {
           detected_sep <- detect_delim(file_or_dir)
           return(scuttle::readSparseCounts(file_or_dir, sep = detected_sep))
         },
         tsv = {
           detected_sep <- detect_delim(file_or_dir)
           return(scuttle::readSparseCounts(file_or_dir, sep = detected_sep))
         },
         h5ad = {
           return(zellkonverter::readH5AD(file_or_dir))
         },
         loom = {
           return(LoomExperiment::import(file_or_dir, type="SingleCellLoomExperiment"))
         },
         stop("Tipo de archivo no soportado. El archivo debe ser csv, tsv, h5ad, loom o cellRanger.")
  )
}

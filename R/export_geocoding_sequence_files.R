#' Create files for batchgeocoding.
#'
#' Convert a large data frame containing and address column
#' into multiple files for batch geocoding
#'
#' @param addresses a \code{data.frame} containing one column named address and additional columns as desired.
#' @param file_path a \code{character} destination for sequence files and the run.bat used to execute docker based geocoding
#' @param max_rows_per_file a \code{integer} specifying the maximum number of rows per file to be geocoded.
#'
#' @export
#'
#' @import dplyr
#' @import stringr

export_geocoding_sequence_files <- function(addresses,
                                  file_path = "c:/temp/geocode/",
                                  max_rows_per_file = 50000){

  run_file <- paste0(file_path, " run.bat")
  dir.create(file_path, showWarnings = F)
  unlink(run_file)

  addresses <- addresses %>%
    dplyr::arrange(.data$address) %>%
    dplyr::mutate(sequence =
             str_pad(row_number() %/% max_rows_per_file, 3, pad = "0"))

  addresses_by_sequence <- split(addresses, addresses$sequence)

  sequence_prefixes <- names(addresses_by_sequence)
  for(prefix in sequence_prefixes) {
    file_name <- paste0(file_path, prefix, '.csv')
    docker_command <- paste0("docker run --rm=TRUE ",
                             "-v ", file_path, ":/tmp degauss/cchmc_batch_geocoder ",
                             paste0(prefix, '.csv'))
    write(docker_command,
          run_file,
          append = T)
    readr::write_csv(addresses_by_sequence[[prefix]],
              file = file_name)
  }
}

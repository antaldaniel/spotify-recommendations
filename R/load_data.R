

load_data <- function ( variable_name = NULL,
                        data_path,
                        envir = NULL ) {

  if ( is.null(envir)) {
    envir <- parent.env( environment())
  }

  if ( !is.null(data_path) ) {

    if ( ! fs::file_exists(data_path) ) {
      stop ( "The Listen Local data file", data_path, " cannot be found.")
    }

    if ( fs::path_ext( data_path ) == 'rds' ) {
      if (is.null(variable_name)) stop("With rds files the variable name must be given as a parameter.")
      assign(variable_name, readRDS(data_path), envir = envir)
    }

    if ( fs::path_ext( data_path ) == 'rda' ) {
      load(data_path, envir = envir)  # load to the parent environment
    }

    if ( fs::path_ext( data_path ) %in% c('xls', 'xlsx') ) {

      if (is.null(variable_name)) stop("With xls* files the variable name must be given as a parameter.")
      assign(variable_name, readxl::read_excel(data_path), envir = envir)
    }

    if ( fs::path_ext( data_path ) == 'csv' ) {
      if (is.null(variable_name)) stop("With csv files the variable name must be given as a parameter.")
      assign(variable_name, read.csv(data_path), envir = envir)
    }
  } else {
   NULL
  }
}

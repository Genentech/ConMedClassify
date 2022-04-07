#' Convert a S4Vectors::DataFrame to a tibble
#'
#' @md
#' @param d input `data.frame` of clinical data
#' @param add_rownames (`logical`) should rownames be added?
#'
#' @import rlang
#' @importFrom tibble as_tibble add_column
#'
#' @details
#' This function carefully deals with nested domains
#'
#' @return a `tibble` version of `d`
#' @export
DF_to_tibble <- function(d, add_rownames = FALSE) {
  ## simple conversion
  data_tbl <- suppressWarnings(tibble::as_tibble(d))
  row_names <- rownames(d)

  ## identify list cols
  list_cols <- which(lapply(data_tbl, class) %in% c("list", "AsIs"))

  ## convert to actual lists
  for (l in list_cols) {
    data_tbl[[names(data_tbl)[l]]] <- as.list(data_tbl[[names(data_tbl)[l]]])
    class(data_tbl[[names(data_tbl)[l]]]) <- "list"
  }

  if (!is.null(row_names) && add_rownames) {
    if (rlang::has_name(data_tbl, "sample_id") && identical(row_names, data_tbl$sample_id)) {
      NULL
    } else {
      data_tbl <- tibble::add_column(data_tbl, sample_id = row_names, .before = 1)
    }
  }

  data_tbl
}


#' Unnest a nested domain
#'
#' Specific to this structure of nested data, but otherwise similar to the `tidyr` version
#'
#' @md
#' @param data input clinical data with nested column(s)
#' @param cols column to unnest
#' @param .drop passed to `tidyr::unnest_legacy`
#' @param .id passed to `tidyr::unnest_legacy`
#' @param .sep passed to `tidyr::unnest_legacy`
#' @param .preserve passed to `tidyr::unnest_legacy`
#'
#' @import dplyr
#' @import rlang
#' @importFrom tidyr unnest_legacy
#' @importFrom purrr map_lgl
#'
#' @return input data with the requested column unnested
#' @export
unnest_domain <- function(data, cols, .drop = NA, .id = NULL, .sep = NULL, .preserve = NULL) {

  domain <- as.character(rlang::enexpr(cols))
  length(domain) > 0 || stop("A domain to unnest is required", call. = FALSE)
  length(domain) == 1L || stop("Only one domain can be unnested at a time", call. = FALSE)

  if (!grepl("nested.", domain, fixed = TRUE)) domain <- paste0("nested.", domain)
  domain %in% names(data) || stop("Unable to locate ", domain, " column in ", substitute(data), call. = FALSE)

  ## convert to tibble
  data_tbl <- DF_to_tibble(data)

  ## drop any NULL rows
  data_tbl <- dplyr::filter(data_tbl, !purrr::map_lgl(data_tbl[[unlist(domain)]], is.null))

  data_unnested <- tidyr::unnest_legacy(data_tbl, !!rlang::sym(domain), .drop = .drop, .id = .id, .sep = .sep, .preserve = .preserve)

  return(data_unnested)

}


#' Extract a PARAMCD-AVAL style key-value entries from nested data
#'
#' @md
#' @param d input `data.frame` of clinical data
#' @param nested.X nested column
#' @param param.code PARAMCD entry to which results should be filtered
#' @param val.col column containing the value to be extracted, corresponding to the PARAMCD
#' @param col.name column name to use for the extracted value
#'
#' @import dplyr
#' @import rlang
#'
#' @return a `data.frame` of extracted data
#' @export
extract_paramcd <- function(d, nested.X, param.code, val.col, col.name = NA) {

  ## unnest_domain and extract a given parameter
  colSelected <- d %>%
    unnest_domain(!!rlang::sym(nested.X)) %>%
    dplyr::filter(PARAMCD == param.code) %>%
    dplyr::select(STUDYID, USUBJID, !!rlang::sym(val.col))

  if (is.na(col.name)) {
    colnames(colSelected)[3] = val.col
  } else{
    colnames(colSelected)[3] = col.name
  }
  return(colSelected)
}


# extract_domain_paramcd <- function(d, nested.X, param.code, val.type = "auto") {
#   ## Automatically determine type of value.
#   if (val.type == "auto") {
#     aval.data = extract_paramcd(d, nested.X, param.code, "AVAL")
#     val.type = "numeric"
#     if (sum(is.na(aval.data$AVAL)) == dim(aval.data)[1]) {
#       val.type = "character"
#     }
#   } else if(val.type == "numeric") {
#     return(extract_paramcd(d, nested.X, param.code, "AVAL", param.code))
#   } else if(val.type == "character") {
#     ## Turn blank strings to NAs. Saves problems with factor conversion in
#     ## downstream processing steps.
#     avalc.table = extract_paramcd(d, nested.X, param.code, "AVALC", param.code)
#     missing.idx <- which(avalc.table[,param.code] == "")
#     if(length(missing.idx) > 0) {
#       avalc.table[missing.idx, param.code] = NA
#     }
#     return(avalc.table)
#   }
# }

#' Get package contents
#'
#' This function retrieves the contents of a package, including the names and types of exported objects.
#'
#' @param pkg_names A character vector of package names.
#' @return A tibble with columns for the package name, function name, and function type.
#' @export
get_package_contents <- function(pkg_names) {
  return_df <- tibble::tibble()
  for (pkg_name in pkg_names) {
    ns <- getNamespace(pkg_name)
    export_types <- sapply(ls(ns), function(x) {
      x_class <- class(ns[[x]])[1]
      if (x_class == "function") {
        if (sloop::is_s3_generic(x, ns)) {
          "S3 generic"
        } else if (sloop::is_s3_method(x, ns)) {
          "S3 method"
        } else {
          "function"
        }
      } else if (x_class == "standardGeneric") {
        "S4 generic"
      } else if (x_class == "nonstandardGenericFunction") {
        "S4 generic (non-standard)"
      } else {
        paste(class(ns[[x]]), collapse = ", ")
      }
    })
    return_df <- return_df |>
      dplyr::bind_rows(
        tibble::tibble(
          pkg_name = pkg_name,
          func_name = names(export_types),
          func_type = export_types
        )
      )
  }
  return(return_df)
}

#' Filter function objects
#'
#' This function filters the exported objects of a package to include only functions and S3/S4 methods/generics.
#'
#' @param pkg_exports A tibble of package exports, as returned by get_package_contents().
#' @return A filtered tibble of package exports.
#' @export
filter_func_objs <- function(pkg_exports) {
  pkg_exports |>
    dplyr::filter(
      func_type %in% c(
        "function", "S3 generic", "S3 method",
        "S4 generic", "S4 generic (non-standard)"
      )
    )
}

#' Get function calls
#'
#' This function retrieves the function calls within a specified function in a package.
#'
#' @param pkg A character string specifying the package name.
#' @param func A character string specifying the function name.
#' @return A tibble of function calls within the specified function.
#' @export
get_function_calls <- function(pkg, func) {
  func_def <- getNamespace(pkg)[[func]]
  pkg_exports <- get_package_contents(pkg) |>
    filter_func_objs()
  pkg_exports |>
    dplyr::filter(func_name %in% codetools::findGlobals(func_def))
}

#' Get all function dependencies
#'
#' This function retrieves all function dependencies within a specified package.
#'
#' @param pkg A character string specifying the package name.
#' @return A tibble of function dependencies within the specified package.
#' @export
get_all_function_deps <- function(pkg) {
  pkg_exports <- get_package_contents(pkg) |>
    filter_func_objs()
  func_relations <- purrr::map_df(pkg_exports$func_name, ~ tibble::tibble(
    from = .x,
    to = get_function_calls(pkg, .x)$func_name
  ), .progress = TRUE)
  return(func_relations)
}


#' Build visualization network
#'
#' This function builds a visualization network for a specified package.
#'
#' @param pkg A character string specifying the package name.
#' @return A visNetwork object representing the visualization network.
#' @export
build_vis_network <- function(pkg) {
  pkg_funcs <- get_package_contents(pkg) |>
    filter_func_objs()
  func_deps <- get_all_function_deps(pkg)
  nodes <- pkg_funcs |>
    dplyr::rename(id = func_name, group = func_type) |>
    dplyr::mutate(label = id)
  edges <- func_deps
  visNetwork::visNetwork(nodes, edges) |>
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) |>
    visNetwork::visLegend()
}

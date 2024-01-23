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

#' Get R6 methods
#'
#' This function retrieves the methods of R6 classes within a specified package.
#'
#' @param pkg A character string specifying the package name.
#' @return A tibble of R6 methods within the specified package.
#' @export
get_r6_methods <- function(pkg) {
  ns <- getNamespace(pkg)
  purrr::map_df(
    get_package_contents(pkg) |>
      dplyr::filter(func_type == "R6ClassGenerator") |>
      dplyr::pull(func_name),
    function(x, y) {
      public_methods <- names(ns[[x]]$public_methods)
      public_methods <- public_methods[!public_methods %in% c("initialize", "clone")]
      private_methods <- names(ns[[x]]$private_methods)
      tibble::tibble(
        class_name = x,
        method = c(public_methods, private_methods),
        type = c(rep("public", length(public_methods)), rep("private", length(private_methods)))
      )
    }
  )
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


#' Get R6 method calls
#'
#' This function retrieves the method calls within a specified R6 method in a package.
#'
#' @param pkg A character string specifying the package name.
#' @param r6_class A character string specifying the R6 class name.
#' @param r6_method A character string specifying the R6 method name.
#' @param method_type A character string specifying the method type (public or private).
#' @return A tibble of method calls within the specified R6 method.
#' @export
get_r6_method_calls <- function(pkg, r6_class, r6_method, method_type = "public") {
  r6_def <- getNamespace(pkg)[[r6_class]]
  func_def <- r6_def[[paste0(method_type, "_methods")]][[r6_method]]
  pkg_exports <- get_package_contents(pkg) |>
    filter_func_objs()
  pkg_exports |>
    dplyr::filter(func_name %in% codetools::findGlobals(func_def))
}

#' Get all R6 method dependencies
#'
#' This function retrieves all R6 method dependencies within a specified package.
#'
#' @param pkg A character string specifying the package name.
#' @return A tibble of R6 method dependencies within the specified package.
#' @export
get_all_r6_method_deps <- function(pkg) {
  r6_methods <- get_r6_methods(pkg)
  method_relations <- purrr::pmap_df(r6_methods, function(class_name, method, type) {
    internal_calls <- get_r6_method_calls(pkg, class_name, method, type)
    if (nrow(internal_calls) > 0) {
      tibble::tibble(
        from = paste0(class_name, "::", method),
        to = internal_calls$func_name
      )
    } else {
      NULL
    }
  }, .progress = TRUE)
  return(method_relations)
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
  s3_generics <- pkg_funcs |>
    dplyr::filter(func_type == "S3 generic") |>
    dplyr::pull(func_name)
  if (length(s3_generics) > 0) {
    s3_methods <- pkg_funcs |>
      dplyr::filter(func_type == "S3 method") |>
      dplyr::pull(func_name)
    s3_edges <- purrr::map_df(s3_generics, function(s3_generic) {
      method_vector <- as.character(.S3methods(s3_generic, envir = getNamespace(pkg)))
      tibble::tibble(from = s3_generic, to = method_vector)
    }) |>
      dplyr::filter(to %in% s3_methods)
    edges <- dplyr::bind_rows(edges, s3_edges)
  }
  r6_methods <- get_r6_methods(pkg)
  if (nrow(r6_methods) > 0) {
    r6_classes <- tibble::tibble(
      pkg_name = pkg,
      id = unique(r6_methods$class_name),
      group = "R6 class",
      label = unique(r6_methods$class_name)
    )
    r6_class_edges <- tibble::tibble(
      from = r6_methods$class_name,
      to = paste0(r6_methods$class_name, "::", r6_methods$method)
    )
    r6_methods <- r6_methods |>
      dplyr::transmute(
        pkg_name = pkg,
        id = paste0(class_name, "::", method),
        label = method,
        group = paste0("R6 method (", type, ")")
      )
    r6_deps <- get_all_r6_method_deps(pkg)
    nodes <- nodes |>
      dplyr::bind_rows(r6_classes) |>
      dplyr::bind_rows(r6_methods)
    edges <- edges |>
      dplyr::bind_rows(r6_class_edges) |>
      dplyr::bind_rows(r6_deps)
  }
  network <- visNetwork::visNetwork(nodes, edges, width = "100%", height = "960px") |>
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) |>
    visNetwork::visLegend() |>
    visNetwork::visEdges(arrows = "to")
  network$x$options$groups <- list(
    "function" = list(color = "#4ba3e2"),
    "S3 generic" = list(color = "#f2e534"),
    "S3 method" = list(color = "#2cd6ac"),
    "S4 generic" = list(color = "#f2a756"),
    "S4 generic (non-standard)" = list(color = "#f2a756"),
    "R6 class" = list(color = "#9f75f2"),
    "R6 method (public)" = list(color = "#69f177"),
    "R6 method (private)" = list(color = "#f16363")
  )
  return(network)
}

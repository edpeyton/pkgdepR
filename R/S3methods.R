
#' @title pkgdepR object
#' @description An internal generic function. Methods for \code{is.pkgdepR} should only return \code{TRUE} if the class is \code{pkgdepR}.
#' @param x Object to be tested.
#'
#' @return Boolean. \code{TRUE} when \code{x} is of class \code{pkgdepR}.
is.pkgdepR = function(x) {
  
  inherits(x, "pkgdepR")
  
}

#' @title Visualizing package dependencies
#' @description This function is a simple wrapper for plotting a network visualization using \link[visNetwork]{visNetwork}.
#' @param x An object of class \code{pkgdepR}.
#' @param width The width of the vis.js render.
#' @param height The height of the vis.js render.
#' @param main The title. To remove the title, pass \code{list(text = NULL)}.
#' @param submain The subtitle. To remove the subtitle, pass \code{list(text = NULL)}.
#' @param alpha A transparency value to use for colors. Must be between 0 and 1.
#' @param ... Other arguments passed onto \link[visNetwork]{visNetwork} and  \link[viridisLite]{viridis}.
#' @examples
#' library(pkgdepR)
#' deps(pkg = "pkgdepR") %>% plot(option = "E", direction = -1)
#' deps(pkg = "pkgdepR") %>% plot(option = "D", alpha = 0.5, main = list(text = NULL))
#' deps(pkg = "pkgdepR") %>% 
#'   plot(option = "D", alpha = 0.8, main = list(text = NULL)) %>% 
#'   visNetwork::visInteraction(dragNodes = FALSE)
#' @return An object of classes \code{visNetwork} and \code{htmlwidget}.
#' @export
plot.pkgdepR = function(x,
                        width = NULL, 
                        height = NULL, 
                        main = NULL,
                        submain = NULL, 
                        alpha = 0.8,
                        ...) {
  
  if (is.null(main)) {
    main = list(text = "Package function network<br></br>", 
                style = "font-family:Helvetica, Helvetica, Times, serif; font-size:24px; text-align:left;")
  }
  
  if (is.null(submain)) {
    submain = list(text = paste0("<strong>Packages: [", paste0(x$pkg, collapse = "], ["), "]</strong><br></br>"), 
                   style = "font-family:Helvetica, Helvetica, Times, serif; font-size:16px; text-align:left;")
  }
  
  alpha = max(min(alpha, 1), 0)
  
  cols = grDevices::col2rgb(viridisLite::viridis(n = length(unique(x$funs$package)), ...))
  cols = apply(cols, 2, FUN = function(x, alpha) {return(paste0("rgba(", paste0(x, collapse = ", "), ", ", alpha, ")"))}, alpha = alpha)
  cols = data.frame(package = unique(x$funs$package), color = cols)
  
  x$funs = x$funs %>% dplyr::left_join(cols, by = "package")
  
  x$funs$shape = "dot"
  x$funs$shape[!x$funs$exported] = "square"
  
  visNetwork::visNetwork(x[[1]], x[[2]], width = width, height = height, main = main, submain = submain, ...) %>%
    visNetwork::visEdges(arrows = "from", color = list(color = "black", opacity = 0.6), arrowStrikethrough = FALSE) %>%
    visNetwork::visNodes() %>%
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, hover = FALSE),
                           nodesIdSelection = list(main = "Select function",
                                                   style = "width: 280px; height: 26px;"),
                           selectedBy = list(variable = "group",
                                             main = "Select package",
                                             style = "width: 280px; height: 26px;")) %>%
    visNetwork::visInteraction(dragNodes = TRUE, hover = TRUE) %>%
    visNetwork::visPhysics(solver = "repulsion", stabilization = list(enabled = TRUE, iterations = 4000, onlyDynamicEdges = FALSE))
  
}


#' @title Print pkgdepR object
#' @param x An object of class \code{pkgdepR}.
#' @param ... Redundant argument for consistency with method.
#' @return No return value.
#' @export
print.pkgdepR = function(x, ...) {
  
  id = NULL
  package = NULL
  pkg_from = NULL
  pkg_to = NULL
  
  y = x$links %>% 
    dplyr::left_join(x$funs %>% dplyr::select(id, package) %>% dplyr::rename(pkg_from = package), by = c("from" = "id")) %>% 
    dplyr::left_join(x$funs %>% dplyr::select(id, package) %>% dplyr::rename(pkg_to   = package), by = c("to"   = "id"))
  
  dim1 = y %>% dplyr::filter(pkg_from != pkg_to) %>% dim()
  dim2 = y %>% dplyr::filter(pkg_from == pkg_to) %>% dim()
  
  cat("\npkgdepR object\n")
  cat(paste0(paste0(rep("-", 30), collapse = ""), "\n"))
  cat(paste0("Packages:    ", paste0(x$pkg, collapse = ", ")))
  cat(paste0("\nTotal nodes: ", scales::comma(length(unique(x$funs$id)))))
  cat(paste0("\nTotal links: ", scales::comma(dim(x$links)[1])))
  cat(paste0("\n  -Between packages: ", scales::comma(dim1[1])))
  cat(paste0("\n  -Within packages:  ", scales::comma(dim2[1])))
  cat(paste0("\n    --Between functions: ", scales::comma(dim2[1] - dim(x$links[x$links$from==x$links$to, ])[1])))
  cat(paste0("\n    --Self-referential:  ", scales::comma(dim(x$links[x$links$from==x$links$to, ])[1])))
  cat("\n\n")
  
}


#' @title Summarise pkgdepR object
#' @param object An object of class \code{pkgdepR}.
#' @param ... Redundant argument for consistency with method.
#' @return No return value.
#' @export
summary.pkgdepR = function(object, ...) {
  
  print.pkgdepR(x = object, ...)
  
}


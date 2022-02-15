


#' @title Get environment dependency object
#' @description This function creates an object of class \code{pkgdepR}, representing the network of function calls between one or more packages.
#' @param pkg A character vector of the environments to explore. Should be on the \link[base]{search} path. Cannot be \code{base}. 
#' @param exported_only Boolean. Whether to include non-exported functions.
#' @examples
#' library(pkgdepR)
#' deps(pkg = "pkgdepR")
#' @return An object of class \code{pkgdepR}.
#' @details An object of class \code{pkgdepR} is a list with three named objects:
##' \itemize{
##'  \item{\strong{\code{funs}}}{: a data frame describing the functions. Contains columns \code{id}, \code{label}, \code{package}, \code{exported}, \code{group}, and \code{name}.}
##'  \item{\strong{\code{links}}}{: a data frame containing the linkages between functions. Contains columns \code{from} and \code{to}.}
##'  \item{\strong{\code{pkg}}}{: a character vector containing the packages explored.}
##' }
#' @export
deps = function(pkg, exported_only = FALSE) {
  
  from = NULL
  to = NULL
  label = NULL
  package = NULL
  imported = NULL
  exported = NULL
  old_id = NULL
  
  pkg = unique(pkg)
  
  pkg = setdiff(pkg, "base")
  
  if (any(!pkg %in% .packages())) {
    stop("All packages in 'pkg' must be on the search path. Ensure you load the required packages.")
  }
  
  if (!length(pkg) > 0) {
    
    x = list(funs = data.frame(id = 0, label = "NULL", package = "NULL", exported = 0, group = "NULL", name = "NULL"),
             links = data.frame(from = 0, to = 0),
             pkg = "NULL")
    class(x) = "pkgdepR"
    return(x)
    
  }
  
  vis = list()
  for (i in pkg) {  
      
    name.functions = sort(pkgdepR::get_functions(i))
    
    non_exported = setdiff(name.functions, ls(pkgdepR::all_info_ns(i)$exports))
    
    if (exported_only) {
      name.functions = setdiff(name.functions, non_exported)
      non_exported = character(0)
    }
    
    if (length(name.functions) > 1) {
      calls = get_links(paste0("package:", i), funs = name.functions)
    } else {
      calls = NULL
    }
    
    if (length(name.functions) == 0) {
      name.functions = NULL
    }
    
    if (!is.null(dim(vis$funs)[1])) {
      n = max(vis$funs$id %>% as.numeric())
    } else {
      n = 0
    }
    
    vis0 = convert_to_visnetwork(unique(calls), name.functions)
    vis0$funs = vis0$funs %>% dplyr::mutate(id = as.character(as.numeric(id) + n)) %>% dplyr::mutate(package = i)
    vis0$links = vis0$links %>% dplyr::mutate(from = from + n, to = to + n)
    
    vis0$funs = vis0$funs %>% dplyr::mutate(exported = 1)
    
    if (length(non_exported)) {
      
      vis0$funs$exported[(vis0$funs$label %in% non_exported) & (vis0$funs$package == i)] = 0
      
    }
    
    vis0$funs$label = paste0(vis0$funs$package, "::", vis0$funs$label)
    
    vis$funs = rbind(vis$funs, vis0$funs)
    
    vis$links = rbind(vis$links, vis0$links)
    
  }
  
  
  if (dim(vis$funs %>% dplyr::select(-id) %>% dplyr::distinct())[1]<dim(vis$funs)[1]) {
    
    temp = vis$funs %>% dplyr::group_by(label, package, imported, exported) %>% dplyr::filter(dplyr::n() > 1)
    id = min(temp$id %>% as.numeric())
    
    vis$funs = vis$funs[-setdiff(temp$id %>% as.numeric(), id), ]
    vis$links$from[vis$links$from %in% setdiff(temp$id %>% as.numeric(), id)] = id
    vis$links$to[vis$links$to %in% setdiff(temp$id %>% as.numeric(), id)] = id
    
    vis$links = unique(vis$links)
    
  }
  
  
  if (length(pkg) > 1) {
    
    combns = utils::combn(pkg, 2)
    
    for (i in 1:dim(combns)[2]) {
        
      pkgs = combns[, i] %>% as.character()
      
      name.functions1 = pkgdepR::get_functions(pkgs[1])
      name.functions2 = pkgdepR::get_functions(pkgs[2])
 
      name.functions = union(name.functions1, name.functions2)

      non_exported1 = setdiff(name.functions1, ls(pkgdepR::all_info_ns(pkgs[1])$exports))
      non_exported2 = setdiff(name.functions2, ls(pkgdepR::all_info_ns(pkgs[2])$exports))
      
      imports1 = pkgdepR::all_info_ns(pkgs[1])$imports
      imports1 = imports1[names(imports1) %in% pkgs[2]] %>% unlist() %>% unique()
      
      imports2 = pkgdepR::all_info_ns(pkgs[2])$imports
      imports2 = imports2[names(imports2) %in% pkgs[1]] %>% unlist() %>% unique()
      
      imports = union(as.character(imports1), as.character(imports2))
        
      name.functions = sort(union(name.functions, imports))
        
      if (length(name.functions) > 1) {
        calls = get_links(paste0("package:", combns[, i]), funs = name.functions)
      } else {
        calls = NULL
      }
        
      if (!is.null(calls)) {
        
        vis0 = convert_to_visnetwork(unique(calls), unique(union(calls$master, calls$slave)))
        
        vis0$funs$package = sapply(vis0$funs$label, function(x) {unlist(strsplit(x, "::"))[1]}) %>% as.character()
        
        vis0$funs = vis0$funs %>% dplyr::rename(old_id = id) %>% dplyr::left_join(vis$funs, by = c("label", "package"))
        
        vis0$links = vis0$links %>% 
          dplyr::mutate(from = as.character(from)) %>% 
          dplyr::left_join(vis0$funs %>% dplyr::select(old_id, id), by = c("from" = "old_id")) %>% 
          dplyr::mutate(from = id) %>%
          dplyr::select(-id)
        
        vis0$links = vis0$links %>% 
          dplyr::mutate(to = as.character(to)) %>% 
          dplyr::left_join(vis0$funs %>% dplyr::select(old_id, id), by = c("to" = "old_id")) %>% 
          dplyr::mutate(to = id) %>%
          dplyr::select(-id)
        
        vis0$funs = vis0$funs %>% dplyr::select(-old_id)
        
        vis$links = rbind(vis$links, vis0$links)
        
      }
      
    }
    
    
  }
  
  vis$links = vis$links %>% dplyr::mutate(from = as.character(from), to = as.character(to))
  vis$funs = vis$funs %>% dplyr::mutate(group = package, name = id)
  vis$funs$label[!vis$funs$exported] = gsub("::", ":::", vis$funs$label[!vis$funs$exported])
  
  vis$pkg = pkg
  class(vis) = "pkgdepR"
  return(vis)
  
}



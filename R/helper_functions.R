

#' @title Get functions from namespace
#'
#' @param ns A character representing the namespace to explore.
#'
#' @return A character vector of function names.
#' @export
get_functions = function(ns) {

  names(asNamespace(ns))[sapply(names(asNamespace(ns)), function(j, k) {get(j, envir = as.environment(asNamespace(k))) %>% is.function()}, k = ns)]
  
}

#' @title Get namespace information
#'
#' @param ns A character representing the namespace to explore.
#' @param ... Other arguments passed to \link[base]{ls}.
#'
#' @return A character vector of the names to search for a given namespace.
#' @export
ls_namespace_info = function(ns, ...) {
  
  ns = asNamespace(ns, base.OK = FALSE)
  ls(..., envir = get(".__NAMESPACE__.", envir = ns, inherits = FALSE))
  
}

#' @title Get all information on a namespace
#'
#' @param ns A character representing the namespace to explore.
#'
#' @return A named list.
#' @export
all_info_ns = function(ns) {
  sapply(pkgdepR::ls_namespace_info(ns), getNamespaceInfo, ns = ns)
}


get_links = function(envir, funs) {
  
  mapping = build_mapping(envir = envir, funs)
  
  ms = apply(mapping, 1, function(x) {
    names(which(x == 1))
  })
  
  ms = data.frame(master = rep(names(ms), as.vector(unlist(lapply(ms, length)))), slave = unlist(ms, use.names = F))
  
  if (length(ms) < 2) {
    
    return(NULL)
    
  } else {
    
    ms = data.frame(ms)
    return(ms)
    
  }
  
}


get_mapping = function(where) {
  
  pkgs = sapply(where, function(x) {unlist(strsplit(x, ":"))[2]}) %>% as.character()
  
  name.functions = union(paste0(pkgs[1], "::", pkgdepR::get_functions(pkgs[1])),
                         paste0(pkgs[2], "::", pkgdepR::get_functions(pkgs[2])))
  
  n = length(name.functions)
  matrix(0, n, n, dimnames = list(MASTER = name.functions, SLAVE = name.functions))
  
}


build_mapping = function(envir, funs) {
  
  if (length(envir) == 1) {
    
    if (length(funs) > 1) {
      
      n = length(funs)
      
      mapping = matrix(0, n, n, dimnames = list(MASTER = funs, SLAVE = funs))
      
      master = lapply(funs, get_intra, choices = funs, where = envir)
      
      num_master = unlist(lapply(master, length))
      
      setup = c(rep(1:length(funs), num_master), unlist(master))
      dim(setup) = c(sum(num_master), 2)
      
      mapping[setup] = 1
      
      return(mapping)
      
    } else {
      
      return(NULL)
      
    }
    
  } else if (length(envir)==2) {
    
    mapping = get_mapping(envir)
    
    for (i in funs) {
      mapping = get_inter(i, choices = funs, where = envir, mapping = mapping)
    }
    
    return(mapping)
    
  } else {
    
    return(NULL)
    
  }
  
}



get_intra = function(fun_name, choices, where) {

  where = as.list(where)
    
  which = unlist(lapply(where, exists, x = fun_name), use.names = FALSE)
  
  if (!any(which)) {
    
    if (exists(fun_name)) {
      
      f = get(fun_name)
      
    } else {
      
      f = list()
      
    }
    
  } else {
    
    f = get(fun_name, pos = where[[seq_along(which)[which][1]]])
    
  }
  
  fun_list = get_fun_parts(f)
  
  if (!length(fun_list)) {
    
    return(numeric(0))
    
  }
    
  x = match(fun_list, choices, nomatch = 0)

  for (i in 1:length(x)) {
    
    if (x[i]>0) {
      
      if (any(fun_list[i-1] %in% c("=", "<-", "<<-"))) {
        
        x[i] = 0
        
      }
      
      if (i>2) {
        
        if (fun_list[i-2]=="::" & fun_list[i-1]!=gsub("^package:", "", where[[1]])) {
          
          x[i] = 0
          
        }
      }
    }
  }

  return(x[x > 0])
  
}





get_inter = function(fun_name, choices, where, mapping) {

  if (length(unique(where)) != 2) {
    return(NULL)
  }
  
  pkgs = sapply(where, function(x) {unlist(strsplit(x, ":"))[2]}) %>% as.character()
  
  which = c(fun_name %in% pkgdepR::get_functions(pkgs[1]),
            fun_name %in% pkgdepR::get_functions(pkgs[2]))
  
  if (!any(which)) {
    return(mapping)
  }
  
  where_sub = where[which]

  f = sapply(gsub("^package:", "", where_sub), function(x, fun_name) {get(fun_name, envir = asNamespace(x))}, fun_name = fun_name)
  
  fun_list = list()
  for (i in f) {
    
    fun_list[[length(fun_list) + 1]] = get_fun_parts(i)
    
  }
  
  y = fun_list
  y = lapply(y, function(x, fun_name) {match(x, fun_name, nomatch = 0)}, fun_name = choices)
  
  for (i in 1:length(y)) {
    
    for (j in 1:length(y[[i]])) {
      
      if (y[[i]][j]>0) {
        
        if (choices[y[[i]][j]] %in% pkgdepR::get_functions(unlist(strsplit(where_sub[i], ":"))[2])) {

          if (j>2) {
            
            if (fun_list[[i]][j-2]=="::" & fun_list[[i]][j-1]==setdiff(pkgs, unlist(strsplit(where_sub[i], ":"))[2])) {
              
              ref1 = match(paste0(unlist(strsplit(where_sub[i], ":"))[2], "::", fun_name), rownames(mapping))
              ref2 = match(paste0(setdiff(pkgs, unlist(strsplit(where_sub[i], ":"))[2]), "::", choices[y[[i]][j]]), rownames(mapping))
              mapping[ref1, ref2] = 1
              
            }
          }
          
        } else {
          
          if (j>2) {
            
            if (fun_list[[i]][j-2]=="::" & any(setdiff(pkgs, unlist(strsplit(where_sub[i], ":"))[2])==fun_list[[i]][j-1])) {
              
              ref1 = match(paste0(unlist(strsplit(where_sub[i], ":"))[2], "::", fun_name), rownames(mapping))
              ref2 = match(paste0(setdiff(pkgs, unlist(strsplit(where_sub[i], ":"))[2]), "::", choices[y[[i]][j]]), rownames(mapping))
              mapping[ref1, ref2] = 1
              
            } else if (any(setdiff(pkgs, pkgs[which]) %in% names(pkgdepR::all_info_ns(unlist(strsplit(where_sub[i], ":"))[2])$imports))) {
              
              imports = pkgdepR::all_info_ns(unlist(strsplit(where_sub[i], ":"))[2])$imports
              imports = imports[names(imports) %in% setdiff(pkgs, pkgs[which])] %>% unlist() %>% as.character()
              
              if (choices[y[[i]][j]] %in% imports) {
                
                ref1 = match(paste0(unlist(strsplit(where_sub[i], ":"))[2], "::", fun_name), rownames(mapping))
                ref2 = match(paste0(setdiff(pkgs, unlist(strsplit(where_sub[i], ":"))[2]), "::", choices[y[[i]][j]]), rownames(mapping))
                mapping[ref1, ref2] = 1
                
              }
            }
          }
          
          
        }
        

      }
      
    }
  }
  
  return(mapping)
  
}


get_fun_parts = function(x) {
  
  y = is.list(x)
  
  if (!y) {
      
    y = !is.atomic(x) & !is.symbol(x)
    
    if (y) {
      
      try = try(as.list(x), silent = TRUE)
      
      if (inherits(x, "try-error", which = FALSE)) {
        
        y = FALSE
        
      } else {
        
        x = try
        
      }
    }
    
  }
  
  if (y) {
    
    unlist(lapply(x, get_fun_parts), use.names = FALSE)
    
  } else {
    
    paste(deparse(x), collapse = "\n")
    
  }
  
}


convert_to_visnetwork = function(x, funs = NULL) {
  
  y = list()
  
  name_fun = funs
  name_fun = data.frame(cbind(id = 1:length(name_fun), label = name_fun))
  
  func.link = sort(unique(c(as.character(x[, 1]), as.character(x[, 2]))))
  func.nom = sort(unique(as.character(name_fun[, 2])))
  
  func.prob = func.link[which(!func.link %in% func.nom)]
    
  if (length(func.prob) > 0) {
      
    x = x[-unique(c(which(x[, 1] %in% func.prob), which(x[, 2] %in% func.prob))), ]
      
  }
  
  if (!is.null(x)) {
    
    from_to = matrix(0, ncol = dim(x)[2], nrow = dim(x)[1])
    
    if (length(from_to) > 0) {
      
      for (i in 1:dim(x)[1]) {
        
        from_to[i, 1] = which(as.character(x[i, 2]) == name_fun[, 2])
        from_to[i, 2] = which(as.character(x[i, 1]) == name_fun[, 2])
        
        if (dim(x)[2] > 2) {
          
          from_to[i, 3:length(x[i, ])] = x[i, 3:length(x[i, ])]
          
        }
      }
    }
    
  } else {
    
    from_to = cbind(0, 0)
    
  }
  
  from_to = data.frame(from_to)
  names(from_to) = c("from", "to")
  
  y$funs = name_fun
  y$links = from_to
  
  return(y)
  
}


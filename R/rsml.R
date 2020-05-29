#' @export
importrsml <- function(filepath) {
  rsml <- XML::xmlToList(XML::xmlParse(filepath))
  return(rsml)
}

#' @export
nchild <- function(parent, childname = "root") {
  count <- sum(names(parent) == childname)
  return(count)
}

#' @export
#eg. ndescendants(rsml$scene$plant)
#eg. ndescendants(diam$scene$plant)
ndescendants <- function(parent, childname = "root") {
  descendants <- 0 #container for number of descendants
  children <- 1 #set arbitrary starting value above 0 to call nchild() at least once
  times <- 1 #write childname once

  #compute number of descendants
  while (children > 0) {
    children <- nchild(parent, childname = paste(rep(childname, times = times), collapse = "."))
    descendants <- descendants + children

    #adjust parameters for next generation
    parent <- unlist(parent, recursive = FALSE)
    times <- times + 1
  }
  return(descendants)
}

#' @export
#eg. rootlength(mydart$scene$plant$root)
rootlength <- function(root, path = list("geometry", "polyline")) {
  polyline <- extract.from(list = root, path = path)
  polyline <- unlist(unlist(polyline, recursive = FALSE), recursive = FALSE)
  polyline <- do.call("rbind", polyline) #convert list to matrix
  polyline <- apply(polyline, 2, as.numeric) #convert data type from character to numeric
  #calculate distance between point in row n and point in row n+1
  #2D
  if (!"z" %in% colnames(polyline)) {
    distances <- lapply(1:(nrow(polyline)-1), function(n) {
      sqrt(((polyline[n, "x"] - polyline[n+1, "x"])^2) + (polyline[n, "y"] - polyline[n+1, "y"])^2)
    })
  }
  #3D
  else {
    distances <- lapply(1:(nrow(polyline)-1), function(n) {
      sqrt(((polyline[n, "x"] - polyline[n+1, "x"])^2) + ((polyline[n, "y"] - polyline[n+1, "y"])^2) + ((polyline[n, "z"] - polyline[n+1, "z"])^2))
    })
  }
  length <- sum(unlist(distances)) #sum all distances to get complete length
  return(length)
}

#' @export
#eg. rootsyslength(mydart$scene$plant, list("root", "geometry", "polyline"))
#eg. rootsyslength(taproot1$scene$plant, list("root", "geometry", "polyline")) ###########################sum
rootsyslength <- function(plant, path = list("root", "geometry", "polyline")) {
  polyline <- extract.from(list = plant, path = path)
  polyline <- unlist(polyline, recursive = FALSE)
  polyline <- lapply(polyline, function(x) {
    matrix <- do.call("rbind", x)
    matrix <- apply(matrix, 2, as.numeric)
    matrix
    })
  distances.per.root <- lapply(polyline, function(i) {
    if (!"z" %in% colnames(i)) {
      distances <- lapply(1:(nrow(i)-1), function(n) {
        sqrt(((i[n, "x"] - i[n+1, "x"])^2) + (i[n, "y"] - i[n+1, "y"])^2)
      })
    }
    #3D
    else {
      distances <- lapply(1:(nrow(i)-1), function(n) {
        sqrt(((i[n, "x"] - i[n+1, "x"])^2) + ((i[n, "y"] - i[n+1, "y"])^2) + ((i[n, "z"] - i[n+1, "z"])^2))
      })
    }
  })
  syslength <- sum(unlist(distances.per.root))
  return(syslength)
}

#' @export
#eg. (save<-rootlengths(diam$scene$plant, path = list("root", "geometry", "polyline")))
rootlengths <- function(plant, path = list("root", "geometry", "polyline")) {
  polyline <- extract.from(list = plant, path = path)
  polyline <- lapply(polyline, function(x) {
    lapply(x, function(y) {
      matrix <- do.call("rbind", y)
      matrix <- apply(matrix, 2, as.numeric)
      matrix
    })
  })
  lengths.per.root <- lapply(polyline, function(j) {
    lapply(j, function(i) {
      #2D
      if (!"z" %in% colnames(i)) {
        distances <- lapply(1:(nrow(i)-1), function(n) {
          sqrt(((i[n, "x"] - i[n+1, "x"])^2) + (i[n, "y"] - i[n+1, "y"])^2)
        })
      }
      #3D
      else {
        distances <- lapply(1:(nrow(i)-1), function(n) {
          sqrt(((i[n, "x"] - i[n+1, "x"])^2) + ((i[n, "y"] - i[n+1, "y"])^2) + ((i[n, "z"] - i[n+1, "z"])^2))
        })
      }
      sum(unlist(distances))
    })
  })
  return(lengths.per.root)
}

#' @export
#eg. rootxspan(diam$scene$plant$root)
rootxspan <- function(root, path = list("geometry", "polyline", list("point", "x"))) {
  x <- extract.from(list = root, path = path)
  x <- as.numeric(unlist(x))
  xspan <- range(x)[2] - range(x)[1]
  return(xspan)
}

#' @export
#eg. rootyspan(diam$scene$plant$root[[4]]$root)
rootyspan <- function(root, path = list("geometry", "polyline", list("point", "y"))) {
  y <- extract.from(list = root, path = path)
  y <- as.numeric(unlist(y))
  yspan <- range(y)[2] - range(y)[1]
  return(yspan)
}

#' @export
#eg. rootsysxspan(diam$scene$plant)
#eg. rootsysxspan(taproot1$scene$plant)
rootsysxspan <- function(plant, path = list("root", "geometry", "polyline", list("point", "x"))) {
  x <- extract.from(list = plant, path = path)
  x <- as.numeric(unlist(x))
  xspan <- range(x)[2] - range(x)[1]
  return(xspan)
}

#' @export
#eg. rootsysyspan(diam$scene$plant)
#eg. rootsysyspan(taproot1$scene$plant)
rootsysyspan <- function(plant, path = list("root", "geometry", "polyline", list("point", "y"))) {
  y <- extract.from(list = plant, path = path)
  y <- as.numeric(unlist(y))
  yspan <- range(y)[2] - range(y)[1]
  return(yspan)
}

#' @export
#eg. rootzspan(mydart$scene$plant[[1]])
rootzspan <- function(root, path = list("geometry", "polyline", list("point", "z"))) {
  z <- extract.from(list = root, path = path)
  z <- as.numeric(unlist(z))
  zspan <- range(z)[2] - range(z)[1]
  return(zspan)
}

#' @export
#eg. rootsyszspan(mydart$scene$plant)
#eg. rootsyszspan(taproot1$scene$plant)
rootsyszspan <- function(plant, path = list("root", "geometry", "polyline", list("point", "z"))) {
  z <- extract.from(list = plant, path = path)
  z <- as.numeric(unlist(z))
  zspan <- range(z)[2] - range(z)[1]
  return(zspan)
}

#' @export
#eg. rootattr(diam$scene$plant)
#eg. rootattr(taproot1$scene$plant)
#plyr::rbind.fill
rootattr <- function(plant, path = list("root", ".attrs")) {
  root.attr <- extract.from(list = plant, path = path)
  root.attr <- unlist(root.attr, recursive = FALSE)
  root.attr <- lapply(root.attr, function(x) {
    titles <- names(x)
    df <- as.data.frame(matrix(x, nrow = 1))
    colnames(df) <- titles
    df
    })
  root.attr <- plyr::rbind.fill(root.attr)
  return(root.attr)
} #########################translate accession id

#' @export
#eg. rootontology(diam$scene$plant)
#eg. rootontology(taproot1$scene$plant)
rootontology <- function(plant, path = list("root", ".attrs", "accession")) {
  #extract accession values from all roots
  ontology <- extract.from(list = plant, path = path)
  ontology <- table(unlist(ontology))
  ontology <- matrix(ontology, nrow = 1, dimnames = list(NULL, names(ontology)))
  return(ontology)
}

#' @export
#eg. save<-plotrootsys(mydart$scene$plant, threedim = TRUE)
#eg. plotrootsys(taproot1$scene$plant, threedim = TRUE)
#did not account for NAs
#ggplot2
plotrootsys <- function(plant, coordinates.path = list("root", "geometry", "polyline"), threedim = FALSE,
                        colour.by = "order", palette = NULL) {
  #extract coordinates from all roots
  coordinates <- extract.from(list = plant, path = coordinates.path)

  #count number of points per root and per order
  npoints <- unlist(lapply(coordinates, function(x) {
    unlist(lapply(x, function(y) {length(y)}), recursive = FALSE)
  }), recursive = FALSE)
  npoints.order <- unlist(lapply(coordinates, function(x) {
    length(unlist(x, recursive = FALSE))
    }))

  #convert coordinates list to data.frame
  coordinates <- do.call("rbind", lapply(coordinates, function(x) {
    do.call("rbind", lapply(1:length(x), function(y) {
      per.root <- do.call("rbind", x[[y]])
      per.root <- apply(per.root, 2, as.numeric)
      as.data.frame(per.root)
      }))
    }))

  coordinates$root <- as.factor(unlist(lapply(1:length(npoints), function(x) {
    rep(x, times = npoints[x])
  })))
  coordinates$order <- as.factor(unlist(lapply(1:length(npoints.order), function(x) {
    rep(x, times = npoints.order[x])
  })))
  rownames(coordinates) <- NULL

  #draw plot
  if (threedim) {
    plot <- drawroots3d(dat = coordinates, x = "x", y = "y", z = "z",
                        colour.by = colour.by, root = "root", palette = palette)
  }
  else {
    plot <- drawroots2d(dat = coordinates, x = "x", y = "y",
                        colour.by = colour.by, root = "root", palette = palette)
  }

  #compile outputs
  out <- list()
  out[["coordinates"]]<-coordinates
  out[["plot"]]<-plot
  return(out)
}

#' @export
#eg. drawroots2d(save, colour.by = "root")
drawroots2d <- function(dat, x = "x", y = "y", colour.by = "order", root = "root", palette = NULL) {
  #draw plot
  plot <- ggplot2::ggplot(data = dat, mapping = ggplot2::aes_string(x = x, y = y, color = colour.by)) +
    ggplot2::geom_point() +
    ggplot2::geom_path(group = dat[,root]) +
    ggplot2::scale_y_reverse() +
    ggplot2::geom_hline(yintercept = 0)
  if (is.null(palette) == FALSE) {
    plot <- plot + ggplot2::scale_colour_manual(values = palette)
  }
  return(plot)
}

#'@export
#eg. drawroots3d(save)
drawroots3d <- function(dat, x = "x", y = "y", z = "z", colour.by = "order", root = "root", palette = NULL) {
  #set same default colour palette as ggplot2
  if (is.null(palette)) {colour.palette <- scales::hue_pal()(length(levels(dat[,colour.by])))}
  #if custom colour(s) are indicated in argument palette
  else {
    colour.palette <- palette
    if (length(palette) < length(levels(dat[,colour.by]))) {warning("insufficient colours indicated in palette")}
  }

  #empty plot
  rgl::plot3d(1, 1, 1, type = "n",
              xlim = c(range(dat[,x])[1], range(dat[,x])[2]),
              ylim = c(range(dat[,y])[1], range(dat[,y])[2]),
              zlim = c(range(dat[,z])[1], range(dat[,z])[2]),
              xlab = "", ylab = "", zlab = "z")

  #draw roots onto empty plot 1 by 1
  for (i in 1:length(levels(dat[,colour.by]))) {
    per.colour <- dat[dat[,colour.by] == levels(dat[,colour.by])[i],]
    per.colour[,root] <- droplevels(per.colour[,root])
    for (j in 1:length(levels(per.colour[,root]))) {
      per.root <- per.colour[per.colour[,root] == levels(per.colour[,root])[j],]
      rgl::plot3d(x = per.root[,x], y = per.root[,y], z = per.root[,z],
                  type = "l", lwd = 2, col = colour.palette[i],
                  add = TRUE)
    }
  }
}

#' @export
#eg. (save<-extract.from0(diam$scene$plant, list("root", "geometry", "polyline")))
extract.from0 <- function(list, path) {
  if (is.character(path[[1]]) && !path[[1]] %in% names(list)) {
    stop(path[[1]], " is not found in names(", deparse(substitute(list)), ")")
  }
  else {
    first.path <- path[[1]]
    container <- list()

    #specify parameters for first cycle
    children <- 1
    order <- 1

    #extract values from list as specified by path
    while(children > 0) {
      path[[1]] <- paste(rep(first.path, times = order), collapse = ".")
      index <- which(names(list) %in% path[[1]])

      #access list according to path
      container[[paste("order", order)]] <- lapply(index, function(i) {
        list <- list[[i]]
        if (length(path) >= 2) {
          x <- 2
          while (x <= length(path)) {
            if (is.character(path[[x]]) && !path[[x]] %in% names(list)) {
              stop(path[[x]], " not found after ", path[[x-1]])
              }
            list <- list[[path[[x]]]]
            x <- x + 1
          }
        }
        list
      })

      #adjust parameters for next cycle
      list <- unlist(list, recursive = FALSE)
      children <- sum(names(list) == paste(rep(first.path, times = order + 1), collapse = "."))
      order <- order + 1
    }
    container <- lapply(container, function(x) {
      names(x) <- paste(first.path, 1:length(x))
      x
    })
  }
  return(container)
}

#' @export
#eg. (save<-extract.from(diam$scene$plant, list("root", "functions", list("function", list("sample", list("value"))))))
#eg. (save<-extract.from(taproot1$scene$plant, list("root", "functions", 1, list("sample"))))
extract.from <- function(list, path) {
  #if path is not a nested list, simply pass to extract.from0
  if (length(path) == 1) {list <- extract.from0(list = list, path = path)}
  if (length(path) > 1 &&
      is.list(path[[length(path)]]) == FALSE) {list <- extract.from0(list = list, path = path)}

  #if path is a nested list
  if (is.list(path[[length(path)]]) == TRUE && length(path) > 1) {
    list <- extract.from0(list = list, path = path[1:(length(path)-1)]) #root, functions
    while (is.list(path[[length(path)]])) {
      path <- path[[length(path)]]
      if (is.list(path[[length(path)]]) == TRUE) {
        list <- lapply(list, function(order) {
          lapply(order, function(root) {
            unlist(unlist(extract.from0(list = root, path = path[1:(length(path)-1)]),
                          recursive = FALSE, use.names = FALSE),
                   recursive = FALSE)
          })
        })
      }
      else {
        list <- lapply(list, function(order) {
          lapply(order, function(root) {
            unlist(unlist(extract.from0(list = root, path = path[1:length(path)]),
                          recursive = FALSE, use.names = FALSE),
                   recursive = FALSE)
          })
        })
      }
    }
  }
  return(list)
}

#' @export
#eg. plantattr(diam$scene$plant, path = list(".attrs"))
#eg. plantattr(taproot1$scene$plant, path = list(".attrs")) #not found
plantattr <- function(plant, path = list(".attrs")) {
  attr <- extract.from(list = plant, path = path)
  attr <- unlist(unlist(attr, recursive = FALSE, use.names = FALSE), recursive = FALSE) #change list to named character vector
  attr <- as.data.frame(matrix (attr, nrow = 1, dimnames = list(NULL, names(attr))))
  return(attr)
} #################check if archidart package data has genotype

#' @export
#eg. metadf(rsml)
metadf <- function(rsml) {
  metadata <- rsml[["metadata"]]
  metadata <- unlist(metadata)
  metadata <- as.data.frame(matrix(metadata, nrow = 1, dimnames = list(NULL, names(metadata))))
  return(metadata)
}

#' @export
#eg. buildarow(diam$scene$plant, c("plantattr", "rootsyslength", "ndescendants", "rootsysxspan", "rootsysyspan", "rootsyszspan", "rootsyssurfarea", "rootsysvol", "rootontology"))
#eg. buildarow(taproot1$scene$plant, c("plantattr", "rootsyslength", "ndescendants", "rootsysxspan", "rootsysyspan", "rootsyszspan", "rootsyssurfarea", "rootsysvol", "rootontology"))
buildarow <- function(plant, function.names, labels = NULL) {
  #produce error message if functions are not character strings
  if (!data.class(function.names) == "character") {
    stop("argument function.names accepts only character vectors of names of functions")
    }
  #apply each function on object in first argument
  evals <- lapply(1:length(function.names), function(x){
    eval <- tryCatch(get(function.names[x])(plant), error = function(e) {NA})
    if (TRUE %in% is.na(eval)) {warning("errors are printed as NA")}
    eval
    })
  if (is.null(labels)) {names(evals) <- function.names}
  else {names(evals) <- labels}
  #compile into a single row
  evals <- do.call("cbind", evals)
  return(evals)
}

#' @export
#eg. rsmltodf("C:/Users/Stephanie/Desktop/rsml/datasets/homepage")
#eg. rsmltodf("C:/Users/Stephanie/Desktop/rsml/datasets/mini")
rsmltodf <- function(directory,
                     function.names = c("plantattr", "rootsyslength", "ndescendants", "rootsysxspan", "rootsysyspan", "rootsyszspan", "rootsyssurfarea", "rootsysvol"),
                     labels = c("attributes", "total root length", "total no. of roots", "x range", "y range", "z range", "total surface area", "total volume")) {
  #import rsml files
  filepaths <- list.files(path = directory, pattern = "*.rsml", full.names = TRUE)
  rsml <- lapply(filepaths, importrsml)

  #create a data.frame compiling traits
  filename <- list.files(directory)
  metadata <- plyr::rbind.fill(lapply(rsml, metadf))
  df <- do.call("rbind", lapply(rsml, function(x) {
    buildarow(x$scene$plant, function.names = function.names, labels = labels)}))
  df <- cbind(filename, metadata, df)
  return(df)
}

#' @export
#eg. rootsyssurfarea(diam$scene$plant)
#eg. rootsyssurfarea(taproot1$scene$plant)
rootsyssurfarea <- function(plant,
                            coord.path = list("root", "geometry", "polyline"),
                            diam.path = list("root", "functions", "function", list("sample"))) {
  #calculate distances between points for every root
  polyline <- extract.from(list = plant, path = coord.path)
  polyline <- lapply(polyline, function(x) {
    lapply(x, function(y) {
      matrix <- do.call("rbind", y)
      matrix <- apply(matrix, 2, as.numeric)
      matrix
    })
  })
  distances.per.root <- lapply(polyline, function(j) {
    lapply(j, function(i) {
      #2D
      if (!"z" %in% colnames(i)) {
        distances <- lapply(1:(nrow(i)-1), function(n) {
          sqrt(((i[n, "x"] - i[n+1, "x"])^2) + (i[n, "y"] - i[n+1, "y"])^2)
        })
      }
      #3D
      else {
        distances <- lapply(1:(nrow(i)-1), function(n) {
          sqrt(((i[n, "x"] - i[n+1, "x"])^2) + ((i[n, "y"] - i[n+1, "y"])^2) + ((i[n, "z"] - i[n+1, "z"])^2))
        })
      }
    })
  })
  distances <- unlist(distances.per.root, recursive = FALSE)
  distances <- lapply(distances, unlist)

  #get diameters per point for every root
  diameters <- unlist(extract.from(list = plant, path = diam.path), recursive = FALSE)

  #calculate surface area
  surf.area.bw.points <- lapply(1:length(distances), function(rootn) {
    lapply(1:length(distances[[rootn]]), function(x) {
      rad1 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[2]/2 #smaller radius
      rad2 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[1]/2 #larger radius
      if (rad1 == rad2) {rad1 * 2 * pi * distances[[rootn]][x]}
      else {
        h2 <- (rad2 * distances[[rootn]][x])/(rad1 - rad2)
        h1 <- distances[[rootn]][x] + h2
        slant1 <- sqrt(rad1^2 + h1^2)
        slant2 <- sqrt(rad2^2 + h2^2)
        (pi * rad1 * slant1) - (pi * rad2 * slant2)
      }
    })
  })
  total.surf.area <- sum(unlist(surf.area.bw.points))
  return(total.surf.area)
}

#' @export
#eg. rootsysvol(diam$scene$plant, diam.path = list("root", "functions",  "function", list("sample")))
#eg. rootsysvol(taproot1$scene$plant, diam.path = list("root", "functions", "function", list("sample")))
rootsysvol <- function(plant,
                       coord.path = list("root", "geometry", "polyline"),
                       diam.path = list("root", "functions",  "function", list("sample"))) {
  #calculate distances between points for every root
  polyline <- extract.from(list = plant, path = coord.path)
  polyline <- lapply(polyline, function(x) {
    lapply(x, function(y) {
      matrix <- do.call("rbind", y)
      matrix <- apply(matrix, 2, as.numeric)
      matrix
    })
  })
  distances <- lapply(polyline, function(j) {
    lapply(j, function(i) {
      #2D
      if (!"z" %in% colnames(i)) {
        distances <- lapply(1:(nrow(i)-1), function(n) {
          sqrt(((i[n, "x"] - i[n+1, "x"])^2) + (i[n, "y"] - i[n+1, "y"])^2)
        })
      }
      #3D
      else {
        distances <- lapply(1:(nrow(i)-1), function(n) {
          sqrt(((i[n, "x"] - i[n+1, "x"])^2) + ((i[n, "y"] - i[n+1, "y"])^2) + ((i[n, "z"] - i[n+1, "z"])^2))
        })
      }
    })
  })
  distances <- unlist(distances, recursive = FALSE)
  distances <- lapply(distances, unlist)

  #get diameters per point for every root
  diameters <- unlist(extract.from(list = plant, path = diam.path), recursive = FALSE)

  #calculate volume
  vol.bw.points <- lapply(1:length(distances), function(rootn) {
    lapply(1:length(distances[[rootn]]), function(x) {
      rad1 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[2]/2 #smaller radius
      rad2 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[1]/2 #larger radius
      if (rad1 == rad2) {rad1^2 * pi * distances[[rootn]][x]}
      else {
        h2 <- (rad2 * distances[[rootn]][x])/(rad1 - rad2)
        h1 <- distances[[rootn]][x] + h2
        slant1 <- sqrt(rad1^2 + h1^2)
        slant2 <- sqrt(rad2^2 + h2^2)
        (pi * (rad1^2) * h1/3) - (pi * (rad2^2) * h2/3)
      }
    })
  })
  total.vol <- sum(unlist(vol.bw.points))
  return(total.vol)
}

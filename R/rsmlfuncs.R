#' @import XML
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

  #set parameters to enter subsequent while loop
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
rootlength <- function(root, route = list("geometry", "polyline")) {
  polyline <- extract.from(rsml = root, route = route)
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
rootSysLength <- function(plant, route = list("root", "geometry", "polyline")) {
  polyline <- extract.from(rsml = plant, route = route)
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
#eg. (save<-rootlengths(diam$scene$plant, list("root", "geometry", "polyline")))
rootlengths <- function(plant, route = list("root", "geometry", "polyline")) {
  polyline <- extract.from(rsml = plant, route = route)
  polyline <- lapply(polyline, function(x) {
    lapply(x, function(y) {
      matrix <- do.call("rbind", y)
      apply(matrix, 2, as.numeric)
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
rootxspan <- function(root, route = list("geometry", "polyline", list("point", "x"))) {
  x <- extract.from(rsml = root, route = route)
  x <- as.numeric(unlist(x))
  xspan <- range(x)[2] - range(x)[1]
  return(xspan)
}

#' @export
#eg. rootyspan(diam$scene$plant$root[[4]]$root)
rootyspan <- function(root, route = list("geometry", "polyline", list("point", "y"))) {
  y <- extract.from(rsml = root, route = route)
  y <- as.numeric(unlist(y))
  yspan <- range(y)[2] - range(y)[1]
  return(yspan)
}

#' @export
#eg. rootsysxspan(diam$scene$plant)
#eg. rootsysxspan(taproot1$scene$plant)
rootSysXSpan <- function(plant, route = list("root", "geometry", "polyline", list("point", "x"))) {
  x <- extract.from(rsml = plant, route = route)
  x <- as.numeric(unlist(x))
  xspan <- range(x)[2] - range(x)[1]
  return(xspan)
}

#' @export
#eg. rootsysyspan(diam$scene$plant)
#eg. rootsysyspan(taproot1$scene$plant)
rootSysYSpan <- function(plant, route = list("root", "geometry", "polyline", list("point", "y"))) {
  y <- extract.from(rsml = plant, route = route)
  y <- as.numeric(unlist(y))
  yspan <- range(y)[2] - range(y)[1]
  return(yspan)
}

#' @export
#eg. rootzspan(mydart$scene$plant[[1]])
rootzspan <- function(root, route = list("geometry", "polyline", list("point", "z"))) {
  z <- extract.from(rsml = root, route = route)
  z <- as.numeric(unlist(z))
  zspan <- range(z)[2] - range(z)[1]
  return(zspan)
}

#' @export
#eg. rootsyszspan(mydart$scene$plant)
#eg. rootsyszspan(taproot1$scene$plant)
rootSysZSpan <- function(plant, route = list("root", "geometry", "polyline", list("point", "z"))) {
  z <- extract.from(rsml = plant, route = route)
  z <- as.numeric(unlist(z))
  zspan <- range(z)[2] - range(z)[1]
  return(zspan)
}

#' @export
#eg. rootattr(diam$scene$plant)
#eg. rootattr(taproot1$scene$plant)
#plyr::rbind.fill
rootattr <- function(plant, route = list("root", ".attrs")) {
  root.attr <- extract.from(rsml = plant, route = route) #retrieve attributes from all roots
  root.attr <- unlist(root.attr, recursive = FALSE) #unlist once since root order details are not needed

  #for each root, arrange attributes in a single-row data.frame
  root.attr <- lapply(root.attr, function(x) {
    titles <- names(x)
    df <- as.data.frame(matrix(x, nrow = 1))
    colnames(df) <- titles
    df
    })

  #rbind rows for all roots
  root.attr <- plyr::rbind.fill(root.attr)
  return(root.attr)
}

#' @export
#eg. rootOntology(diam$scene$plant)
#eg. rootOntology(taproot1$scene$plant)
rootOntology <- function(plant, route = list("root", ".attrs", "accession"), translate = TRUE) {
  #extract accession values from all roots
  ontology <- extract.from(rsml = plant, route = route)
  ontology <- table(unlist(ontology))
  ontology <- matrix(ontology, nrow = 1, dimnames = list(NULL, names(ontology)))
  if (translate) {
    colnames(ontology) <- plantOntology[match(colnames(ontology), plantOntology[,1]), 2]
  }
  return(ontology)
}

#' @export
#eg. save<-plotrootsys(mydart$scene$plant, threedim = FALSE, plot.pdf = "C:/Users/Stephanie/Desktop/root.pdf")
#eg. plotrootsys(taproot1$scene$plant, threedim = TRUE)
#did not account for NAs
#ggplot2
plotRootSys <- function(plant, coordinates.route = list("root", "geometry", "polyline"), threedim = FALSE,
                        colour.by = "order", palette = NULL,
                        plot.pdf = NULL, pdf.size = c(7, 7)) {
  #extract coordinates from all roots
  coordinates <- extract.from(rsml = plant, route = coordinates.route)

  #count number of points per root and per order
  npoints <- unlist(lapply(coordinates, function(x) {
    unlist(lapply(x, function(y) {length(y)}), recursive = FALSE)
  }), recursive = FALSE)
  #count number of points for all roots per order
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

  #indicate which root and order the set of coordinates in the corresponding row are derived from
  coordinates$root <- as.factor(unlist(lapply(1:length(npoints), function(x) {
    rep(x, times = npoints[x])
  })))
  coordinates$order <- as.factor(unlist(lapply(1:length(npoints.order), function(x) {
    rep(x, times = npoints.order[x])
  })))
  rownames(coordinates) <- NULL

  #draw plot
  if (threedim == TRUE) {
    plot <- drawroots3d(dat = coordinates, x = "x", y = "y", z = "z",
                        colour.by = colour.by, root = "root", palette = palette)
  }
  else {
    plot <- drawroots2d(dat = coordinates, x = "x", y = "y",
                        colour.by = colour.by, root = "root", palette = palette)
  }

  if (is.null(plot.pdf) == FALSE) {
    pdf(file = paste0(plot.pdf), width = pdf.size[1], height = pdf.size[2])
    print(plot)
    dev.off()
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

#' @import rgl scales
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
extract.from0 <- function(rsml, route) {
  #ensure first element in route is a name found in rsml
  if (is.character(route[[1]]) && !route[[1]] %in% names(rsml)) {
    stop(route[[1]], " is not found in names(", deparse(substitute(rsml)), ")")
  }

  first.route <- route[[1]] #save first element of route
  container <- list() #compile extracted values from rsml

  #specify parameters to enter subsequent while loop
  children <- 1 #arbitrary number but must be > 0
  order <- 1 #begin by extracting values from first-order roots

  #extract values from rsml as specified by route
  while(children > 0) {
    route[[1]] <- paste(rep(first.route, times = order), collapse = ".") #rewrite first element of route after every loop
    index <- which(names(rsml) %in% route[[1]]) #record all indexes with the same name as the first element in route

    #access rsml according to route
    container[[paste("order", order)]] <- lapply(index, function(i) {
      rsml <- rsml[[i]] #access each index
      if (length(route) >= 2) {
        x <- 2
        while (x <= length(route)) {
          #again, ensure first element in route is a name found in rsml
          if (is.character(route[[x]]) && !route[[x]] %in% names(rsml)) {
            stop(route[[x]], " not found after ", route[[x-1]])
          }
          rsml <- rsml[[route[[x]]]] #access elements (elements after the first) specified in route
          x <- x + 1
        }
      }
      rsml
    })

    #adjust parameters for next cycle
    rsml <- unlist(rsml, recursive = FALSE)
    children <- sum(names(rsml) == paste(rep(first.route, times = order + 1), collapse = "."))
    order <- order + 1
  }

  #assign names to roots under each order
  container <- lapply(container, function(x) {
    names(x) <- paste(first.route, 1:length(x))
    x
  })
  return(container)
}

#' @export
#eg. (save<-extract.from(diam$scene$plant, list("root", "functions", list("function", list("sample", list("value"))))))
#eg. (save<-extract.from(taproot1$scene$plant, list("root", "functions", 1, list("sample"))))
extract.from <- function(rsml, route) {
  #if route is not a nested list, simply pass to extract.from0
  if (length(route) == 1) {rsml <- extract.from0(rsml = rsml, route = route)}
  if (length(route) > 1 &&
      is.list(route[[length(route)]]) == FALSE) {rsml <- extract.from0(rsml = rsml, route = route)}

  #if route is a nested list
  if (is.list(route[[length(route)]]) == TRUE && length(route) > 1) {
    rsml <- extract.from0(rsml = rsml, route = route[1:(length(route)-1)]) #extract values along route before the last element of the first level

    while (is.list(route[[length(route)]])) { #proceed if last element of the first level of route is a list
      route <- route[[length(route)]] #rewrite route to remove first level of route

      #after rewriting route, proceed if last element of first level of route is still a list
      if (is.list(route[[length(route)]]) == TRUE) {
        #for each root in each order, extract values along route before the last element of the first level
        rsml <- lapply(rsml, function(order) {
          lapply(order, function(root) {
            #unlist to prevent additional layers in rsml list
            unlist(unlist(extract.from0(rsml = root, route = route[1:(length(route)-1)]),
                          recursive = FALSE, use.names = FALSE),
                   recursive = FALSE)
          })
        })
      }
      #after rewriting route, proceed if last element of first level of route is no longer a list
      else {
        #for each root in each order, extract values along all elements in route
        rsml <- lapply(rsml, function(order) {
          lapply(order, function(root) {
            #unlist to prevent additional layers in rsml list
            unlist(unlist(extract.from0(rsml = root, route = route[1:length(route)]),
                          recursive = FALSE, use.names = FALSE),
                   recursive = FALSE)
          })
        })
      }
    }
  }
  return(rsml)
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
#eg. buildarow(taproot1$scene$plant, single.value.functions=c("rootsyslength", "ndescendants", "rootsysxspan", "rootsysyspan", "rootsyszspan", "rootsyssurfarea", "rootsysvol"),
#          single.value.function.labels = c("total root length", "total no. of roots", "x range", "y range", "z range", "total surface area", "total volume"),
#          multi.value.functions = "rootontology")
buildarow <- function(plant,
                      single.value.functions = NULL, single.value.function.labels = NULL,
                      multi.value.functions = NULL) {
  #stop if functions are not character strings or no functions are indicated at all
  if (!is.null(single.value.functions) && !is.character(single.value.functions)) {
    stop("arguments single.value.functions accepts only character vectors of names of functions")
  }
  if (!is.null(multi.value.functions) && !is.character(multi.value.functions)) {
    stop("argument multi.value.functions accepts only character vectors of names of functions")
  }
  if (is.null(single.value.functions) && is.null(multi.value.functions)) {
    stop("please input at least 1 function name")
  }

  #apply each function in argument single.value.function on object in first argument
  if (!is.null(single.value.functions)) {
    single.evals <- lapply(1:length(single.value.functions), function(x){
      eval <- tryCatch(get(single.value.functions[x])(plant), error = function(e) {NA})
      if (TRUE %in% is.na(eval)) {warning("errors are printed as NA")}
      eval
    })
    if (is.null(single.value.function.labels)) {names(single.evals)[1:length(single.value.functions)] <- single.value.functions}
    else {names(single.evals) <- single.value.function.labels}
  }
  else {single.evals <- NULL}

  #apply each function in argument multi.value.function on object in first argument
  if (!is.null(multi.value.functions)) {
    multi.evals <- lapply(1:length(multi.value.functions), function(x){
      eval <- tryCatch(get(multi.value.functions[x])(plant), error = function(e) {NA})
      if (TRUE %in% is.na(eval)) {warning("errors are printed as NA")}
      eval
    })
  }
  else {multi.evals <- NULL}

  #compile into a single row
  evals <- do.call("cbind", c(single.evals, multi.evals))
  return(evals)
}

#' @export
#eg. rsmltodf("C:/Users/Stephanie/Desktop/rsml/datasets/homepage")
#eg. rsmltodf("C:/Users/Stephanie/Desktop/rsml/datasets/mini")
rsmltodf <- function(directory,
                     single.value.functions = c("rootSysLength", "ndescendants", "rootSysXSpan", "rootSysYSpan", "rootSysZSpan", "rootSysSurfArea", "rootSysVol"),
                     single.value.function.labels = c("total root length", "total no. of roots", "x range", "y range", "z range", "total surface area", "total volume"),
                     multi.value.functions = "rootOntology") {
  #import rsml files
  filepaths <- list.files(path = directory, pattern = "*.rsml", full.names = TRUE)
  rsml <- lapply(filepaths, importrsml)

  #create a data.frame compiling traits
  filename <- list.files(directory)
  metadata <- plyr::rbind.fill(lapply(rsml, metadf))
  df <- plyr::rbind.fill(lapply(rsml, function(x) {
    as.data.frame(buildarow(x$scene$plant, single.value.functions = single.value.functions,
                            single.value.function.labels = single.value.function.labels,
                            multi.value.functions = multi.value.functions))}))
  df <- cbind(filename, metadata, df)
  return(df)
}

#' @export
#eg. rootsyssurfarea(diam$scene$plant)
#eg. rootsyssurfarea(taproot1$scene$plant)
rootSysSurfArea <- function(plant,
                            coordinates.route = list("root", "geometry", "polyline"),
                            diameter.route = list("root", "functions", "function", list("sample"))) {

  #extract all coordinates for all roots
  polyline <- extract.from(rsml = plant, route = coordinates.route)
  polyline <- lapply(polyline, function(order) { #arrange coordinates into a data.frame per root per order
    lapply(order, function(root) {
      matrix <- do.call("rbind", root)
      apply(matrix, 2, as.numeric)
    })
  })

  #calculate distances between adjacent points for every root
  distances.per.root <- lapply(polyline, function(order) {
    lapply(order, function(root) {
      #2D
      if (!"z" %in% colnames(root)) {
        distances <- lapply(1:(nrow(root)-1), function(n) {
          sqrt(((root[n, "x"] - root[n+1, "x"])^2) + (root[n, "y"] - root[n+1, "y"])^2)
        })
      }
      #3D
      else {
        distances <- lapply(1:(nrow(root)-1), function(n) {
          sqrt(((root[n, "x"] - root[n+1, "x"])^2) + ((root[n, "y"] - root[n+1, "y"])^2) + ((root[n, "z"] - root[n+1, "z"])^2))
        })
      }
    })
  })
  distances <- unlist(distances.per.root, recursive = FALSE) #unlist once since root order details are not needed
  distances <- lapply(distances, unlist) #convert list of distances between adjacent points per root into a vector

  #get diameters per point for every root
  diameters <- unlist(extract.from(rsml = plant, route = diameter.route), recursive = FALSE)

  #calculate surface area
  surf.area.bw.points <- lapply(1:length(distances), function(rootn) {
    lapply(1:length(distances[[rootn]]), function(x) {
      rad1 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[2]/2 #larger radius
      rad2 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[1]/2 #smaller radius
      if (rad1 == rad2) {rad1 * 2 * pi * distances[[rootn]][x]}
      else {
        pi * (rad1 + rad2) * sqrt(as.numeric(distances[[rootn]][x])^2 + (rad1 - rad2)^2)
      }
    })
  })
  total.surf.area <- sum(unlist(surf.area.bw.points))
  return(total.surf.area)
}

#' @export
#eg. rootsysvol(diam$scene$plant)
#eg. rootsysvol(taproot1$scene$plant)
rootSysVol <- function(plant,
                       coordinates.route = list("root", "geometry", "polyline"),
                       diameter.route = list("root", "functions",  "function", list("sample"))) {
  ##extract all coordinates for all roots
  polyline <- extract.from(rsml = plant, route = coordinates.route)
  polyline <- lapply(polyline, function(x) { #arrange coordinates into a data.frame per root per order
    lapply(x, function(y) {
      matrix <- do.call("rbind", y)
      matrix <- apply(matrix, 2, as.numeric)
      matrix
    })
  })

  #calculate distances between adjacent points for every root
  distances <- lapply(polyline, function(order) {
    lapply(order, function(root) {
      #2D
      if (!"z" %in% colnames(root)) {
        distances <- lapply(1:(nrow(root)-1), function(n) {
          sqrt(((root[n, "x"] - root[n+1, "x"])^2) + (root[n, "y"] - root[n+1, "y"])^2)
        })
      }
      #3D
      else {
        distances <- lapply(1:(nrow(root)-1), function(n) {
          sqrt(((root[n, "x"] - root[n+1, "x"])^2) + ((root[n, "y"] - root[n+1, "y"])^2) + ((root[n, "z"] - root[n+1, "z"])^2))
        })
      }
    })
  })
  distances <- unlist(distances, recursive = FALSE) #unlist once since root order details are not needed
  distances <- lapply(distances, unlist) #convert list of distances between adjacent points per root into a vector

  #get diameters per point for every root
  diameters <- unlist(extract.from(rsml = plant, route = diameter.route), recursive = FALSE)

  #calculate volume
  vol.bw.points <- lapply(1:length(distances), function(rootn) {
    lapply(1:length(distances[[rootn]]), function(x) {
      rad1 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[2]/2 #smaller radius
      rad2 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[1]/2 #larger radius
      if (rad1 == rad2) {rad1^2 * pi * distances[[rootn]][x]}
      else {
        1/3 * pi * distances[[rootn]][x] * (rad1^2 + rad1*rad2 + rad2^2)
      }
    })
  })
  total.vol <- sum(unlist(vol.bw.points))
  return(total.vol)
}

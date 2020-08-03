#' Imported rsml file
#'
#' An rsml file imported as a list by \code{\link{rsmlImport}}.
#'
#' @docType data
#'
#' @format An object of "list" class with RSML structure (nested objects beyond the primary root are not described):
#' \itemize{
#'   \item{metadata: metadata about the rsml file}
#'   \item{scene}
#'   \itemize{
#'     \item{plant: plant data}
#'     \itemize{
#'       \item{root: primary root data, as well as lateral roots data}
#'       \itemize{
#'         \item{geometry: geometric information of the primary root}
#'         \itemize{
#'           \item {polyline: list of points constituting the polyline and their x, y and z coordinates}
#'         }
#'         \item{functions: continuous data along the primary root}
#'         \itemize{
#'           \item{function: list of diameters, the order of which corresponds to the list of points in the polyline}
#'           \item{function: list of ages, the order of which corresponds to the list of points in the polyline}
#'         }
#'         \item{root: "list"-class object representing a lateral root sprouting from the primary root}
#'         \item{root: "list"-class object representing a lateral root sprouting from the primary root}
#'         \item{root: "list"-class object representing a lateral root sprouting from the primary root}
#'         \item{root: "list"-class object representing a lateral root sprouting from the primary root}
#'         \item{root: "list"-class object representing a lateral root sprouting from the primary root}
#'         \item{.attrs: primary root metadata. Includes plant ontology accession ID.}
#'       }
#'     }
#'   }
#' }
#'
#' @source Lobet, Guillaume (2017): Simulated dicot root systems. figshare. Dataset. \url{https://doi.org/10.6084/m9.figshare.5624878.v1}
#'
#' @examples
#' data("sparse", package = "PhenomatoR")
#' graph <- plotRootSys(sparse$scene$plant)
#' graph$plot
"sparse"

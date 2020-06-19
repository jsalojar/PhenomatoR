#' @title PhenomatoR: An R package for the Analysis of Large-Scale Phenomics
#'   Data
#'
#' @description The main purpose of this package is to provide functions to
#'   perform analysis on high-throughput phenomics data. Outcomes of
#'   experimental classes are normalized against that of the control and
#'   visualized in a heatmap.
#'
#'   Functions can also be categorized by their sub-purposes which are to:
#'   build/organise dataset, visualize effect sizes in a heatmap, and conduct
#'   principal component analysis (PCA).
#'
#' @section Build/organise dataset:
#'
#'   This section can be further divided into 2 categories: general and specific
#'   to rsml files.
#'
#'   General functions: \code{nominalToCount}, \code{nominalToOrdinal},
#'   \code{omit}
#'
#'   RSML functions: \code{importrsml}, \code{buildarow}, \code{rsmltodf},
#'   \code{metadf}, \code{nchild}, \code{ndescendants}, \code{extract.from0},
#'   \code{extract.from}, \code{rootlength}, \code{rootSysLength},
#'   \code{rootlengths}, \code{rootxspan}, \code{rootSysXSpan},
#'   \code{rootyspan}, \code{rootSysYSpan}, \code{rootzspan},
#'   \code{rootSysZSpan}, \code{rootattr}, \code{rootOntology},
#'   \code{plotRootSys}, \code{drawroots2d}, \code{drawroots3d},
#'   \code{plantattr}, \code{rootSysSurfArea}, \code{rootSysVol}
#'
#' @section Visualize effect sizes in a heatmap:
#'
#'   Functions: \code{bootstrap.model.z.heatmap}, \code{bootstrap.model.z},
#'   \code{cutz.heatmap}, \code{extract.glht.z}, \code{long.to.wide.df},
#'   \code{two.factor.interaction}
#'
#' @section Principal Component Analysis (PCA):
#'
#'   Functions: \code{pcaMat}, \code{pcaDf}
#'
#' @docType package
#' @name PhenomatoR
NULL

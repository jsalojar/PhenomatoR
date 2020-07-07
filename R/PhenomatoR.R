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
#'   RSML functions: \code{rsmlImport}, \code{buildRow}, \code{rsmlToDf},
#'   \code{metaToDf}, \code{nChild}, \code{nDescendants}, \code{rsmlExtract0},
#'   \code{rsmlExtract}, \code{rootLength}, \code{rootSysLength},
#'   \code{rootLengthList}, \code{rootXSpan}, \code{rootSysXSpan},
#'   \code{rootYSpan}, \code{rootSysYSpan}, \code{rootZSpan},
#'   \code{rootSysZSpan}, \code{rootOntology}, \code{plotRootSys},
#'   \code{drawRoots2D}, \code{drawRoots3D}, \code{rootSysSurfArea},
#'   \code{rootSysVol}
#'
#' @section Visualize effect sizes in a heatmap:
#'
#'   Functions: \code{bootstrapModelZHeatmap}, \code{bootstrapModelZ},
#'   \code{heatmapper}, \code{glhtExtract}, \code{stretchDf},
#'   \code{twoFactorInteraction}
#'
#' @section Principal Component Analysis (PCA):
#'
#'   Functions: \code{pcaMat}, \code{pcaDf}
#'
#' @docType package
#' @name PhenomatoR
NULL

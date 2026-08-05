#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
unknown <- setdiff(args, "--refresh")
if (length(unknown)) stop("Unknown argument(s): ", paste(unknown, collapse = ", "), call. = FALSE)

source("R/config.R")
source("R/utils.R")
source("R/download.R")
source("R/geography.R")
source("R/adapters.R")
source("R/pipeline.R")

run_humanitarian_indices_pipeline(refresh = "--refresh" %in% args)

PROJECT_ROOT <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
project_path <- function(...) file.path(PROJECT_ROOT, ...)

source(project_path("R", "config.R"))
source(project_path("R", "utils.R"))
source(project_path("R", "download.R"))
source(project_path("R", "geography.R"))
source(project_path("R", "adapters.R"))
source(project_path("R", "pipeline.R"))

#!/usr/bin/env Rscript

set.seed(202022026)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("3 parameters are required: rmd-file name, diagnosis and Ion mode(s)")
}

rmd_file <- args[1]
diags_list <- gsub("\"", "", args[2])
ion_mode <- as.integer(args[3])

if (!file.exists(rmd_file)) {
  stop("File not found: ", rmd_file)
}
rmd_file_name <- basename(rmd_file)
workspace_folder <- sub(paste0("/", rmd_file_name), "", rmd_file)
renv::load(workspace_folder)
if (!grepl("\\.Rmd$|\\.rmd$", rmd_file)) {
  stop("File is not a Rmd-file: ", rmd_file)
}

diags_data <- stringi::stri_split_fixed(
  diags_list,
  ", ",
  simplify = TRUE
)
if (dim(diags_data)[[1]] == 0) {
  stop("Wrong diags list was set!")
}
target_path <- file.path(
  workspace_folder,
  "data",
  paste0(c("diag", diags_data[1, ]), collapse = "_")
)
if (!is.na(ion_mode)) {
  target_path <- file.path(
    target_path,
    paste0("exp_1.res_1.mode_", ion_mode, "dev_2.mz_2")
  )
}
if (!dir.exists(target_path)) {
  cat(target_path, " doesn't exist.\nLet's create it!", fill = TRUE)
  dir.create(target_path, recursive = TRUE)
}
target_rmd <- file.path(
  target_path,
  rmd_file_name
)

file.copy(
  rmd_file,
  target_rmd,
  overwrite = TRUE
)
tryCatch(
  {
    rmarkdown::render(
      target_rmd,
      params = list(
        options = list(
          run_dir = workspace_folder,
          work_dir = target_path,
          diags_data = diags_data,
          mode = ion_mode
        )
      ),
      clean = FALSE,
      envir = new.env()
    )
  },
  error = function(e) {
    cat(
      format(Sys.time(), "%Y-%m-%d %H-%M"),
      e$message, "\r\n",
      file = sub(".Rmd", ".try.out", target_rmd),
      append = TRUE
    )
    cat(e$message, "\n")
    "error"
  }
)

message("Done!")
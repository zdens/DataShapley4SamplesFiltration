# Run Shapley values calculation

set.seed(202022026)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Ion mode must be specified!")
}

report_file_name <- "shapley_mislabling_smote.Rmd"
run_dir <- getwd()

target_diag <- 6
base_diag <- 32

run_params <- list(
  expt = c(1),
  res = c(1),
  mode = c(args[1]),
  dev = c(2),
  mz = c(2)
)

run_params_grid <- expand.grid(run_params)

render_rmd <- function(
  report_file_name, data_dir, params
) {
  if (!dir.exists(data_dir)) {
    cat(data_dir, " doesn't exist.\nLet's create it!", fill = TRUE)
    dir.create(data_dir, recursive = TRUE)
  }
  file_name <- file.path(
    data_dir,
    report_file_name
  )
  file.copy(
    file.path(
      run_dir,
      report_file_name
    ),
    file_name,
    overwrite = TRUE
  )
  rs <- tryCatch(
    {
      rmarkdown::render(
        file_name,
        params = params,
        clean = FALSE,
        envir = new.env()
      )
      file_name
    },
    error = function(e) {
      cat(
        format(Sys.time(), "%Y-%m-%d %H-%M"),
        e$message,
        file = sub(".Rmd", ".try.out", file_name),
        append = TRUE
      )
      cat(e$message, "\n")
      "error"
    }
  )
  rs
}

res <- lapply(
  seq_len(nrow(run_params_grid)),
  function(i) {
    params_i <- run_params_grid[i, ]
    dir_name <- paste0(
      names(run_params_grid),
      "_", run_params_grid[i, ], collapse = "."
    )
    data_dir <- file.path(
      run_dir,
      "data",
      paste0("diag_", target_diag, "_", base_diag),
      dir_name
    )
    cat(
      format(Sys.time(), "%Y-%m-%d %H-%M"),
      "Start at",
      data_dir,
      append = TRUE,
      fill = TRUE
    )
    render_rmd(
      report_file_name, data_dir,
      list(
        options = list(
          acq_params = as.list(params_i),
          run_dir = run_dir,
          work_dir = data_dir,
          target_diag = target_diag,
          base_diag = base_diag
        )
      )
    )
  }
)
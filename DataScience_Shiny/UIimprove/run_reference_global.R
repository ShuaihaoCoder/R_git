# Run DataScience_reference_optimized.R without source() line-decoding issues.
# Every expression is evaluated in .GlobalEnv so intermediate objects remain
# available for interactive work after this runner finishes.

reference_path <- file.path(
  dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE)),
  "DataScience_reference_optimized.R"
)

reference_size <- file.info(reference_path)$size
reference_raw <- readBin(reference_path, what = "raw", n = reference_size)
reference_text <- rawToChar(reference_raw)
Encoding(reference_text) <- "UTF-8"
reference_text <- gsub("\r\n", "\n", reference_text, fixed = TRUE)
reference_text <- gsub("\r", "\n", reference_text, fixed = TRUE)

reference_expressions <- parse(
  text = reference_text,
  keep.source = FALSE,
  encoding = "UTF-8"
)

for (reference_index in seq_along(reference_expressions)) {
  eval(reference_expressions[[reference_index]], envir = .GlobalEnv)
}

message(
  "REFERENCE COMPLETE: ",
  length(reference_expressions),
  " expressions; tep=",
  exists("tep", envir = .GlobalEnv, inherits = FALSE),
  "; summary_df=",
  exists("summary_df", envir = .GlobalEnv, inherits = FALSE),
  "; final_table=",
  exists("final_table", envir = .GlobalEnv, inherits = FALSE)
)

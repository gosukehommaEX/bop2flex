# Software example for the supplementary material of the article on sample
# size deviations in the BOP2 and BOP2-FE designs.
#
# Run from the package root (open bop2flex.Rproj first):
#   devtools::load_all()
#   source(file.path("inst", "supplementary", "software_example.R"))
#
# Each chunk below is evaluated, and its code and printed output are written
# as an R console transcript to
# inst/supplementary/output/software/software_example.tex, which the
# supplementary document includes with \input. The output shown in the
# document is therefore produced by the code shown, not typed by hand. The
# example follows the worked example of the article (setting S1, BOP2-FE-1
# design with binding futility, actual analyses after 12, 16, 19, 25, 31, 37
# and 39 patients).

if (!exists("flex_design", mode = "function")) library(bop2flex)

out_dir <- file.path("inst", "supplementary", "output", "software")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(out_dir, full.names = TRUE))

# Width of the printed output; the document typesets the transcript in
# \footnotesize, which holds about 85 characters per line
old_options <- options(width = 84)

# Each chunk is one R expression, given as a character vector of lines. The
# first chunk is shown but not evaluated, because the package is already
# attached (by library() above or by devtools::load_all()).
chunks <- list(
  list(code = "library(bop2flex)", eval = FALSE),
  list(code = c("design <- flex_design(p0 = 0.2, p1 = 0.4,",
                "                      n = c(10, 15, 20, 25, 30, 35, 40),",
                "                      alpha = 0.1)"), eval = TRUE),
  list(code = "design", eval = TRUE),
  list(code = "design$calibration$equivalent", eval = TRUE),
  list(code = "flex_spending(design)", eval = TRUE),
  list(code = "flex_update(design, m = c(12, 16, 19))", eval = TRUE),
  list(code = "m <- c(12, 16, 19, 25, 31, 37, 39)", eval = TRUE),
  list(code = "y <- c(3, 5, 6, 7, 9, 10, 11)", eval = TRUE),
  list(code = "flex_update(design, m = m, final = TRUE, y = y)", eval = TRUE),
  list(code = "flex_naive(design, m = m, final = TRUE, y = y)", eval = TRUE),
  list(code = "oc <- flex_compare(design, list(m))", eval = TRUE),
  list(code = "oc[, c(\"method\", \"type1\", \"power\", \"en0\", \"en1\")]",
       eval = TRUE)
)

env <- new.env(parent = globalenv())
transcript <- character(0)
for (ch in chunks) {
  prompt <- c("> ", rep("+ ", length(ch$code) - 1))
  transcript <- c(transcript, paste0(prompt, ch$code))
  if (ch$eval) {
    expr <- parse(text = paste(ch$code, collapse = "\n"))
    if (length(expr) != 1) stop("Each chunk must contain one expression.")
    res <- withVisible(eval(expr[[1]], envir = env))
    if (res$visible) {
      transcript <- c(transcript, utils::capture.output(print(res$value)))
    }
  }
}
options(old_options)

# Guard against lines that would not fit the typeset width
too_long <- nchar(transcript) > 84
if (any(too_long)) {
  stop("Transcript lines longer than 84 characters: ",
       paste(which(too_long), collapse = ", "))
}

writeLines(c("{\\footnotesize", "\\begin{verbatim}", transcript,
             "\\end{verbatim}", "}"),
           file.path(out_dir, "software_example.tex"))
message("Software example written to ",
        file.path(out_dir, "software_example.tex"))

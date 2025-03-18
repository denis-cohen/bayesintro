## Functions ----
inline_child <- function(lines, output_path) {
  # Extract the child file path using regular expression
  child_path <- stringr::str_extract(lines, "<<\"([^\"]+)\">>")

  # If a child file path is found
  if (!is.na(child_path)) {
    # Strip the surrounding <<>> and quotes from the path
    child_path <- gsub("^<<\"|\">>$", "", child_path)

    # Check if the child file exists
    if (file.exists(child_path)) {
      # Read the content of the child file
      child_content <- paste(readLines(child_path, warn = FALSE), collapse = "\n")
    } else {
      # Handle missing child file
      child_content <- paste0("<!-- Missing child: ", child_path, " -->")
    }

    # Replace the placeholder with the child content in the entire string
    lines <- stringr::str_replace_all(lines, stringr::fixed(stringr::str_c("<<\"", child_path, "\">>")), child_content)
  }

  # Step 3: Write the final flattened content to the output file
  writeLines(lines, output_path, useBytes = TRUE)
}


## Meta information ----
meta <- rio::import("rmd/meta.csv")
formats <- c("learnr", "pdf", "html", "html_sol")

## Apply function within nested loop ----
for (i in seq_len(nrow(meta))) {
  ibu <- i

  ## Available formats
  available_formats <- formats[meta[i, formats] != ""]

  for (j in available_formats) {
    i <- ibu
    if (j == "learnr") {
      params <- list(
        title = meta$title[i],
        content = paste0("rmd/", meta$file[i])
      )

      inline_child(
        lines = do.call(knitr::knit_expand, c(list(file = paste0("rmd/", j, ".rmd")), params)),
        output_path = paste(meta[i, j], meta$file[i], sep = "/")
      )
    } else if (j == "pdf") {
      params <- list(
        title = meta$title[i],
        content = meta$file[i]
      )

      ## Render
      rmarkdown::render(
        input = paste0("rmd/", j, ".rmd"),
        output_dir = meta[i, j],
        output_file = gsub("rmd", "pdf", meta$file[i]),
        params = params
      )
    } else if (j == "html") {
      params <- list(
        title = meta$title[i],
        content = meta$file[i]
      )

      ## Render
      rmarkdown::render(
        input = paste0("rmd/", j, ".rmd"),
        output_dir = meta[i, j],
        output_file = gsub("rmd", "html", meta$file[i]),
        params = params
      )
    } else if (j == "html_sol") {
      params <- list(
        title = meta$title[i],
        content = meta$file[i]
      )

      ## Render
      rmarkdown::render(
        input = paste0("rmd/", j, ".rmd"),
        output_dir = meta[i, j],
        output_file = gsub(".rmd", "-with-solutions.html", meta$file[i]),
        params = params
      )
    }
  }
}

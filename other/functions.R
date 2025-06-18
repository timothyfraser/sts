# functions.R

# Helper functions for the course:

# Visualize a file/folder tree
file_tree <- function(path = ".", exclude_folders = c("images"), desc = FALSE) {
  
  folder_sort = switch(as.character(desc), "TRUE" = 'desc', "FALSE" = "asc")
  
  if (!dir.exists(path)) stop("Directory does not exist: ", path)
  
  read_gitignore <- function(folder) {
    gi_path <- file.path(folder, ".gitignore")
    if (!file.exists(gi_path)) return(character(0))
    lines <- readLines(gi_path, warn = FALSE)
    lines <- trimws(lines)
    lines <- lines[!grepl("^#", lines)]        # Remove comments
    lines <- lines[nzchar(lines)]              # Remove blanks
    lines
  }
  
  patterns_to_regex <- function(patterns) {
    sapply(patterns, function(pat) {
      pat <- gsub("/$", "", pat)
      pat <- glob2rx(pat)
      paste0("^", pat, "$")
    }, USE.NAMES = FALSE)
  }
  
  should_ignore <- function(file, regexes) {
    any(sapply(regexes, function(rx) grepl(rx, basename(file))))
  }
  
  recurse <- function(current_path, prefix = "", is_last = TRUE, is_top = FALSE, parent_ignores = character(0)) {
    patterns <- read_gitignore(current_path)
    ignores <- c(parent_ignores, patterns_to_regex(patterns))
    
    items <- list.files(current_path, full.names = TRUE, all.files = FALSE)
    items <- items[!sapply(items, should_ignore, regexes = ignores)]
    
    if (length(items) == 0) return()
    
    folders <- items[file.info(items)$isdir]
    files <- items[!file.info(items)$isdir]
    
    # Exclude named folders
    folders <- folders[!basename(folders) %in% exclude_folders]
    
    # Sort folders by name
    folder_names <- basename(folders)
    order_idx <- order(folder_names, decreasing = (folder_sort == "desc"))
    folders <- folders[order_idx]
    
    if (is_top) {
      for (i in seq_along(files)) {
        name <- basename(files[i])
        connector <- if (i == length(files) && length(folders) == 0) "└──" else "├──"
        cat(prefix, connector, " 📄 ", name, "\n", sep = "")
      }
    }
    
    for (i in seq_along(folders)) {
      folder <- folders[i]
      name <- basename(folder)
      is_last_folder <- i == length(folders)
      connector <- if (is_last_folder) "└──" else "├──"
      cat(prefix, connector, " 📁 ", name, "\n", sep = "")
      new_prefix <- paste0(prefix, if (is_last_folder) "    " else "│   ")
      recurse(folder, new_prefix, is_last = TRUE, parent_ignores = ignores)
    }
  }
  
  cat("📁 ", basename(normalizePath(path)), "\n", sep = "")
  recurse(path, prefix = "", is_top = TRUE)
}


file_tree(path = ".", desc = TRUE, exclude_folders = "images")


file_tree(path = "workshops", desc = TRUE, exclude_folders = "images")

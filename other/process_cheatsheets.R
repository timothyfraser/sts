#' @name other/process_cheatsheets.R
#' @description
#' Script for splitting cheatsheets from pdfs 
#' into pngs so they can be easily posted on this repository.


library(dplyr)
library(pdftools)
library(stringr)


tibble(file = dir("docs", pattern = ".pdf", full.names = TRUE)) %>%
  filter(str_detect(file, pattern ="cheatsheet" )) %>%
  # For each file, get all pages
  group_by(file) %>%
  reframe(pages = pdf_info(file) %>% with(pages) %>% 1:.) %>%
  ungroup() %>%
  # Format the output file path
  mutate(output = file %>% str_remove(".pdf") %>% paste0("_", pages, ".png")) %>%
  # Make a unique ID
  mutate(id = 1:n())  %>%
  # For each file...
  split(.$id) %>%
  # Convert from pdf to image
  purrr::walk(~pdftools::pdf_convert(pdf = .$file, filenames = .$output, pages = .$pages))

wd <- paste("~/Library/CloudStorage/Box-Box/",
            "Longitudinal_omics_review/TeXfiles/",sep = "")
setwd(wd)
input_file <- "LongitudinalOmics2.txt"# "queries.tsv"
out_dir <- "pubmed_report2026"

email <- "ali.taheriyoun@gwu.edu"
api_key <- NULL #"YOUR_NCBI_API_KEY"   # set to NULL if you do not have one
library(rentrez)
library(dplyr)
library(purrr)
library(readr)
library(httr)

# -----------------------------
# User settings
# -----------------------------
# api_key <- "paste_your_real_ncbi_api_key_here"

start_year <- 2026
min_year <- 1920
datetype <- "pdat"

# -----------------------------
# Setup
# -----------------------------

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

if (!is.null(api_key) && api_key != "" && api_key != "YOUR_NCBI_API_KEY") {
  rentrez::set_entrez_key(api_key)
}

queries <- readr::read_tsv(input_file, show_col_types = FALSE)

print(queries)

# -----------------------------
# Fetch one PubMed count
# -----------------------------

fetch_pubmed_count <- function(term, year, email = NULL, datetype = "pdat") {
  
  message("Fetching: ", term, " | year: ", year)
  
  email_config <- NULL
  
  if (!is.null(email)) {
    email_config <- httr::config(
      useragent = paste0("R rentrez PubMed count script; email=", email)
    )
  }
  
  res <- tryCatch(
    {
      rentrez::entrez_search(
        db = "pubmed",
        term = term,
        retmax = 0,
        mindate = paste0(year, "/01/01"),
        maxdate = paste0(year, "/12/31"),
        datetype = datetype,
        config = email_config
      )
    },
    error = function(e) {
      message("Error for term: ", term, " year: ", year)
      message(e$message)
      return(NULL)
    }
  )
  
  if (is.null(res)) {
    return(NA_integer_)
  }
  
  as.integer(res$count)
}

# -----------------------------
# Fetch yearly data for one query
# Stops at min_year
# -----------------------------

get_year_data <- function(search_query,
                          start_year,
                          min_year = 1920,
                          email = NULL,
                          datetype = "pdat",
                          sleep_seconds = 0.35) {
  
  years <- integer()
  counts <- integer()
  
  for (year in seq(from = start_year, to = min_year, by = -1)) {
    
    count <- fetch_pubmed_count(
      term = search_query,
      year = year,
      email = email,
      datetype = datetype
    )
    
    years <- c(years, year)
    counts <- c(counts, count)
    
    Sys.sleep(sleep_seconds)
  }
  
  tibble(
    year = years,
    count = counts
  ) %>%
    filter(!is.na(count), count > 0)
}

# -----------------------------
# Run all queries
# -----------------------------

pubmed_data <- purrr::map_dfr(seq_len(nrow(queries)), function(i) {
  
  tmp <- get_year_data(
    search_query = queries$query[i],
    start_year = start_year,
    min_year = min_year,
    email = email,
    datetype = datetype
  )
  
  tmp %>%
    mutate(
      main_term = queries$main_term[i],
      sub_term = queries$sub_term[i]
    )
})

# -----------------------------
# Save output
# -----------------------------

readr::write_tsv(
  pubmed_data,
  file.path(out_dir, "pubmed_data.tsv")
)

pubmed_data

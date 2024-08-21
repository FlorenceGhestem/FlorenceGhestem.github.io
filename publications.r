
create_pub_listing <- function(bib_file) {
  bib <- strsplit(paste(readLines(bib_file), collapse = "\n"), "\n@")[[1]]
  articles <- lapply(
    X = paste0("@", bib[bib != ""]),
    FUN = function(ibib) {
      f <- tempfile()
      on.exit(unlink(f))
      writeLines(ibib, f)
      as.yaml(
        list(
          list(
            title = sub(".*title = \\{([^}]*)\\}.*", "\\1", ibib),
            path = sub(".*url = \\{([^}]*)\\}.*", "\\1", ibib),
            date = paste(
              sub("month = ", "", regmatches(ibib, gregexpr("month = (\\w+)", ibib))[[1]]),
              sub(".*year = \\{([^}]*)\\}.*", "\\1", ibib)
            ),
            journal = sub(".*journal = \\{([^}]*)\\}.*", "\\1", ibib),
            keywords = sub(".*keywords = \\{([^}]*)\\}.*", "\\1", ibib)
          )
        )
      )
    }
  )
  writeLines(text = unlist(articles), con = sub("\\.bib$", ".yml", bib_file))
}

create_pub_listing("publications.bib")

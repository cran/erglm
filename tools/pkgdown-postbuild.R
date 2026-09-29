#!/usr/bin/env Rscript
# pkgdown renders every *.md file it finds at the package root (and in
# .github/) into its own docs/*.html page -- see pkgdown:::package_mds().
# That skip-list is hard-coded inside pkgdown (only issue/PR templates and
# cran-comments.md are excluded) and isn't configurable via _pkgdown.yml,
# so .Rbuildignore-ing AGENTS.md -- which keeps it out of the built
# *package* -- has no effect on the *pkgdown site*. (.agents/PLAN.md and
# .agents/HISTORY.md live in a subdirectory pkgdown doesn't scan for this
# purpose, so they need no equivalent handling.) Run this script after
# pkgdown::build_site()/build_site_github_pages() to strip the resulting
# maintainer-only page back out of docs/ before deploying.

docs <- "docs"
stray <- c("AGENTS.md", "AGENTS.html")
stray_paths <- file.path(docs, stray)
removed <- stray_paths[file.exists(stray_paths)]
if (length(removed) > 0) {
  file.remove(removed)
  message("Removed stray pkgdown pages: ", paste(basename(removed), collapse = ", "))
} else {
  message("No stray AGENTS page found in docs/ -- nothing to do.")
}

# Also drop it from the search index and sitemap so it doesn't linger
# as a dead link/search result.
search_json <- file.path(docs, "search.json")
if (file.exists(search_json)) {
  entries <- jsonlite::fromJSON(search_json, simplifyVector = FALSE)
  is_stray_entry <- function(entry) {
    path <- entry$path
    length(path) == 1 && grepl("AGENTS\\.html$", path)
  }
  entries <- entries[!vapply(entries, is_stray_entry, logical(1))]
  jsonlite::write_json(entries, search_json, auto_unbox = TRUE)
  message("Pruned AGENTS entry from search.json")
}

sitemap <- file.path(docs, "sitemap.xml")
if (file.exists(sitemap)) {
  lines <- readLines(sitemap)
  kept <- lines[!grepl("AGENTS\\.html", lines)]
  if (length(kept) != length(lines)) {
    writeLines(kept, sitemap)
    message("Pruned AGENTS entry from sitemap.xml")
  }
}

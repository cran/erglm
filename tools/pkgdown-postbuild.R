#!/usr/bin/env Rscript
# pkgdown renders every *.md file it finds at the package root (and in
# .github/) into its own docs/*.html page -- see pkgdown:::package_mds().
# That skip-list is hard-coded inside pkgdown (only issue/PR templates and
# cran-comments.md are excluded) and isn't configurable via _pkgdown.yml,
# so .Rbuildignore-ing AGENTS.md/PLAN.md -- which keeps them out of the
# built *package* -- has no effect on the *pkgdown site*. Run this script
# after pkgdown::build_site()/build_site_github_pages() to strip the
# resulting maintainer-only pages back out of docs/ before deploying.

docs <- "docs"
stray <- c("AGENTS.md", "AGENTS.html", "PLAN.md", "PLAN.html")
stray_paths <- file.path(docs, stray)
removed <- stray_paths[file.exists(stray_paths)]
if (length(removed) > 0) {
  file.remove(removed)
  message("Removed stray pkgdown pages: ", paste(basename(removed), collapse = ", "))
} else {
  message("No stray AGENTS/PLAN pages found in docs/ -- nothing to do.")
}

# Also drop them from the search index and sitemap so they don't linger
# as dead links/search results.
search_json <- file.path(docs, "search.json")
if (file.exists(search_json)) {
  entries <- jsonlite::fromJSON(search_json, simplifyVector = FALSE)
  is_stray_entry <- function(entry) {
    path <- entry$path
    length(path) == 1 && grepl("AGENTS\\.html$|PLAN\\.html$", path)
  }
  entries <- entries[!vapply(entries, is_stray_entry, logical(1))]
  jsonlite::write_json(entries, search_json, auto_unbox = TRUE)
  message("Pruned AGENTS/PLAN entries from search.json")
}

sitemap <- file.path(docs, "sitemap.xml")
if (file.exists(sitemap)) {
  lines <- readLines(sitemap)
  kept <- lines[!grepl("AGENTS\\.html|PLAN\\.html", lines)]
  if (length(kept) != length(lines)) {
    writeLines(kept, sitemap)
    message("Pruned AGENTS/PLAN entries from sitemap.xml")
  }
}

#!/usr/bin/env Rscript
# Artifact checks only; xml2 is a QA dependency, not a rendering dependency.
args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
assets <- dirname(normalizePath(sub("^--file=", "", args[[1L]])))
dev <- dirname(assets)
library(xml2)
doc <- read_html(file.path(dev, "repair-uncertainty-methods.html"))
math <- xml_find_all(doc, '//*[local-name()="math"]')
stopifnot(length(math) == 178L, sum(xml_attr(math, "display") == "block") == 41L,
          length(xml_find_all(doc, '//*[local-name()="merror"]')) == 0L)
ids <- xml_attr(xml_find_all(doc, "//*[@id]"), "id")
href <- xml_attr(xml_find_all(doc, "//a[@href]"), "href")
stopifnot(!anyDuplicated(ids),
          all(substring(href[startsWith(href, "#")], 2L) %in% ids))
relative <- href[!grepl("^(#|[[:alnum:]+.-]+:|/)", href)]
stopifnot(all(file.exists(file.path(dev, URLdecode(sub("#.*$", "", relative))))))
resources <- c(xml_attr(xml_find_all(doc, "//script[@src]"), "src"),
               xml_attr(xml_find_all(doc, '//link[@rel="stylesheet"]'), "href"),
               xml_attr(xml_find_all(doc, "//img[@src]"), "src"))
stopifnot(all(startsWith(resources, "data:")),
          length(xml_find_all(doc, "//main//img")) == 13L,
          length(xml_find_all(doc, '//*[@class="csl-entry"]')) == 5L)
prose <- xml_text(xml_find_all(doc, "//main//p[not(.//math) and not(.//code)]"))
stopifnot(!any(grepl("\\\\(widehat|psi|xi|ldots)|@eq-|@fig-", prose)))
for (name in c("earlier-predictor-coverage", "legacy-outcome-coverage", "fresh-interval-construction")) {
  cells <- read.csv(file.path(assets, paste0(name, ".csv")))
  stopifnot(all(cells$coverage_mc_low >= .5), all(cells$coverage_mc_high <= 1))
}
cat("PASS 178 MathML expressions (41 display), no math errors or raw prose fragments.\n",
    "PASS 13 embedded graphics, styles/scripts embedded, 5 bibliography entries.\n",
    "PASS unique IDs, internal references and all relative companion links.\n",
    "PASS all new coverage bounds inside 50–100%; 96 endpoint cells checked on render.\n", sep = "")

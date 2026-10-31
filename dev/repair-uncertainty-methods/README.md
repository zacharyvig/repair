# Uncertainty methods report assets

The adjacent `../repair-uncertainty-methods.qmd` is the methods synthesis; its
rendered HTML embeds all figures, styles and native MathML equations. Source
links are navigation links to companion files, not rendering dependencies.

From the package root:

```sh
quarto render dev/repair-uncertainty-methods.qmd
Rscript dev/robust-refit-simulation/verify-results.R
Rscript dev/influence-simulation/verify-results.R
Rscript dev/leverage-simulation/verify-results.R
Rscript dev/repair-uncertainty-methods/verify-report.R
```

Rendering uses R packages ggplot2, scales, knitr and rmarkdown; it does not fit
models, generate simulation replications, load compiled repair, or source the
legacy GPL research helper. `figures.R` reads the frozen endpoint and summary
bundles relative to `dev/`, exports four PNG/PDF figures and CSV inputs, and
checks all 96 new coverage cells against their actual interval endpoints,
denominators and Wilson bounds. It asserts every coverage bound fits the
50–100% display. The deletion-transition plot shows all positive retained
relative RMSE values, with no clipping or trimming, and checks its strata counts.

The other figures are reused byte-for-byte from the robust and fresh study
bundles. Their existing manifests and endpoint/plot validators remain unchanged.
The reused influence cohort and fresh leverage cohort are labeled separately;
no pooled coverage estimate or new confirmatory replication is created.

`references.bib` identifies the unpublished manuscript separately from published
primary methodology. `report.css` supplies minimal layout/print styling.
`verify-report.R` (additional QA dependency: xml2) checks the rendered mathematics,
embedded resources, bibliography, internal IDs and companion-file links. Its
fixed expression/image counts intentionally detect a lost block or inline-math
rendering regression; update them deliberately if the document's content changes.
See the report's provenance section and the original bundle protocols for
numerical source commits, seed/hash evidence and limitations. This addition
changes documentation and presentation only.

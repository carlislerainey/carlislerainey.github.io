# CV (Typst source)

**Build:** `typst compile _cv/cv.typ _cv/cv.pdf` (run from the repo root; Typst 0.14+).
Add `--watch` while editing to rebuild on save.

**Edit `cv.typ` for content** — name, contact, entries, citations, links. It is plain
Typst markup (`= Section`, `== Subsection`, `- entry`) plus a few helpers.

**Edit `cv-style.typ` only for looks** — page setup, headings, footer, address block,
teaching grid, and the bracketed-link helpers (`#doi(url)`, `#pdf(url)`, `#lk(label, url)`, …).
You should not need to touch it to update the CV.

**Publishing:** the website serves the CV from the repo root — `_quarto.yml` lists `cv.pdf`
under `resources` and `index.qmd` links to `cv.pdf`. After building, either copy
`_cv/cv.pdf` over the root `cv.pdf`, or point `_quarto.yml` and `index.qmd` at `_cv/cv.pdf`.
The build here never overwrites the root `cv.pdf`.

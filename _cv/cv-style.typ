// ============================================================================
// cv-style.typ — layout and formatting for the CV.
//
// Everything here is presentation. To update the CV's *content*, edit cv.typ;
// you should never need to touch this file.
//
// Design notes: Typst defaults are used wherever they are good enough
// (Libertinus Serif, default text size, default margins, default list marker).
// The only deliberate departures are US Letter paper, an asymmetric title block,
// slightly tighter heading spacing, underlined links, and the page footer.
// No external packages are used.
// ============================================================================

// --- Bracketed hyperlink, e.g. [DOI] with only "DOI" underlined ------------
// The square brackets sit outside the link, matching the original CV.
#let lk(label, url) = [\[#link(url)[#label]\]]

// Convenience wrappers for the link labels this CV uses.
#let doi(url) = lk("DOI", url)
#let pdf(url) = lk("PDF", url)
#let errata(url) = lk("Errata", url)
#let website(url) = lk("Website", url)
#let dropbox(url) = lk("Dropbox", url)
#let materials(url) = lk("materials", url)

// --- Small italic annotation line (used under "Papers" and "Recent Talks") --
#let note(body) = block(
  above: 0.4em,
  below: 0.7em,
  text(size: 0.92em, style: "italic", fill: luma(40%), body),
)

// --- A plain, un-bulleted paragraph inside a section (the Department lists) -
#let plain(body) = block(above: 0.5em, below: 0.5em, body)

// --- Light italic sub-heading (the "Current"/"Past" splits in Appointments) -
// Deliberately not a real heading: it groups entries without the weight of the
// bold level-2 headings used elsewhere.
#let subhead(name) = block(above: 0.8em, below: 0.3em, emph(name))

// --- Sub-heading for a degree-granting institution in Education ------------
#let institution(name) = block(above: 0.7em, below: 0.25em, name)

// --- Two-column address block ---------------------------------------------
// Left: postal address and email. Right: online identifiers.
#let address-block(left-col, right-col) = block(
  above: 0.6em,
  below: 0.6em,
  grid(
    columns: (1fr, 1fr),
    column-gutter: 1em,
    left-col,
    right-col,
  ),
)

// --- Teaching grid ---------------------------------------------------------
// Takes any number of dictionaries: (school: "FSU", undergraduate: [...],
// graduate: [...]). Renders a stroke-less grid with the school in the left
// column and the two course lines on the right.
#let teaching(..rows) = {
  let cells = ()
  for r in rows.pos() {
    cells.push(text(size: 0.95em, r.school))
    cells.push[
      #strong(emph[Undergraduate:]) #r.undergraduate \
      #strong(emph[Graduate:]) #r.graduate
    ]
  }
  block(above: 0.6em, grid(
    columns: (auto, 1fr),
    column-gutter: 1em,
    row-gutter: 0.9em,
    align: (right + horizon, left),
    ..cells,
  ))
}

// --- The document template -------------------------------------------------
#let cv(
  name: "",
  title: "Curriculum Vitae",
  footer-name: "",
  body,
) = {
  set page(
    paper: "us-letter",
    // margins left at Typst's default (~1in on US Letter)
    footer: context {
      set align(center)
      set text(size: 9pt, style: "italic", fill: luma(40%))
      [Page #counter(page).display("1 of 1", both: true) — #footer-name — CV]
    },
  )

  // Slightly tighter than the Typst default (0.65em) so the CV holds to four pages.
  set par(leading: 0.6em)
  set list(spacing: 0.55em, indent: 0.3em)
  show link: underline

  set heading(numbering: none)
  show heading.where(level: 1): set text(size: 1.3em)
  show heading.where(level: 1): set block(above: 1.3em, below: 0.55em)
  show heading.where(level: 2): set text(size: 1.05em)
  show heading.where(level: 2): set block(above: 0.95em, below: 0.45em)

  // Title block: the name flush left with "Curriculum Vitae" set quietly in
  // small caps at the right of the same line, over a hairline rule.
  block(width: 100%, below: 0.7em, grid(
    columns: (1fr, auto),
    align: bottom,
    text(size: 1.9em, name),
    text(fill: luma(50%), smallcaps(title)),
  ))
  line(length: 100%, stroke: 0.5pt + luma(60%))

  body
}

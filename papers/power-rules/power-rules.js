// Power Rules, the HTML version (make page). Four small behaviors; the page reads
// the same without them.
//   1. The contents: open in the left margin on wide screens (closed under the abstract on
//      narrow ones), with the section being read marked.
//   2. The margin notes: on wide screens, each is placed beside its mark (pushed down only to
//      clear the note before it, and Table 2), so that an open code box can run across the
//      margin; a note the open box overlaps is hidden until the box closes. Pointing at a note
//      or its number marks both. Without the script, the notes float, and code boxes stay the
//      width of the text.
//   3. A copy button in each code box (copies the code, not its output; in a box with tabs,
//      the open tab's code).
//   4. On hover, a preview of the reference a citation points to, or of the rule or table a
//      cross-reference points to.
//   5. Tabs in the code boxes (Base R / DeclareDesign): choosing a tab chooses the same tab in
//      every box of its group, and the choice is kept for the next visit. Without the script,
//      each box's tabs work on their own.

(() => {
  // 1. contents ---------------------------------------------------------------
  const toc = document.querySelector("#TOC details");
  const wide = matchMedia("(min-width: 73rem)");
  const setOpen = () => {  // in the margin the contents stay open: no toggle to tab to
    if (!toc) return;
    toc.open = wide.matches;
    toc.querySelector("summary").tabIndex = wide.matches ? -1 : 0;
  };
  setOpen();
  wide.addEventListener("change", setOpen);

  const links = [...document.querySelectorAll("#TOC a")];
  const heads = links.map(a => document.getElementById(decodeURIComponent(a.hash.slice(1))));
  let ticking = false;
  const mark = () => {
    ticking = false;
    let i = -1;
    heads.forEach((h, j) => { if (h && h.getBoundingClientRect().top < 140) i = j; });
    const nav = document.getElementById("TOC"), a = links[i];
    if (a && wide.matches) {  // keep the reader's place in view when the contents are taller than the window
      if (a.offsetTop < nav.scrollTop + 40) nav.scrollTop = a.offsetTop - 40;
      else if (a.offsetTop + a.offsetHeight > nav.scrollTop + nav.clientHeight - 40)
        nav.scrollTop = a.offsetTop + a.offsetHeight - nav.clientHeight + 40;
    }
    links.forEach((a, j) => {
      a.classList.toggle("active", j === i);
      if (j === i) a.setAttribute("aria-current", "location"); else a.removeAttribute("aria-current");
    });
  };
  addEventListener("scroll", () => { if (!ticking) { ticking = true; requestAnimationFrame(mark); } }, { passive: true });
  mark();

  // 2. margin notes --------------------------------------------------------------
  const main = document.querySelector("main");
  const notes = [...document.querySelectorAll(".sidenote")];
  const marks = notes.map(n => document.querySelector(`label[for="sn-${n.dataset.n}"]`));
  const obstacles = [...document.querySelectorAll("#tbl\\:how")];
  const notesWide = matchMedia("(min-width: 63rem)");
  document.documentElement.classList.add("js-notes");
  let queued = false;
  const place = () => {
    queued = false;
    if (!notesWide.matches) return;
    const origin = main.getBoundingClientRect().top;
    const blocked = obstacles.map(o => { const r = o.getBoundingClientRect(); return [r.top - origin, r.bottom - origin]; });
    let floor = -Infinity;
    notes.forEach((n, i) => {
      const h = n.offsetHeight;
      let top = Math.max(marks[i].getBoundingClientRect().top - origin - 3, floor);
      blocked.forEach(([a, b]) => { if (top < b && top + h > a) top = b + 16; });
      n.style.top = `${Math.round(top)}px`;
      floor = top + h + 16;
    });
    // an open code box runs across the margin: hide the notes it overlaps, whole
    const open = [...document.querySelectorAll("details.code[open]")].map(d => d.getBoundingClientRect());
    notes.forEach(n => {
      const r = n.getBoundingClientRect();
      n.classList.toggle("covered", open.some(b => r.top < b.bottom && r.bottom > b.top));
    });
  };
  const queue = () => { if (!queued) { queued = true; requestAnimationFrame(place); } };
  new ResizeObserver(queue).observe(main);  // fonts, math, opened code, window width
  document.querySelectorAll("details.code").forEach(d => d.addEventListener("toggle", queue));
  queue();

  // a note and its number, marked together under the pointer
  const pair = e => {
    const el = e.target.closest(".sidenote, .sidenote-number");
    if (!el) return;
    const n = el.matches(".sidenote") ? el.dataset.n : el.htmlFor.slice(3);
    const on = e.type === "mouseover";
    document.querySelector(`.sidenote[data-n="${n}"]`).classList.toggle("hl", on);
    document.querySelector(`label[for="sn-${n}"]`).classList.toggle("hl", on);
  };
  document.addEventListener("mouseover", pair);
  document.addEventListener("mouseout", pair);

  // 3. copy buttons --------------------------------------------------------------
  if (navigator.clipboard) {
    document.querySelectorAll("details.code").forEach(box => {
      const button = document.createElement("button");
      button.className = "copy";
      button.type = "button";
      button.textContent = "Copy";
      button.setAttribute("aria-live", "polite");
      button.addEventListener("click", () => {
        const code = [...box.querySelectorAll("pre.cell-code")]
          .filter(p => p.offsetParent !== null)  // not in a closed tab
          .map(p => p.innerText.replace(/\n$/, ""));
        navigator.clipboard.writeText(code.join("\n\n") + "\n").then(() => {
          button.textContent = "Copied";
          setTimeout(() => { button.textContent = "Copy"; }, 1500);
        });
      });
      box.querySelector("summary").after(button);
    });
  }

  // 5. tabs ------------------------------------------------------------------
  const tabsets = [...document.querySelectorAll(".tabs[data-group]")];
  const choose = (group, title) => {
    tabsets.filter(t => t.dataset.group === group).forEach(t => {
      const input = [...t.querySelectorAll("input")].find(i => t.querySelector(`label[for="${i.id}"]`).textContent === title);
      if (input) input.checked = true;
    });
  };
  tabsets.forEach(t => t.addEventListener("change", e => {
    const title = t.querySelector(`label[for="${e.target.id}"]`).textContent;
    choose(t.dataset.group, title);
    try { localStorage.setItem(`tabs-${t.dataset.group}`, title); } catch {}
  }));
  new Set(tabsets.map(t => t.dataset.group)).forEach(group => {
    let title = null;
    try { title = localStorage.getItem(`tabs-${group}`); } catch {}
    if (title) choose(group, title);
  });

  // 4. previews ----------------------------------------------------------------
  if (!matchMedia("(hover: hover) and (pointer: fine)").matches) return;
  const peek = document.createElement("div");
  peek.className = "peek";
  peek.setAttribute("role", "tooltip");
  document.body.append(peek);
  let timer, current;

  const appRules = document.getElementById("app:rules");
  const show = (a, target) => {
    // a table: the table alone, without its caption
    const copy = (target.id.startsWith("tbl:") ? target.querySelector("table") : target).cloneNode(true);
    copy.removeAttribute("id");
    copy.querySelectorAll("[id]").forEach(e => e.removeAttribute("id"));
    // a reference that repeats the authors above it prints a dash; alone, it needs the names
    const first = copy.firstChild;
    if (target.matches(".csl-entry") && first && first.nodeType === 3 && first.data.trimStart().startsWith("———")) {
      let prev = target.previousElementSibling;
      while (prev && prev.textContent.trimStart().startsWith("———")) prev = prev.previousElementSibling;
      const names = prev && prev.textContent.trim().match(/^(.*?)\. (?=\d{4}|n\.d\.)/);
      if (names) first.data = first.data.replace("———", names[1]);
    }
    peek.replaceChildren(copy);
    peek.classList.add("on");
    const r = a.getBoundingClientRect();
    const w = peek.offsetWidth, h = peek.offsetHeight;
    const note = a.closest(".sidenote");
    if (note && notesWide.matches) {  // from a margin note: open over the text, not over the note
      const nr = note.getBoundingClientRect();
      peek.style.left = `${Math.max(8, nr.left - w - 16) + scrollX}px`;
      peek.style.top = `${Math.min(r.top, innerHeight - h - 8) + scrollY}px`;
      return;
    }
    const left = Math.min(Math.max(8, r.left), document.documentElement.clientWidth - w - 8);
    const below = r.bottom + 8 + h < innerHeight;
    peek.style.left = `${left + scrollX}px`;
    peek.style.top = `${(below ? r.bottom + 8 : r.top - h - 8) + scrollY}px`;
  };
  const hide = () => { clearTimeout(timer); current = null; peek.classList.remove("on"); };

  document.addEventListener("mouseover", e => {
    const a = e.target.closest('a[href^="#ref-"], a[href^="#rule"], a[href="#tbl:factors"], a[href^="#tbl:mdes"]');
    if (!a || a === current || a.closest("#TOC")) return;
    // Appendix 4 restates the rules, so a rule link there would preview the box beside it
    if (appRules && a.hash.startsWith("#rule") && appRules.compareDocumentPosition(a) & Node.DOCUMENT_POSITION_FOLLOWING) return;
    const target = document.getElementById(decodeURIComponent(a.hash.slice(1)));
    if (!target || target.contains(a)) return;
    current = a;
    clearTimeout(timer);
    timer = setTimeout(() => show(a, target), 180);
  });
  document.addEventListener("mouseout", e => {
    if (current && !current.contains(e.relatedTarget)) hide();
  });
  addEventListener("scroll", hide, { passive: true });
})();

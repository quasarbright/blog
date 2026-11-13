// blog.js — small page behaviors, no dependencies.
//  * builds the table of contents in a post's left margin from its section headings
//  * highlights the section you're reading
//  * gives Markdown headings the same "#" link that scribble headings have
//  * closes the Tags menu on an outside click or Escape

(function () {
  "use strict";

  function headingText(h) {
    const clone = h.cloneNode(true);
    clone.querySelectorAll(".button-group, .heading-anchor").forEach((el) => el.remove());
    return clone.textContent.replace(/\s+/g, " ").trim();
  }

  // Where a heading's link should point. Scribble headings carry an <a name>,
  // Markdown headings an id.
  function headingHref(h) {
    const anchor = h.querySelector(".heading-anchor");
    if (anchor) return anchor.getAttribute("href");
    if (h.id) return "#" + encodeURIComponent(h.id);
    return null;
  }

  function addMarkdownAnchors(body) {
    body.querySelectorAll("h1[id], h2[id], h3[id], h4[id]").forEach((h) => {
      if (h.querySelector(".heading-anchor")) return;
      const a = document.createElement("a");
      a.className = "heading-anchor";
      a.href = "#" + encodeURIComponent(h.id);
      a.title = "Link to this section";
      a.textContent = "#";
      h.appendChild(a);
    });
  }

  function buildToc() {
    const body = document.querySelector(".post-body");
    const toc = document.querySelector(".post-toc");
    if (!body || !toc) return;
    addMarkdownAnchors(body);

    // Section levels: the top two heading levels the post actually uses.
    const all = Array.from(body.querySelectorAll("h1, h2, h3"));
    const levels = Array.from(new Set(all.map((h) => +h.tagName[1]))).sort();
    const [top, sub] = levels;
    const heads = all.filter((h) => {
      const l = +h.tagName[1];
      return (l === top || l === sub) && headingHref(h);
    });
    if (heads.filter((h) => +h.tagName[1] === top).length < 2) return;

    const title = document.createElement("p");
    title.className = "post-toc-title";
    title.textContent = "Contents";
    const ol = document.createElement("ol");
    const links = heads.map((h) => {
      const li = document.createElement("li");
      if (+h.tagName[1] !== top) li.className = "sub";
      const a = document.createElement("a");
      a.href = headingHref(h);
      a.textContent = headingText(h);
      li.appendChild(a);
      ol.appendChild(li);
      return a;
    });
    toc.append(title, ol);
    toc.hidden = false;

    // The current section is the last heading above the top fifth of the screen.
    let ticking = false;
    function update() {
      ticking = false;
      const line = window.innerHeight * 0.2;
      let current = -1;
      heads.forEach((h, i) => {
        if (h.getBoundingClientRect().top <= line) current = i;
      });
      links.forEach((a, i) => a.classList.toggle("is-current", i === current));
      const active = links[current];
      if (active && toc.scrollHeight > toc.clientHeight) {
        const r = active.getBoundingClientRect(), t = toc.getBoundingClientRect();
        if (r.top < t.top || r.bottom > t.bottom) {
          toc.scrollTop += r.top - t.top - toc.clientHeight / 2;
        }
      }
    }
    window.addEventListener("scroll", () => {
      if (!ticking) { ticking = true; requestAnimationFrame(update); }
    }, { passive: true });
    window.addEventListener("resize", update);
    update();
  }

  function tagsMenu() {
    const menu = document.querySelector(".tags-menu");
    if (!menu) return;
    document.addEventListener("click", (e) => {
      if (menu.open && !menu.contains(e.target)) menu.open = false;
    });
    document.addEventListener("keydown", (e) => {
      if (e.key === "Escape" && menu.open) {
        menu.open = false;
        menu.querySelector("summary").focus();
      }
    });
  }

  function init() {
    buildToc();
    tagsMenu();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", init);
  else init();
})();

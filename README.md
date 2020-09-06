#Haskset

Port from AWK to Haskell of [Awkrdeck](https://github.com/rberenguel/awkrdeck):

> Convert presentations written for Deckset to be compatible with conversion via pandoc and reveal.js to HTML

---

## Installation

```bash
git clone https://github.com/rberenguel/haskset
cd haskset
stack install
```

should work. You need Deckset, beware

## Usage

Navigate to the correct folder where you have a presentation, and just run

```
haskset presentation.md
```

This will generate a `presentation.html` via Pandoc. You need a webserver to serve it, you can use the classic `python3 -m http.server 8080`.

## Notes and TODOs

- The CSS is not customizable yet (*TODO*)
- You can't pass options to Pandoc (*TODO*)
- Several features of Deckset are not implemented (some may never be, *TODO*)
- There is no useful debug information via flags (*TODO*)
- Add more test cases and edge test cases (*TODO*)

On the flip side, it works for simple presentations (may need some minor tweaking of the Markdown). You can see a freshly generated example [here](https://rberenguel.github.io/commoditisation-languages/commoditisation.html#/).
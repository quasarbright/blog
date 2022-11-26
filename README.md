# Mike Delmonaco's blog

This is the source code for [my blog](https://quasarbright.github.io/blog/).

This blog is built using [frog](https://github.com/greghendershott/frog).

It is hosted on GitHub Pages. In the master branch, generated files are ignored.
Through GitHub actions (see `.github/workflows/deploy.yml`), frog is invoked to
build the website. Every push to master triggers a build, wipes the `gh-pages` branch,
and pushes the source code and the generated files to the `gh-pages` branch. GitHub pages
serves html from this branch, not master.

## To Build Locally

Requirements:

* [Racket](https://download.racket-lang.org/) 
* [Python](https://www.python.org/downloads/)
* [Pygments](https://pygments.org/) (can install with `pip install pygments` once Python is installed)

Instructions:

```sh
raco pkg install
raco frog -bp
```

## Things I had to figure out and change

Here are the noteworthy things I had to figure out to get this working the way I wanted.
I'm including this information so that I can remember how all of this works, and also
to save anyone else some time and effort if they want to do anything similar.

* In order to get this hosted on GitHub Pages under `/blog`, I had to set [current-uri-prefix](https://docs.racket-lang.org/frog/parameters.html#%28def._%28%28lib._frog%2Fparams..rkt%29._current-uri-prefix%29%29)
in `frog.rkt`. I did not need to provide a `--root` flag when running `raco frog`, though.

* In order to keep generated files out of master, but still have GitHub Pages serve the site,
I used a GitHub action (see `.github/workflows/deploy.yml`) which builds the site in the `gh-pages` branch,
replaces the frog+racket .gitignore with a racket .gitignore to commit generated files, but not racket bytecode,
and pushes. It also wipes the branch to keep the history small. 

* By default, documentation-style scribble using forms like `@examples` and `@racketgrammar*` don't get formatted nicely by frog.
To get typesetting and highlighting that looks like scribble documentation, I added `css/racket.css` and linked it in `_src/page-template.html`.


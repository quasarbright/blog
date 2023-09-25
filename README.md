# Mike Delmonaco's blog

This is the source code for [my blog](https://quasarbright.github.io/blog/).

This blog is built using [my fork](https://github.com/quasarbright/frog) of [frog](https://github.com/greghendershott/frog).

It is hosted on GitHub Pages. In the master branch, generated files are ignored.
Through GitHub actions (see `.github/workflows/deploy.yml`), frog is invoked to
build the website. Every push to master triggers a build, wipes the `gh-pages` branch,
and pushes the source code and the generated files to the `gh-pages` branch. GitHub pages
serves html from this branch, not master.

## To Build and Serve Locally

Requirements:

* [Racket](https://download.racket-lang.org/) 
* [Python](https://www.python.org/downloads/). `python` must be in the PATH, `python3` won't work.
* [Pygments](https://pygments.org/) (can install with `pip install pygments` once Python is installed)

To build:

```sh
raco pkg install
raco frog -b
```

To serve the blog:

```sh
./preview.sh
```

This will perform live updates when you edit an existing scribble or markdown file, but will not update when you create a new one.

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

* For mathjax, I needed to add some styling to prevent it from being green. Also, when using mathjax in scribble posts,
you only need a single backslash in the math delimiters, like `\[ x^2 \]`. For markdown, you can use `$$ ax + b = 0 $$` for block math, but for inline math you have to do `\\(x\\)`.

* When I was trying to use code blocks in markdown with highlighting, I was getting this error:
```
Pygments pipe.py not responding. Using plain `pre` blocks.
```

When running with the `-V` switch, I got this:

```
Launching `/usr/local/bin/python /Applications/Racket v8.7/share/pkgs/frog/frog/private/enhance-body/syntax-highlight/pipe.py` to use Pygments.
Pygments pipe.py not responding. Using plain `pre` blocks.
```

Since the path for Racket has a space in it, python was trying to execute `/Applications/Racket`!

To fix this, I created my own fork of frog and added quotes around the path that gets used. [fix](https://github.com/quasarbright/frog/commit/5a3dbbc24858f6ac768a7f2ed1f9aa7783ec37ba)

## Weird things to keep in mind

* Editing a post's title may wipe disqus comments since it changes its full-uri, which is used as the identifier for the disqus thread.
TODO make id the file name, not the title

* Images generated automatically are stored in `img/posts`, so they are gitignored. To have a local image, put it in img. It'll
get duplicated into `img/posts`, but that's ok

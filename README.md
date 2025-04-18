# Bord

_[boo-rd]_

Named after the Swedish word for _tables_ (as in, the ones you find at a furniture store).

You can try it out at [bord.beretta.nu](https://bord.beretta.nu).

## Usage

Create a new table from scratch or upload a CSV. Columns can be of type Number, Text or Boolean.

A table can have functions applied to it, of types Map, Reduce and Filter. Each function can only use compatible columns.
Click run to process a function.

Each editor has a setup and a data view.

## Technical description

The project is written in ClojureScript, Reagent and Sass.
It currently runs entirely in client and uses some neat browser features like IndexedDB and web workers.

Data is stored in fragments of 5 kB or so, making them easy to chunk and parallelize.

The result is a fairly lightweight application that only requires a 200 kB initial download.

### Security Concerns

The site makes no external calls, uses a minimal number of dependencies and doesn't implement features that allow for arbitrary code execution.
However, data stored in the browser is unencrypted which makes it vulnerable to malware or access to the computer.

## Development

Requires JDK for ClojureScript and npm for building.

You can modify the Dockerfile for a development setup.

Alternatively it should work with these NixOS packages.

```
pkgs.nodejs_22
pkgs.jdk21_headless
pkgs.clojure
pkgs.clojure-lsp
pkgs.leiningen
```

After running `npm install`, run auto-reloading cljs and sass services with the following

```
npm run app
npm run sass
```

## TODO

Still very much a fun side project in progress.

### Known issues

+ Functions aren't fully tested, there are probably many bugs there
+ Worker tasks don't update the editor
+ UX for the editor buttons is lacking
+ The task summary symbols render incorrectly on some platforms

### Future

+ Export to JSON/CSV
+ Data validation
+ Investigate WASM for some Rust experience
+ Offloading to some external process
+ Optional cloud functionality

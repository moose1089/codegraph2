# codegraph

Generates a dot file based on Clojure/ClojureScript code.

## Installation

`git clone https://gitlab.com/200ok/codegraph.git`

## Usage

If you want to see the dot code...

    $ lein run <path-to-clj-file>

Or pipe directly into dot to get an SVG.

    $ lein run <path-to-clj-file> | dot -Tsvg > codegraph.svg

## Options

None.

## Examples

Spoiler: Running codegraph on [itself](src/codegraph/core.clj) will give you this...

![codegraph graph of codegraph itself](codegraph.png)

### Bugs

`(or :bugfree :plenty)`

## License

Copyright © 2016 phil@200ok.ch

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

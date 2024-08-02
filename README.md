# jdf/comfort

Collection of tiny things which make life easier in Clojure and ClojureScript. API not guaranteed to be stable; you can choose a specific `sha` to depend on.

## Usage
```edn
{:deps {jdf/comfort {:git/url "https://github.com/jdf-id-au/comfort.git"
                     :sha "<insert current commit>"}}}
```

## Functionality

- [`comfort.core`](src/comfort/core.cljc) incl
  - `defre` simplistic regular expression group naming
  - `collate-by` (like `group-by` but less voluminous)
  - `update-if-present`, `assoc-if-absent`, `redact-keys`
  - `tabulate`, `detabulate`
  - `hierarchicalise`
  - `dag`, `deps-order`
  - `with-resources`
- [`comfort.gen`](src/comfort/gen.cljc) clojure.spec conveniences incl
  - `salad` word salad generator
- [`comfort.io`](src/comfort/io.cljc) incl
  - `pprint-with-meta`
  - `safe-spit`
  - `percent-encode`
  - `copy!` and `paste` with system clipboard
- [`comfort.plot`](src/comfort/plot.cljc) data manipulation in the spirit of d3.js, incl polymorphic
  - `(scale [domain-from domain-to] [range-from range-to])`
  - `ticks`, `nice`
- [`comfort.system`](src/comfort/system.cljc) incl
  - `make-help` repl documentation displayer
  - `mem-report`
  - `doseq-timed`
- [`comfort.ui`](src/comfort/ui.cljc) Swing-based REPL-friendly 2d painting incl
  - `repl-frame`
  - `inset`, `align`, `anchor-string`

```clojure
(ns project
  (:require [comfort.core :as cc]
            [comfort.system :as cs]))

(cs/timed (apply + (range 1000)))
;=> [4.12151E-4 499500]
```

Also see [test namespaces](https://github.com/jdf-id-au/comfort/tree/master/test/comfort).
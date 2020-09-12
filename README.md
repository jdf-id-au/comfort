# jdf/comfort

Collection of tiny things which make life easier. Constantly changing api for the time being!

## Usage
```edn
{:deps {jdf/comfort {:git/url "https://github.com/jdf-id-au/comfort.git"
                     :sha "<insert current commit>"}}}
```

```clojure
(ns project
  (:require [comfort.system :as cs]))

(cs/timed (apply + (range 1000)))
;=> [4.12151E-4 499500]
```
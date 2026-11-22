# canvas-protocol

Live HTML canvases for [cl-stack](https://github.com/egao1980/cl-stack) (H4h). A canvas is a **plain CL function** in a session image: keyword arguments (request strings) in, an [html-protocol](https://github.com/egao1980/html-protocol) tree out. htmx attributes point at other canvas functions; SSE (htmx SSE extension) pushes fragments; `freeze` writes one self-contained static `.html`.

| Piece | |
|-------|--|
| Components | stack / grid / card / details / fieldset / tabs / code-block / kv-list / badge / live-region, tables (sort + paging over hx), inputs + forms with `coerce-param` (conditions + restarts) |
| htmx | `hx-get` / `hx-post` plists, `canvas-link` / `canvas-url` through a resolver GF, `htmx-response-headers` as data, `sse-connect` / `sse-swap` / `fragment-event` |
| Output | classless semantic HTML (closed `data-*` vocabulary), CSP-ready for `script-src 'self'` (no inline scripts, no `on*`, no `style=`), `csp-violations` scan |
| Assets | vendored **htmx 2.0.11** + **htmx-ext-sse 2.2.4** (0BSD, checksums in `assets/SHA256SUMS`), placeholder classless stylesheet (engels later) |
| Charts | backend-neutral chart model + `render-chart` GF (SVG backend: [canvas-backend-svg](https://github.com/egao1980/canvas-backend-svg)). Marks are `currentColor` with `data-series="1".."8"`; `chart-stylesheet` / `chart-css` (built with css-protocol) map them to Okabe–Ito `--chart-1..8`, light and dark, and the placeholder theme ends with `(chart-css)` |
| SVG names | camelCase SVG names (`viewBox`, `preserveAspectRatio`, `linearGradient`, ...) keep their case through `el`, `render`, `neutralize` and `freeze` (html-protocol 0.1.2 `canonical-*-name`) |

Servers live elsewhere: [rpc-backend-htmx](https://github.com/egao1980/rpc-backend-htmx) (htmx as an rpc-protocol transport) and the cl-stack-code web view.

```lisp
(defun counter (&key (n "0"))
  (let ((n (canvas:coerce-param n :integer :default 0)))
    (canvas:card (list (canvas:el :p '(:id "count") (princ-to-string n))
                       (canvas:el :button (canvas:hx-get (canvas:canvas-link 'counter :n (1+ n))
                                                         :target "#counter" :swap :outer-html)
                                  "+1"))
                 :id "counter" :title "Counter")))

(canvas:freeze 'counter :args '(:n "41"))   ; => "<!DOCTYPE html>..."
```

Design: [cl-stack/docs/capabilities/canvas.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/canvas.md).

## License

MIT — see [LICENSE](LICENSE). Vendored htmx and htmx-ext-sse are 0BSD (`assets/LICENSE.*`).

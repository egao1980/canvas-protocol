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
| Keys (0.2.0) | `hx-get` / `hx-post` `:keys '("ArrowUp" "Ctrl+z")` bind key chords without eval: `data-canvas-keys` + `hx-trigger="canvas:key"`, served by the same-origin `canvas-keys.js` (`(page body :keys t)`), the chord sent as parameter `key`. `[filter]` triggers stay refused (they need `unsafe-eval`); `:sync` / `hx-sync-value` queue keystrokes in order; `hx-swap-oob` updates several panels from one response |
| Engine SVG (0.2.0) | `svg-fragment` parses third-party SVG text with xml-protocol (no external entities, nothing fetched) into an html-protocol tree through an allowlist: ids, camelCase names, `<use href="#…">` kept; scripts, `on*`, `foreignObject`, external `href`, non-fragment `url(`, htmx attributes signal `unsafe-svg` (restart `strip-violations`); `<style>` is dropped |
| Uploads (0.2.0) | `file-input` (`accept`, `multiple`), forms with a file input get `enctype="multipart/form-data"`, `(coerce-param part :file :max-size n :accept "image/*")` -> `uploaded-file` (filename, media type, size, octets or path) with `canvas-upload-too-large` and the usual `use-value` / `use-default` restarts |
| App assets (0.3.0) | `(define-asset "vendor/kit/kit.wasm" :system "my-app" :file "vendor/kit/kit.wasm" :sha256 "…" :license "Apache-2.0" :license-file "vendor/kit/LICENSE")` (or `register-asset`): an app's own scripts and vendored files, read at load, SHA-256 checked at load (`canvas-asset-checksum-mismatch`), content type from the extension (`.wasm` → `application/wasm`), seen by `asset-names` / `asset-bytes` / `asset-octets` (no copy, for multi-MB files) / `asset-info` / `asset-license` / `resolve-asset-url`. `(page body :scripts '("app/main.js" ("vendor/kit/kit.mjs" :module t)))` adds same-origin `<script src>` tags after the built-ins; `(freeze fn :scripts …)` keeps them as relative `src` (opt-in; frozen pages run no script otherwise). A WASM toolkit also needs `'wasm-unsafe-eval'` in the server's `script-src` |

0.2.0 (HX2: issues #6 and #7): the exact rules are in each `src/*.lisp` header, and the Rove suite under `tests/` checks them.

0.2.1 requires mime-protocol 0.3.1, so an `uploaded-file` filename is the name the browser sent: raw UTF-8 such as `Ünïcödé — 楽譜.svg`, and `C:\fakepath\x.svg` → `x.svg` (mime-protocol#14). It also requires xml-backend-native 0.1.1, where `svg-fragment` signals `xml-well-formed-error` for a character reference to a control character.

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

(defpackage #:canvas-protocol
  (:use #:cl)
  (:nicknames #:stack-canvas #:canvas)
  (:export
   ;; conditions + restarts
   #:canvas-error #:canvas-error-message
   #:canvas-no-resolver
   #:canvas-csp-error #:csp-error-violations
   #:canvas-coercion-error #:coercion-error-field #:coercion-error-value
   #:coercion-error-expected #:coercion-error-detail #:coercion-error-text
   #:canvas-asset-not-found #:asset-error-name
   #:invalid-chart #:invalid-chart-field #:invalid-chart-reason
   #:chart-backend-missing #:chart-backend-missing-chart
   #:use-default #:strip-violations #:use-placeholder
   ;; trees
   #:render #:to-html #:el
   #:component #:component-id #:component-attrs
   ;; layout
   #:stack #:grid #:card #:details #:fieldset #:tabs
   #:code-block #:kv-list #:badge #:live-region
   ;; tables
   #:column #:table-column #:column-header #:column-accessor #:column-name
   #:column-sortable-p #:column-format #:column-align
   #:table #:sort-rows #:page-rows #:parse-sort #:value<
   ;; forms
   #:form #:input-component
   #:text-input #:number-input #:date-input #:checkbox #:select-input
   #:textarea #:submit-button
   #:coerce-param #:coerce-form #:find-field-error #:error-fragment
   #:+coercion-types+
   ;; urls + htmx
   #:*canvas-resolver* #:canvas-resolver #:resolve-canvas-url #:resolve-asset-url
   #:path-resolver #:make-path-resolver #:path-resolver-prefix #:path-resolver-asset-prefix
   #:freeze-resolver #:make-freeze-resolver
   #:canvas-link #:canvas-link-p #:canvas-link-function #:canvas-link-params
   #:canvas-url #:link-url
   #:hx-get #:hx-post #:hx-swap-value #:+htmx-swap-styles+
   #:htmx-response-headers
   ;; sse
   #:sse-connect #:sse-swap #:sse-close #:fragment-event
   ;; page, csp, freeze
   #:page #:+htmx-config+ #:csp-violations #:neutralize #:freeze
   ;; assets
   #:+htmx-version+ #:+htmx-sse-version+
   #:asset-names #:asset-bytes #:asset-content-type #:asset-sha256
   #:asset-info #:asset-license
   #:*canvas-theme* #:theme-stylesheet #:canvas-stylesheet
   ;; charts
   #:chart #:chart-p #:make-chart #:chart-kind #:chart-series #:chart-x-axis
   #:chart-y-axis #:chart-title #:chart-description #:chart-legend
   #:chart-width #:chart-height #:chart-bins
   #:series #:make-series #:series-name #:series-points #:series-color #:series-labels
   #:axis #:make-axis #:axis-label #:axis-scale #:axis-min #:axis-max
   #:axis-ticks #:axis-format
   #:validate-chart #:chart-backend #:*chart-backend* #:render-chart #:chart-node
   #:*chart-palette* #:+chart-kinds+ #:+axis-scales+))

(in-package #:canvas-protocol)

/* canvas-keys.js -- canvas-protocol keyboard bindings. MIT licence.
 *
 * Key filters for htmx without eval. htmx evaluates hx-trigger filters such
 * as keydown[key=='ArrowUp'] with the Function constructor, which the canvas
 * CSP (script-src 'self', htmx allowEval false) forbids; with eval off, htmx
 * 2 treats such a filter as always true. This file is served from 'self'
 * and evaluates nothing.
 *
 * An element with data-canvas-keys="ArrowUp Ctrl+z" receives a non-bubbling
 * "canvas:key" CustomEvent, detail.key = the chord, when that chord is
 * pressed; hx-trigger="canvas:key" turns it into a request, and the chord is
 * sent as the request parameter "key".
 *
 * Chord: [Ctrl+][Alt+][Shift+][Meta+]KEY, KEY = KeyboardEvent.key, with " "
 * named Space and "+" named Plus, and a single letter in lower case. Shift is
 * left out when KEY is a single character other than a letter or a space,
 * because that character already says it ("?", "!", "Plus").
 * Modifier keys alone, dead keys, IME composition and events another handler
 * has already cancelled produce nothing.
 *
 * Focus: when the event target is a form field (input, textarea, select) or
 * contenteditable, only that field itself is matched; otherwise every
 * [data-canvas-keys] element in the document is. When at least one element
 * matches, the keydown's default action is prevented.
 */
(function () {
  "use strict";

  var IGNORED = {
    "Shift": true, "Control": true, "Alt": true, "Meta": true, "AltGraph": true,
    "CapsLock": true, "Dead": true, "Unidentified": true, "Process": true
  };

  function chord(e) {
    var k = e.key;
    if (typeof k !== "string" || k.length === 0 || IGNORED[k] === true) {
      return null;
    }
    var letter = k.length === 1 && k.toLowerCase() !== k.toUpperCase();
    var shiftImplied = k.length === 1 && !letter && k !== " ";
    if (k === " ") {
      k = "Space";
    } else if (k === "+") {
      k = "Plus";
    } else if (letter) {
      k = k.toLowerCase();
    }
    return (e.ctrlKey ? "Ctrl+" : "") +
      (e.altKey ? "Alt+" : "") +
      (e.shiftKey && !shiftImplied ? "Shift+" : "") +
      (e.metaKey ? "Meta+" : "") + k;
  }

  function formField(el) {
    if (!el || el.nodeType !== 1) {
      return false;
    }
    var tag = el.tagName;
    return tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT" ||
      el.isContentEditable === true;
  }

  function bound(el, c) {
    var value = el.getAttribute("data-canvas-keys");
    if (value === null) {
      return false;
    }
    var keys = value.split(/\s+/);
    for (var i = 0; i < keys.length; i++) {
      if (keys[i] === c) {
        return true;
      }
    }
    return false;
  }

  document.addEventListener("keydown", function (e) {
    if (e.defaultPrevented || e.isComposing) {
      return;
    }
    var c = chord(e);
    if (c === null) {
      return;
    }
    var candidates = formField(e.target) ?
      [e.target] :
      Array.prototype.slice.call(document.querySelectorAll("[data-canvas-keys]"));
    var matched = false;
    for (var i = 0; i < candidates.length; i++) {
      if (bound(candidates[i], c)) {
        matched = true;
        candidates[i].dispatchEvent(
          new CustomEvent("canvas:key", { bubbles: false, detail: { key: c } }));
      }
    }
    if (matched) {
      e.preventDefault();
    }
  });

  document.addEventListener("htmx:configRequest", function (e) {
    var t = e.detail && e.detail.triggeringEvent;
    if (t && t.type === "canvas:key" && t.detail && typeof t.detail.key === "string") {
      e.detail.parameters["key"] = t.detail.key;
    }
  });
})();

// The browser's diagnostics: the console sink and the two switches, all of
// which `PUI`'s diagnostics take as parameters — they have no JS of their own, so
// every line of JS in this library lives under the Web layer. Installed at the
// mount entries (see `adoptHostDiagnostics`).

// traceSink :: String -> Logged -> Effect Unit
export const traceSink = (tag) => (value) => () =>
  console.debug("%c[bambik]%c " + tag, "color:#6200ee;font-weight:bold", "color:inherit", value);

// warnSink :: String -> Array String -> Effect Unit
// Beside the message, the failure's field labels arrive as data; the stamp
// invariant makes each label findable in the DOM (`name` on form citizens,
// `aria-label` on quantity displays, `aria-labelledby`-named groups), so the
// warning logs the labels' host elements themselves — clickable in DevTools,
// jumping straight from the starving gate to the place on the page.
export const warnSink = (msg) => (labels) => () => {
  const hosts = labels.flatMap((l) => [
    ...document.getElementsByName(l),
    ...[...document.querySelectorAll("[aria-label]")].filter((e) => e.getAttribute("aria-label") === l),
  ]);
  console.warn("%c[bambik]%c " + msg, "color:#b26a00;font-weight:bold", "color:inherit", ...hosts);
};

// hostTracing :: Effect Boolean
export function hostTracing() {
  if (window.__bambikTrace === true) return true;
  try {
    return localStorage.getItem("bambik-trace") === "true";
  } catch (_) {
    return false; // storage unavailable (file://, sandboxed iframe)
  }
}

// hostDiagnostics :: Effect Boolean
export function hostDiagnostics() {
  return window.__bambikNoWarn !== true;
}

// uniqueId :: Effect String
export function uniqueId() {
  return "" + Math.floor(Math.random() * 99999999 + 100000000); // TODO use UUID?
}

// documentBody :: IOSync Node
export function documentBody() {
  return document.body;
}

// createTextNode :: String -> IOSync Node
export function createTextNode(text) {
  return function () {
    return document.createTextNode(text);
  };
}

// createCommentNode :: String -> IOSync Node
export function createCommentNode(text) {
  return function () {
    return document.createComment(text)
  }
};

// separatorRule :: Node -> Node -> Node -> Effect (Effect Unit) — the rule's
// nodes follow `start` up to `ruleEnd`; the content they introduce follows up
// to `end`. The rule shows while something rendered precedes it and the
// content renders something, re-decided on every change under their parent;
// the returned effect disconnects the observer (run when the scope is
// cancelled).
export function separatorRule(start) {
  return function (ruleEnd) {
    return function (end) {
      return function () {
        const parent = start.parentNode;
        const replaced = /^(input|img|svg|canvas|video|audio|select|textarea|button|iframe|progress|meter|object|embed)$/i;
        const rule = /^(hr|md-divider|sl-divider|fluent-divider)$/i;
        const something = (n) => {
          if (n.nodeType === 3) return n.textContent.trim() !== "";
          if (n.nodeType !== 1) return false;
          if (n.hidden || (n.style && n.style.display === "none")) return false;
          const tag = n.tagName;
          if (rule.test(tag)) return false;
          if (replaced.test(tag) || tag.includes("-")) return true;
          for (let c = n.firstChild; c; c = c.nextSibling) if (something(c)) return true;
          return false;
        };
        const evaluate = () => {
          let before = false;
          for (let n = start.previousSibling; n && !before; n = n.previousSibling) before = something(n);
          let after = false;
          for (let n = ruleEnd.nextSibling; n && n !== end && !after; n = n.nextSibling) after = something(n);
          const show = before && after;
          for (let n = start.nextSibling; n; n = n.nextSibling) {
            if (n.nodeType === 1) n.style.display = show ? "" : "none";
            if (n === ruleEnd) break;
          }
        };
        evaluate();
        const observer = new MutationObserver(evaluate);
        observer.observe(parent, { childList: true, subtree: true, characterData: true });
        return function () {
          observer.disconnect();
        };
      };
    };
  };
}

// revealRule :: Node -> Effect (Effect Unit) — scrolls `container`, and
// nothing else, just far enough to show the element that last entered it
// or the element whose text last changed in it — with as much around it
// as fits: the largest of its ancestors no taller than the container, a
// row for a cell; the returned effect disconnects the observer. A text written again with the value it had
// counts for nothing.
export function revealRule(container) {
  return function () {
    const reveal = (changed) => {
      const box = container.getBoundingClientRect();
      if (box.height === 0) return;
      let el = changed;
      for (let p = el.parentElement; p && p !== container && p.getBoundingClientRect().height <= container.clientHeight; p = p.parentElement) el = p;
      const r = el.getBoundingClientRect();
      if (r.height === 0) return;
      const top = box.top + container.clientTop;
      const bottom = top + container.clientHeight;
      if (r.top < top || r.height > container.clientHeight) container.scrollTop += r.top - top;
      else if (r.bottom > bottom) container.scrollTop += r.bottom - bottom;
    };
    const observer = new MutationObserver((records) => {
      let last = null;
      for (const m of records) {
        if (m.type === "characterData") {
          if (m.oldValue !== m.target.nodeValue && m.target.parentElement) last = m.target.parentElement;
        } else {
          for (const n of m.addedNodes) {
            if (n.nodeType === 1) last = n;
            else if (n.nodeType === 3 && n.nodeValue.trim() !== "" && n.parentElement) last = n.parentElement;
          }
        }
      }
      if (last && last !== container && container.contains(last)) reveal(last);
    });
    observer.observe(container, { childList: true, subtree: true, characterData: true, characterDataOldValue: true });
    return function () {
      observer.disconnect();
    };
  };
}

// listenNodeDeferred :: Node -> String -> Effect Unit -> Effect Unit — the
// handler runs a microtask after the event, for components whose default
// action (a filter chip toggling `selected`) settles after the event
// finishes dispatching
export function listenNodeDeferred(node) {
  return function (eventName) {
    return function (callback) {
      return function () {
        node.addEventListener(eventName, function () {
          queueMicrotask(function () {
            callback();
          });
        });
      };
    };
  };
}

// ensureStyle :: String -> String -> Effect Unit — inject a stylesheet once
// under its id, for the chrome a vocabulary hand-rolls
export function ensureStyle(id) {
  return function (css) {
    return function () {
      if (!document.getElementById(id)) {
        const style = document.createElement("style");
        style.id = id;
        style.textContent = css;
        document.head.appendChild(style);
      }
    };
  };
}

// autoDismiss :: Node -> String -> Int -> Effect Unit — show a transient
// status by class and take the class off after the period, a newer show
// restarting the timer
export function autoDismiss(node) {
  return function (className) {
    return function (millis) {
      return function () {
        node.classList.add(className);
        clearTimeout(node.__bambikDismissTimer);
        node.__bambikDismissTimer = setTimeout(function () {
          node.classList.remove(className);
        }, millis);
      };
    };
  };
}

// invokeMethod :: String -> Node -> Effect Unit — call an element's own
// no-argument method by name (a dialog's show/close, whatever the catalogue
// calls them); a method missing on the node is a no-op, so a call before a
// custom element upgrades does nothing rather than throwing
export function invokeMethod(name) {
  return function (node) {
    return function () {
      if (typeof node[name] === "function") node[name]();
    };
  };
}

// showModalOnce :: Node -> Effect Unit — the native <dialog>'s showModal,
// which throws on a dialog already open
export function showModalOnce(node) {
  return function () {
    if (!node.open) node.showModal();
  };
}

// createDocumentFragment :: IOSync Node
export function createDocumentFragment() {
  return document.createDocumentFragment();
}

// createElement :: TagName -> IOSync Node
export function createElement(tag) {
  return function () {
    return document.createElement(tag);
  };
}

// createElementNS :: Namespace -> TagName -> IOSync Node
export function createElementNS(ns) {
  return function (tag) {
    return function () {
      return document.createElementNS(ns, tag);
    };
  };
}

// namespaceURI :: Node -> IOSync String
export function namespaceURI(node) {
  return function () {
    return node.namespaceURI || "http://www.w3.org/1999/xhtml";
  };
}

// insertBefore :: Node -> Node -> IOSync Unit
export function insertBefore(newNode) {
  return function (existingNode) {
    return function () {
      existingNode.before(newNode);
    };
  };
}

// appendChild :: Node -> Node -> Effect Unit
export function appendChild(newNode) {
  return function (parent) {
    return function () {
      parent.appendChild(newNode);
    };
  };
}

// appendRawHtml :: String -> Node -> IOSync Node
export function appendRawHtml(html) {
  return function (parent) {
    return function () {
      // According to https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentHTML
      // this should work:
      //   parent.insertAdjacentHTML('beforeend', html);
      // But it doesn't, at least in PhantomJS. Hence the following hack:

      // This also should work, but doesn't:
      //   var dummyElement = document.createElement('div');
      //   parent.appendChild(dummyElement);
      //   dummyElement.outerHTML = html;

      var dummyElement = document.createElement("div");
      dummyElement.innerHTML = html;

      var node = dummyElement.firstChild;
      var last = null;
      while (node !== null) {
        var next = node.nextSibling;
        parent.appendChild(node); // moves the node from dummyElement to parent
        last = node;
        node = next;
      }
      return last; // the last node appended (dummyElement is empty by now)
    };
  };
}

// addEventListener :: EventType -> (Event -> IOSync Unit) -> Node -> IOSync (IOSync Unit)
export function addEventListener(eventType) {
  return function (node) {
    return function (handler) {
      return function () {
        var listener = function (event) {
          handler(event)();
        };
        node.addEventListener(eventType, listener);
        return function () {
          node.removeEventListener(eventType, listener);
        };
      };
    };
  };
}



// moveAllNodesBetweenSiblings :: Node -> Node -> Node -> IOSync Unit
export function moveAllNodesBetweenSiblings(from) {
  return function (to) {
    return function (newParent) {
      return function () {
        const parent = from.parentNode;
        var node = from.nextSibling;
        var next = null;
        while (node !== to) {
          next = node.nextSibling;
          newParent.appendChild(parent.removeChild(node));
          node = next;
        }
      };
    };
  };
}

// removeAllNodesBetweenSiblings :: Node -> Node -> IOSync Unit
export function removeAllNodesBetweenSiblings(from) {
  return function (to) {
    return function () {
      const parent = from.parentNode;
      var node = from.nextSibling;
      var next = null;
      while (node !== to) {
        next = node.nextSibling;
        parent.removeChild(node)
        node = next;
      }
    };
  };
}

// isFocused :: Node -> IOSync Boolean
export function isFocused(node) {
  return function () {
    return document.activeElement === node;
  };
}

// getValue :: Node -> IOSync String
export function getValue(node) {
  return function () {
    return node.value;
  };
}

// setValue :: Node -> String -> IOSync String
export function setValue(node) {
  return function (value) {
    return function () {
      node.value = value;
    };
  };
}

// getChecked :: Node -> IOSync Boolean
export function getChecked(node) {
  return function () {
    return node.checked;
  };
}

// setChecked :: Node -> Boolean -> IOSync Unit
export function setChecked(node) {
  return function (value) {
    return function () {
      return (node.checked = value);
    };
  };
}

export function removeAttribute(node) {
  return function (name) {
    return function () {
      node.removeAttribute(name);
    }
  }
}
// setAttribute:: Node -> String -> String -> Effect Unit
export function setAttribute(node) {
  return function (name) {
    return function (value) {
      return function () {
          node.setAttribute(name, value);
      }
    }
  }
}

// addClass :: Node -> String -> Effect Unit
export function addClass(node) {
  return function (name) {
    return function () {
        node.classList.add(name);
    }
  }
}

// removeClass :: Node -> String -> Effect Unit
export function removeClass(node) {
  return function (name) {
    return function () {
        node.classList.remove(name);
    }
  }
}

// setNodeValue :: Node -> String -> Effect Unit
export function setTextNodeValue(node) {
  return function (value) {
    return function () {
      node.nodeValue = value;
    };
  };
}

// lastChild :: Node -> Effect Node
export function lastChild(node) {
  return function () {
    return node.lastChild;
  };
}

// childCount :: Node -> Effect Int
export function childCount(node) {
  return function () {
    return node.childNodes.length;
  };
}

// liveRun :: Array Node -> Effect (Array Node) — the siblings that stand
// now from the first node of a run to its last: the run as it was built
// plus whatever a pane attached between its placeholders since
export function liveRun(nodes) {
  return function () {
    if (nodes.length === 0) return nodes;
    const last = nodes[nodes.length - 1];
    const run = [];
    for (let node = nodes[0]; node; node = node.nextSibling) {
      run.push(node);
      if (node === last) return run;
    }
    return nodes;
  };
}

// childrenFrom :: Int -> Node -> Effect (Array Node) — the children from
// the given index on, in document order
export function childrenFrom(index) {
  return function (node) {
    return function () {
      return Array.prototype.slice.call(node.childNodes, index);
    };
  };
}

// onInputDebounced :: Node -> Number -> (String -> Effect Unit) -> Effect Unit
export function onInputDebounced(node) {
  return function (millis) {
    return function (callback) {
      return function () {
        let timer;
        node.addEventListener("input", function () {
          clearTimeout(timer);
          timer = setTimeout(function () {
            callback(node.value)();
          }, millis);
        });
      };
    };
  };
}

export function removeAllChildren(node) {
  return function () {
    while (node.firstChild) node.removeChild(node.firstChild);
  };
}

// removeChild :: Node -> Node -> Effect Unit  (removeChild child parent)
export function removeChild(child) {
  return function (parent) {
    return function () {
      if (child.parentNode === parent) parent.removeChild(child);
    };
  };
}

// onPointerXY :: String -> Node -> (Number -> Number -> Effect Unit) -> Effect Unit
// One pointer event type on the node, its coordinates mapped from CSS pixels
// into the SVG's viewBox space when the node has one, so a responsive canvas
// stays accurate on any screen width. pointerdown works for mouse, touch and
// pen alike and reports the primary button only; contextmenu (a right click,
// a long press) keeps the browser's own menu away; pointermove reports every
// move and the point where the pointer leaves.
export function onPointerXY(type) {
  return function (node) {
    return function (callback) {
      return function () {
        const report = function (event) {
          if (type === "pointerdown" && event.button !== 0) return;
          if (type !== "pointermove") event.preventDefault();
          const rect = node.getBoundingClientRect();
          const vb = node.viewBox && node.viewBox.baseVal;
          const x = vb ? (event.clientX - rect.left) * (vb.width / rect.width) : event.clientX - rect.left;
          const y = vb ? (event.clientY - rect.top) * (vb.height / rect.height) : event.clientY - rect.top;
          callback(x)(y)();
        };
        node.addEventListener(type, report);
        if (type === "pointermove") node.addEventListener("pointerleave", report);
      };
    };
  };
}


// onDeparture :: Node -> Effect Unit -> Effect Unit
// The focus leaving the container — a click elsewhere, Tab, or Enter, which
// blurs the element it was pressed in.
export function onDeparture(node) {
  return function (callback) {
    return function () {
      node.addEventListener("keydown", function (event) {
        if (event.key === "Enter" && event.target.blur) {
          event.preventDefault();
          event.target.blur();
        }
      });
      node.addEventListener("focusout", function (event) {
        if (!node.contains(event.relatedTarget)) callback();
      });
    };
  };
}

// focusWithin :: Node -> Effect Unit — once the current task is done (the
// container may just have been attached), focus its first element, the caret
// at the end, unless the focus is already inside
export function focusWithin(node) {
  return function () {
    setTimeout(function () {
      if (!node.isConnected || node.contains(document.activeElement)) return;
      const target = node.firstElementChild;
      if (!target || !target.focus) return;
      target.focus();
      if (target.setSelectionRange && typeof target.value === "string") {
        target.setSelectionRange(target.value.length, target.value.length);
      }
    }, 0);
  };
}

// afterTask :: Effect Unit -> Effect Unit — run once the current task (and
// the microtasks custom elements defer their own click handling to) is done
export function afterTask(action) {
  return function () {
    setTimeout(action, 0);
  };
}

// Facts of the host, read beside the model (`reading` in PUI): each is
// what it is now, read on demand, and a watch that reports a change and
// returns the effect that ends it.

// Where the device is: the platform only tells by watching, so one watch
// is shared by every reader and its last answer is kept for the reads.
const position = { fix: null, denied: !(typeof navigator !== "undefined" && navigator.geolocation), subscribers: new Set(), id: null };

// positionNow :: ({ lat, lng } -> a) -> a -> a -> Effect a
export function positionNow(located) {
  return function (unknown) {
    return function (denied) {
      return function () {
        return position.fix ? located(position.fix) : position.denied ? denied : unknown;
      };
    };
  };
}

// positionChanges :: Effect Unit -> Effect (Effect Unit)
export function positionChanges(tell) {
  return function () {
    if (!navigator.geolocation) return function () {};
    const notify = () => { for (const s of [...position.subscribers]) s(); };
    position.subscribers.add(tell);
    if (position.id === null) {
      position.id = navigator.geolocation.watchPosition(
        (pos) => { position.fix = { lat: pos.coords.latitude, lng: pos.coords.longitude }; position.denied = false; notify(); },
        () => { position.fix = null; position.denied = true; notify(); },
        { enableHighAccuracy: false, maximumAge: 60000 });
    }
    return function () {
      position.subscribers.delete(tell);
      if (position.subscribers.size === 0 && position.id !== null) {
        navigator.geolocation.clearWatch(position.id);
        position.id = null;
      }
    };
  };
}

const listening = (target, types, tell) => {
  const handler = () => tell();
  for (const t of types) target.addEventListener(t, handler);
  return function () { for (const t of types) target.removeEventListener(t, handler); };
};

export function isOnline() { return navigator.onLine; }

// onlineChanges :: Effect Unit -> Effect (Effect Unit)
export function onlineChanges(tell) {
  return function () { return listening(window, ["online", "offline"], tell); };
}

// mediaMatches :: String -> Effect Boolean
export function mediaMatches(query) {
  return function () { return !!(window.matchMedia && window.matchMedia(query).matches); };
}

// mediaChanges :: Array String -> Effect Unit -> Effect (Effect Unit)
export function mediaChanges(queries) {
  return function (tell) {
    return function () {
      if (!window.matchMedia) return function () {};
      const stops = queries.map(q => listening(window.matchMedia(q), ["change"], tell));
      return function () { stops.forEach(stop => stop()); };
    };
  };
}

export function viewportNow() {
  return { width: window.innerWidth, height: window.innerHeight };
}

// viewportChanges :: Effect Unit -> Effect (Effect Unit)
export function viewportChanges(tell) {
  return function () { return listening(window, ["resize"], tell); };
}

export function languageNow() { return navigator.language || ""; }

// languageChanges :: Effect Unit -> Effect (Effect Unit)
export function languageChanges(tell) {
  return function () { return listening(window, ["languagechange"], tell); };
}

export function timeZoneNow() {
  return Intl.DateTimeFormat().resolvedOptions().timeZone || "";
}

export function localTime() {
  const d = new Date();
  return { hours: d.getHours(), minutes: d.getMinutes(), seconds: d.getSeconds() };
}

export function timeRun() {
  return { ms: performance.now() };
}

// everyMs :: Number -> Effect Unit -> Effect (Effect Unit)
// One timer per period, shared by every subscriber, its beats on whole
// periods of the wall clock: a beat that took time is followed sooner.
const beats = new Map();

export function everyMs(ms) {
  return function (action) {
    return function () {
      let beat = beats.get(ms);
      if (!beat) {
        beat = { subscribers: new Set(), timer: null };
        beats.set(ms, beat);
        const arm = () => {
          const left = ms - (Date.now() % ms);
          beat.timer = setTimeout(() => {
            arm();
            for (const subscriber of [...beat.subscribers]) subscriber();
          }, left < ms / 20 ? left + ms : left);
        };
        arm();
      }
      beat.subscribers.add(action);
      return function () {
        beat.subscribers.delete(action);
        if (beat.subscribers.size === 0 && beats.get(ms) === beat) {
          clearTimeout(beat.timer);
          beats.delete(ms);
        }
      };
    };
  };
}

// onFilePicked :: Node -> ({ name, size, mime } -> Effect Unit) -> Effect (Effect Unit)
export function onFilePicked(node) {
  return function (tell) {
    return function () {
      const handler = () => {
        const f = node.files && node.files[0];
        if (f) tell({ name: f.name, size: f.size, mime: f.type })();
      };
      node.addEventListener("change", handler);
      return function () { node.removeEventListener("change", handler); };
    };
  };
}

// hasClass :: String -> Node -> Effect Boolean
export const hasClass = className => node => () => node.classList.contains(className);

// listenCapture :: Node -> String -> Effect Unit -> Effect Unit — the
// handler runs in the event's capture phase, before the target's own
export const listenCapture = node => eventName => action => () => {
  node.addEventListener(eventName, () => action(), { capture: true });
};

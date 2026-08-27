// The browser's diagnostics: the console sink and the two switches, all of
// which `PUI`'s diagnostics take as parameters — they have no JS of their own, so
// every line of JS in this library lives under the Web layer. Installed at the
// mount entries (see `adoptHostDiagnostics`).

// traceSink :: String -> Logged -> Effect Unit
export const traceSink = (tag) => (value) => () =>
  console.debug("%c[bambik]%c " + tag, "color:#6200ee;font-weight:bold", "color:inherit", value);

// warnSink :: String -> Effect Unit
export const warnSink = (msg) => () =>
  console.warn("%c[bambik]%c " + msg, "color:#b26a00;font-weight:bold", "color:inherit");

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

// randomElementId :: Effect String
export function randomElementId() {
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

// onClickXY :: Node -> (Number -> Number -> Effect Unit) -> Effect Unit
// pointerdown works for mouse, touch and pen alike; coordinates are mapped
// from CSS pixels into the SVG's viewBox space when the node has one, so a
// responsive canvas stays accurate on any screen width.
export function onClickXY(node) {
  return function (callback) {
    return function () {
      node.addEventListener("pointerdown", function (event) {
        event.preventDefault();
        const rect = node.getBoundingClientRect();
        const vb = node.viewBox && node.viewBox.baseVal;
        const x = vb ? (event.clientX - rect.left) * (vb.width / rect.width) : event.clientX - rect.left;
        const y = vb ? (event.clientY - rect.top) * (vb.height / rect.height) : event.clientY - rect.top;
        callback(x)(y)();
      });
    };
  };
}


// randomElementId :: Effect String
export function randomElementId() {
  return "" + Math.floor(Math.random() * 99999999 + 100000000); // TODO use UUID?
}

// from former DOM.js

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

// insertBefore :: Node -> Node -> IOSync Unit
export function insertBefore(newNode) {
  return function (existingNode) {
    return function () {
      existingNode.before(newNode);
    };
  };
}

// insertAsFirstChild :: Node -> Node -> IOSync Unit
export function insertAsFirstChild(newNode) {
  return function (parentNode) {
    return function () {
      parentNode.insertBefore(newNode, parentNode.firstChild);
    };
  };
}

// appendChild :: Node -> Node -> IOSync Unit
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
      while (node !== null) {
        var next = node.nextSibling;
        parent.appendChild(node); // moves the node from dummyElement to parent
        node = next;
      }
      return dummyElement.lastChild;
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

// setAttributes :: Node -> Object String -> Effect Unit
export function setAttributes(node) {
  return function (attrs) {
    return function () {
      for (var k in attrs) {
        if (attrs.hasOwnProperty(k)) {
          node.setAttribute(k, attrs[k]);
        }
      }
    }
  }
}

// removeAttribute :: Node -> String -> Effect Unit
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

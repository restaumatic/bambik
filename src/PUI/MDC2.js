// MDC Web foundation classes used by PUI.MDC2 — the aggregated
// material-components-web bundle exposes one namespace per component
// (`material.textField.MDCTextField`, ...); a component leaf constructs a
// foundation over its documented markup and drives it through its
// documented properties (the generic *Prop helpers below) and methods.
import material_ from "material-components-web/dist/material-components-web.min.js";

export const material = material_;

// newComponent :: ComponentClass -> Node -> Effect Component
export function newComponent(cls) {
  return function (node) {
    return function () {
      return new cls(node);
    };
  };
}

// open :: Component -> Effect Unit (dialog, snackbar, banner, progress)
export function open(component) {
  return function () {
    component.open();
  };
}

// close :: Component -> Effect Unit
export function close(component) {
  return function () {
    component.close();
  };
}

// closeBanner :: Component -> Effect Unit (MDCBanner.close requires a CloseReason)
export function closeBanner(component) {
  return function () {
    if (component.isOpen) {
      component.close(material_.banner.CloseReason.UNSPECIFIED);
    }
  };
}

// layoutComponent :: Component -> Effect Unit (re-measure after style/content changes)
export function layoutComponent(component) {
  return function () {
    component.layout();
  };
}

// activateTab :: Component -> Int -> Effect Unit (MDCTabBar)
export function activateTab(component) {
  return function (index) {
    return function () {
      component.activateTab(index);
    };
  };
}

// onTabBarActivated :: Component -> (Int -> Effect Unit) -> Effect Unit (MDCTabBar)
export function onTabBarActivated(component) {
  return function (handler) {
    return function () {
      component.listen('MDCTabBar:activated', function (e) { handler(e.detail.index)(); });
    };
  };
}

// MDCSlider's value API is method-based, the one foundation off the
// property convention
export function getSliderValue(component) {
  return function () {
    return component.getValue();
  };
}

export function setSliderValue(component) {
  return function (value) {
    return function () {
      component.setValue(value);
    };
  };
}

// setFormFieldInput :: Component -> Component -> Effect Unit (MDCFormField.input, wires label-click ripple)
export function setFormFieldInput(formField) {
  return function (input) {
    return function () {
      formField.input = input;
    };
  };
}

// property access — MDC foundations expose their model as component
// properties (value, selected, selectedIndex, on, open, progress, ...)

export function setStringProp(name) {
  return function (component) {
    return function (value) {
      return function () {
        component[name] = value;
      };
    };
  };
}

export function setNumberProp(name) {
  return function (component) {
    return function (value) {
      return function () {
        component[name] = value;
      };
    };
  };
}

export function setIntProp(name) {
  return function (component) {
    return function (value) {
      return function () {
        component[name] = value;
      };
    };
  };
}

export function getIntProp(name) {
  return function (component) {
    return function () {
      return component[name] | 0;
    };
  };
}

export function setBoolProp(name) {
  return function (component) {
    return function (value) {
      return function () {
        component[name] = value;
      };
    };
  };
}

export function getBoolProp(name) {
  return function (component) {
    return function () {
      return !!component[name];
    };
  };
}

// listen :: Component -> String -> Effect Unit -> Effect Unit (foundation events)
export function listen(component) {
  return function (eventType) {
    return function (handler) {
      return function () {
        component.listen(eventType, function () { handler(); });
      };
    };
  };
}

// listenNode :: Node -> String -> Effect Unit -> Effect Unit
export function listenNode(node) {
  return function (eventType) {
    return function (handler) {
      return function () {
        node.addEventListener(eventType, function () { handler(); });
      };
    };
  };
}

// setClassIf :: Node -> String -> Boolean -> Effect Unit
export function setClassIf(node) {
  return function (name) {
    return function (condition) {
      return function () {
        node.classList.toggle(name, condition);
      };
    };
  };
}

// querySelectorIn :: Node -> String -> Effect Node
export function querySelectorIn(node) {
  return function (selector) {
    return function () {
      return node.querySelector(selector);
    };
  };
}

// fixListTabIndexes :: Node -> Effect Unit — MD2 list roving-tabindex baseline
export function fixListTabIndexes(node) {
  return function () {
    var items = node.querySelectorAll('li');
    items.forEach(function (li, i) { li.setAttribute('tabindex', i === 0 ? '0' : '-1'); });
  };
}

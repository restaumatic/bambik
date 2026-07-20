import material_ from "material-components-web/dist/material-components-web.min.js";
import { MDCTextFieldHelperText } from '@material/textfield/helper-text';

export const material = material_;

export function mdcTextFieldHelperText(node) {
  return function () {
    const comp = new MDCTextFieldHelperText(node);
    comp.getDefaultFoundation().setValidation(true);
    return comp;
  }
}

export function useNativeValidation(comp) {
  return function (value) {
    return function () {
      comp.useNativeValidation = value;
    }
  }
}

// newComponent :: ComponentClass -> Node -> Effect Component
export function newComponent(cls) {
  return function (node) {
    return function () {
      return new cls(node);
    }
  }
}

// open :: Component -> Effect Unit
export function open(mdcDialog) {
  return function () {
    mdcDialog.open();
  }
}

// close :: Component -> Effect Unit
export function close(component) {
  return function () {
    component.close();
  };
}

// setProgress :: Component -> Number -> Effect Unit
export function setProgress(component) {
  return function (value) {
    return function () {
      component.progress = value;
    };
  };
}

// setDeterminate :: Component -> Boolean -> Effect Unit
export function setDeterminate(component) {
  return function (determinate) {
    return function () {
      component.determinate = determinate;
    }
  };
}

// listen :: Component -> String -> Effect Unit -> Effect Unit
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

// getSliderValue :: Component -> Effect Number
export function getSliderValue(component) {
  return function () {
    return component.getValue();
  };
}

// setSliderValue :: Component -> Number -> Effect Unit
export function setSliderValue(component) {
  return function (value) {
    return function () {
      component.setValue(value);
    };
  };
}

// layout :: Component -> Effect Unit
export function layout(component) {
  return function () {
    component.layout();
  };
}

// getSelected :: Component -> Effect Boolean (MDCSwitch)
export function getSelected(component) {
  return function () {
    return component.selected;
  };
}

// setSelected :: Component -> Boolean -> Effect Unit (MDCSwitch)
export function setSelected(component) {
  return function (selected) {
    return function () {
      component.selected = selected;
    };
  };
}

// getSelectedIndex :: Component -> Effect Int (MDCSelect)
export function getSelectedIndex(component) {
  return function () {
    return component.selectedIndex;
  };
}

// setSelectedIndex :: Component -> Int -> Effect Unit (MDCSelect)
export function setSelectedIndex(component) {
  return function (index) {
    return function () {
      component.selectedIndex = index;
    };
  };
}

// getIconToggleOn :: Component -> Effect Boolean (MDCIconButtonToggle)
export function getIconToggleOn(component) {
  return function () {
    return component.on;
  };
}

// setIconToggleOn :: Component -> Boolean -> Effect Unit (MDCIconButtonToggle)
export function setIconToggleOn(component) {
  return function (on) {
    return function () {
      component.on = on;
    };
  };
}

// setMenuOpen :: Component -> Boolean -> Effect Unit (MDCMenu)
export function setMenuOpen(component) {
  return function (open) {
    return function () {
      component.open = open;
    };
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

// setFormFieldInput :: Component -> Component -> Effect Unit (MDCFormField.input, wires label-click ripple)
export function setFormFieldInput(formField) {
  return function (input) {
    return function () {
      formField.input = input;
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

// layoutComponent :: Component -> Effect Unit
export function layoutComponent(component) {
  return function () {
    component.layout();
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

// querySelectorIn :: Node -> String -> Effect Node
export function querySelectorIn(node) {
  return function (selector) {
    return function () {
      return node.querySelector(selector);
    };
  };
}

// setNodeChecked :: Node -> Boolean -> Effect Unit
export function setNodeChecked(node) {
  return function (checked) {
    return function () {
      node.checked = checked;
    };
  };
}

// @material/web custom-element definitions used by PUI.Web.MDC3 — importing a
// component module registers its <md-*> tag, so `element "md-..."` upgrades.
import '@material/web/button/elevated-button.js';
import '@material/web/button/filled-button.js';
import '@material/web/button/filled-tonal-button.js';
import '@material/web/button/outlined-button.js';
import '@material/web/button/text-button.js';
import '@material/web/checkbox/checkbox.js';
import '@material/web/chips/chip-set.js';
import '@material/web/chips/filter-chip.js';
import '@material/web/dialog/dialog.js';
import '@material/web/divider/divider.js';
import '@material/web/fab/fab.js';
import '@material/web/icon/icon.js';
import '@material/web/iconbutton/icon-button.js';
import '@material/web/list/list.js';
import '@material/web/list/list-item.js';
import '@material/web/menu/menu.js';
import '@material/web/menu/menu-item.js';
import '@material/web/progress/circular-progress.js';
import '@material/web/progress/linear-progress.js';
import '@material/web/radio/radio.js';
import '@material/web/select/filled-select.js';
import '@material/web/select/select-option.js';
import '@material/web/slider/slider.js';
import '@material/web/switch/switch.js';
import '@material/web/tabs/primary-tab.js';
import '@material/web/tabs/tabs.js';
import '@material/web/textfield/filled-text-field.js';
import '@material/web/textfield/outlined-text-field.js';
import { styles as typescaleStyles } from '@material/web/typography/md-typescale-styles.js';

// the .md-typescale-* classes the typography oculars decorate with — adopted
// by `body` at mount (once), not at import: a vocabulary acts on the page only
// when the app enters it
export const adoptTypescale = () => {
  if (typescaleStyles.styleSheet && !document.adoptedStyleSheets.includes(typescaleStyles.styleSheet)) {
    document.adoptedStyleSheets = [...document.adoptedStyleSheets, typescaleStyles.styleSheet];
  }
};

// property access — @material/web components expose their model as element
// properties (value, checked, selected, selectedIndex, activeTabIndex, ...)

export function setNumberProp(name) {
  return function (node) {
    return function (value) {
      return function () {
        node[name] = value;
      };
    };
  };
}

export function setStringProp(name) {
  return function (node) {
    return function (value) {
      return function () {
        node[name] = value;
      };
    };
  };
}

export function getNumberProp(name) {
  return function (node) {
    return function () {
      return Number(node[name]);
    };
  };
}

export function setIntProp(name) {
  return function (node) {
    return function (value) {
      return function () {
        node[name] = value;
      };
    };
  };
}

export function getIntProp(name) {
  return function (node) {
    return function () {
      return node[name] | 0;
    };
  };
}

export function setBoolProp(name) {
  return function (node) {
    return function (value) {
      return function () {
        node[name] = value;
      };
    };
  };
}

export function getBoolProp(name) {
  return function (node) {
    return function () {
      return !!node[name];
    };
  };
}

export function setClassIf(node) {
  return function (className) {
    return function (on) {
      return function () {
        node.classList.toggle(className, on);
      };
    };
  };
}

export function showDialog(node) {
  return function () {
    node.show();
  };
}

export function closeDialog(node) {
  return function () {
    node.close();
  };
}

export function openMenuAnchoredTo(menu) {
  return function (anchor) {
    return function () {
      menu.anchorElement = anchor;
      menu.open = true;
    };
  };
}


export const eachMatching = selector => container => f => () => {
  for (const n of container.querySelectorAll(selector)) f(n)();
};

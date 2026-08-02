// Fluent UI (Fluent 2) custom-element definitions used by PUI.Web.Fluent —
// importing a component's define module registers its <fluent-*> tag, so
// `element "fluent-..."` upgrades. The design tokens (--colorNeutral*,
// --fontFamilyBase, ...) are set globally from the official web light theme,
// so pages need no stylesheet link — the whole design system ships in the
// bundle.
import '@fluentui/web-components/button.js';
import '@fluentui/web-components/divider.js';
import '@fluentui/web-components/dropdown.js';
import '@fluentui/web-components/field.js';
import '@fluentui/web-components/label.js';
import '@fluentui/web-components/listbox.js';
import '@fluentui/web-components/message-bar.js';
import '@fluentui/web-components/option.js';
import '@fluentui/web-components/progress-bar.js';
import '@fluentui/web-components/radio.js';
import '@fluentui/web-components/radio-group.js';
import '@fluentui/web-components/rating-display.js';
import '@fluentui/web-components/slider.js';
import '@fluentui/web-components/switch.js';
import '@fluentui/web-components/text.js';
import '@fluentui/web-components/text-input.js';
import { setTheme } from '@fluentui/web-components/theme/set-theme.js';
import { webLightTheme } from '@fluentui/tokens';

setTheme(webLightTheme);

// property access — Fluent components expose their model as element
// properties (value, valueAsNumber, checked, ...)

export function setNumberProp(name) {
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

export function getStringProp(name) {
  return function (node) {
    return function () {
      return String(node[name] ?? '');
    };
  };
}

// Fluent binds a beat after DOM insertion, and property writes landing
// before that are replayed by FAST at bind — where setters touching
// bind-time internals (the dropdown's listbox and control, the radio
// group's radio collection) break. Writes therefore wait for the
// controller; post-bind they run synchronously, so live feeds pay nothing.
// The wait polls a timer, not requestAnimationFrame: FAST's own update
// queue is rAF-driven and starves in frameless (headless) sessions, so the
// helpers below also finish the two starvable registrations themselves.
function whenBoundDo(node, apply) {
  const ready = function () {
    return node.$fastController && node.$fastController.isConnected;
  };
  if (ready()) {
    apply();
    return;
  }
  const retry = function () {
    if (ready()) apply();
    else setTimeout(retry, 30);
  };
  setTimeout(retry, 0);
}

// selectDropdownOption :: Node -> String -> Effect Unit — the dropdown's
// `value` setter routes through `selectOption`, which needs the slotted
// listbox and the control that `insertControl` enqueues at connect; if the
// rAF-driven queue hasn't delivered the control yet, insert it here
export function selectDropdownOption(node) {
  return function (value) {
    return function () {
      whenBoundDo(node, function () {
        if (!node.control) {
          node.insertControl();
        }
        node.value = value;
      });
    };
  };
}

// selectGroupValue :: Node -> String -> Effect Unit — the radio group's
// `value` setter selects among its registered radios; registration rides
// the same starvable queue, so it is completed here when missing
export function selectGroupValue(node) {
  return function (value) {
    return function () {
      whenBoundDo(node, function () {
        if (!(node.radios && node.radios.length)) {
          node.radios = Array.from(node.querySelectorAll('*')).filter(function (x) {
            return x.tagName.toLowerCase().endsWith('-radio');
          });
        }
        node.value = value;
      });
    };
  };
}

// listenNode :: Node -> String -> Effect Unit -> Effect Unit
export function listenNode(node) {
  return function (eventName) {
    return function (callback) {
      return function () {
        node.addEventListener(eventName, function () {
          callback();
        });
      };
    };
  };
}

// containsFocus :: Node -> Effect Boolean — Fluent's text input keeps the
// real <input> in the light DOM (for ARIA), so the focused element is a
// child of the host, not the host itself
export function containsFocus(node) {
  return function () {
    return node.contains(document.activeElement);
  };
}

// inject a stylesheet once per id — the hand-rolled chrome (card, toast
// placement) carries its CSS here instead of a page link
export function ensureStyle(id) {
  return function (css) {
    return function () {
      if (!document.getElementById(id)) {
        const style = document.createElement('style');
        style.id = id;
        style.textContent = css;
        document.head.appendChild(style);
      }
    };
  };
}

// add the class now, remove it after millis; re-feeding resets the timer
export function autoDismiss(node) {
  return function (className) {
    return function (millis) {
      return function () {
        node.classList.add(className);
        clearTimeout(node.__fluentDismissTimer);
        node.__fluentDismissTimer = setTimeout(function () {
          node.classList.remove(className);
        }, millis);
      };
    };
  };
}

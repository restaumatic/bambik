// Shoelace (Web Awesome) custom-element definitions used by PUI.Shoelace —
// importing a component module registers its <sl-*> tag, so `element "sl-..."`
// upgrades. The base path points default-library icon fetches (sl-rating's
// stars, alert icons) at the matching CDN release; the theme stylesheet
// (themes/light.css) is a page requirement, linked from the same release.
import { setBasePath } from '@shoelace-style/shoelace/dist/utilities/base-path.js';
import '@shoelace-style/shoelace/dist/components/alert/alert.js';
import '@shoelace-style/shoelace/dist/components/button/button.js';
import '@shoelace-style/shoelace/dist/components/card/card.js';
import '@shoelace-style/shoelace/dist/components/divider/divider.js';
import '@shoelace-style/shoelace/dist/components/icon/icon.js';
import '@shoelace-style/shoelace/dist/components/input/input.js';
import '@shoelace-style/shoelace/dist/components/option/option.js';
import '@shoelace-style/shoelace/dist/components/rating/rating.js';
import '@shoelace-style/shoelace/dist/components/select/select.js';
import '@shoelace-style/shoelace/dist/components/switch/switch.js';
import '@shoelace-style/shoelace/dist/components/textarea/textarea.js';

setBasePath('https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.20.1/cdn/');

// property access — Shoelace components expose their model as element
// properties (value, checked, open, ...)

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

// showAlert :: Node -> Effect Unit — (re)open an <sl-alert>; its own
// `duration` closes it again, and reopening restarts that timer
export function showAlert(node) {
  return function () {
    node.show();
  };
}

import material_ from "material-components-web/dist/material-components-web.min.js";

export const material = material_;

export function _new(cls, node) {
  return new cls(node);
}

// open :: Component -> IOSync Unit
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

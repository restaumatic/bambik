import material_ from "material-components-web/dist/material-components-web.min.js";

export const material = material_;

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

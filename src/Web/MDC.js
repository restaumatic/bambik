import material_ from "material-components-web/dist/material-components-web.js";

export const material = material_;

export function _new(cls, node) {
  return new cls(node);
}

// openDialog :: Component -> IOSync Unit
export function openDialog(mdcDialog) {
  return function () {
    mdcDialog.open();
    mdcDialog.listen('MDCDialog:cancel', () => {
      console.log('closed')
    });
  }
}


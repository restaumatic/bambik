import material_ from "material-components-web/dist/material-components-web.min.js";
import { MDCTextFieldHelperText } from '@material/textfield/helper-text';

export const material = material_;

// :: Node -> Effect Component
export function mdcTextFieldHelperText(node) {
  return function () {
    const comp = new MDCTextFieldHelperText(node);
    // comp.getDefaultFoundation().setPersistent(true);
    comp.getDefaultFoundation().setValidation(true);
    // comp.getDefaultFoundation().setContent("abc");
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

export function setValid(comp) {
  return function (valid) {
    return function () {
      comp.valid = valid;
    }
  }
}

export function setContent(comp) {
  return function (content) {
    return function () {
      console.log(content);
      console.log(comp);
      comp.helperTextContent = content;
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

// setDeterminate :: Component -> Boolean -> Effect Unit
export function setDeterminate(component) {
  return function (determinate) {
    return function () {
      component.determinate = determinate;
    }
  };
}

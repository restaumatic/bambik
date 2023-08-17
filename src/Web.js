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

// setAttributes :: EffectFn2 Node (Object String) Unit
export function setAttributes(node, attrs) {
  for (var k in attrs) {
    if (attrs.hasOwnProperty(k)) {
      node.setAttribute(k, attrs[k]);
    }
  }
}


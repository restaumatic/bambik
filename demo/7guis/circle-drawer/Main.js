// setInnerHTML :: Node -> String -> Effect Unit
export function setInnerHTML(node) {
  return function (html) {
    return function () {
      node.innerHTML = html;
    };
  };
}

// onCanvasClick :: Node -> (Number -> Number -> Effect Unit) -> Effect Unit
// pointerdown works for mouse, touch and pen alike; coordinates are mapped
// from CSS pixels into the SVG's viewBox space, so the responsive canvas
// stays accurate on any screen width.
export function onCanvasClick(node) {
  return function (callback) {
    return function () {
      node.addEventListener("pointerdown", function (event) {
        event.preventDefault();
        const rect = node.getBoundingClientRect();
        const vb = node.viewBox.baseVal;
        const x = (event.clientX - rect.left) * (vb.width / rect.width);
        const y = (event.clientY - rect.top) * (vb.height / rect.height);
        callback(x)(y)();
      });
    };
  };
}

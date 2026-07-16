// setInnerHTML :: Node -> String -> Effect Unit
export function setInnerHTML(node) {
  return function (html) {
    return function () {
      node.innerHTML = html;
    };
  };
}

// onCanvasClick :: Node -> (Number -> Number -> Effect Unit) -> Effect Unit
export function onCanvasClick(node) {
  return function (callback) {
    return function () {
      node.addEventListener("click", function (event) {
        const rect = node.getBoundingClientRect();
        callback(event.clientX - rect.left)(event.clientY - rect.top)();
      });
    };
  };
}

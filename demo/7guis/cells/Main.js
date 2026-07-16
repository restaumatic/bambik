// setInnerHTML :: Node -> String -> Effect Unit
export function setInnerHTML(node) {
  return function (html) {
    return function () {
      node.innerHTML = html;
    };
  };
}

// onCellClick :: Node -> (String -> Effect Unit) -> Effect Unit
export function onCellClick(node) {
  return function (callback) {
    return function () {
      node.addEventListener("click", function (event) {
        const td = event.target.closest("td[data-key]");
        if (td) callback(td.dataset.key)();
      });
    };
  };
}

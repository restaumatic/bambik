// setInnerHTML :: Node -> String -> Effect Unit
export function setInnerHTML(node) {
  return function (html) {
    return function () {
      node.innerHTML = html;
    };
  };
}

// onEntryClick :: Node -> (Int -> Effect Unit) -> Effect Unit
export function onEntryClick(node) {
  return function (callback) {
    return function () {
      node.addEventListener("click", function (event) {
        const li = event.target.closest("li[data-key]");
        if (li) callback(parseInt(li.dataset.key, 10))();
      });
    };
  };
}

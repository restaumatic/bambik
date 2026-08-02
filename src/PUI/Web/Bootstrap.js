// PUI.Web.Bootstrap is CSS-only — the design system is the Bootstrap stylesheet
// (a page requirement) over native elements, so there is no component
// JavaScript to import. The one piece of behavior Bootstrap normally gets
// from its JS plugin is the toast's timed dismissal, hand-wired here.

// add the class now, remove it after millis; re-feeding resets the timer
export function autoDismiss(node) {
  return function (className) {
    return function (millis) {
      return function () {
        node.classList.add(className);
        clearTimeout(node.__bsDismissTimer);
        node.__bsDismissTimer = setTimeout(function () {
          node.classList.remove(className);
        }, millis);
      };
    };
  };
}

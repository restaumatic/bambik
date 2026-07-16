(() => {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __commonJS = (cb, mod2) => function __require() {
    return mod2 || (0, cb[__getOwnPropNames(cb)[0]])((mod2 = { exports: {} }).exports, mod2), mod2.exports;
  };
  var __copyProps = (to, from2, except, desc) => {
    if (from2 && typeof from2 === "object" || typeof from2 === "function") {
      for (let key of __getOwnPropNames(from2))
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: () => from2[key], enumerable: !(desc = __getOwnPropDesc(from2, key)) || desc.enumerable });
    }
    return to;
  };
  var __toESM = (mod2, isNodeMode, target) => (target = mod2 != null ? __create(__getProtoOf(mod2)) : {}, __copyProps(
    // If the importer is in node compatibility mode or this is not an ESM
    // file that has been converted to a CommonJS file using a Babel-
    // compatible transform (i.e. "__esModule" has not been set), then set
    // "default" to the CommonJS "module.exports" for node compatibility.
    isNodeMode || !mod2 || !mod2.__esModule ? __defProp(target, "default", { value: mod2, enumerable: true }) : target,
    mod2
  ));

  // node_modules/material-components-web/dist/material-components-web.min.js
  var require_material_components_web_min = __commonJS({
    "node_modules/material-components-web/dist/material-components-web.min.js"(exports, module2) {
      !function(t, e) {
        "object" == typeof exports && "object" == typeof module2 ? module2.exports = e() : "function" == typeof define && define.amd ? define("material-components-web", [], e) : "object" == typeof exports ? exports.mdc = e() : t.mdc = e();
      }(exports, function() {
        return i2 = {}, r.m = n = [function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCFoundation = void 0;
          var i3 = (Object.defineProperty(r2, "cssClasses", { get: function() {
            return {};
          }, enumerable: false, configurable: true }), Object.defineProperty(r2, "strings", { get: function() {
            return {};
          }, enumerable: false, configurable: true }), Object.defineProperty(r2, "numbers", { get: function() {
            return {};
          }, enumerable: false, configurable: true }), Object.defineProperty(r2, "defaultAdapter", { get: function() {
            return {};
          }, enumerable: false, configurable: true }), r2.prototype.init = function() {
          }, r2.prototype.destroy = function() {
          }, r2);
          function r2(t2) {
            void 0 === t2 && (t2 = {}), this.adapter = t2;
          }
          e.MDCFoundation = i3, e.default = i3;
        }, function(t, e, n2) {
          "use strict";
          var r2 = this && this.__read || function(t2, e2) {
            var n3 = "function" == typeof Symbol && t2[Symbol.iterator];
            if (!n3) return t2;
            var i4, r3, o2 = n3.call(t2), s2 = [];
            try {
              for (; (void 0 === e2 || 0 < e2--) && !(i4 = o2.next()).done; ) s2.push(i4.value);
            } catch (t3) {
              r3 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (n3 = o2.return) && n3.call(o2);
              } finally {
                if (r3) throw r3.error;
              }
            }
            return s2;
          }, o = this && this.__spreadArray || function(t2, e2) {
            for (var n3 = 0, i4 = e2.length, r3 = t2.length; n3 < i4; n3++, r3++) t2[r3] = e2[n3];
            return t2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCComponent = void 0;
          var i3 = n2(0), s = (a.attachTo = function(t2) {
            return new a(t2, new i3.MDCFoundation({}));
          }, a.prototype.initialize = function() {
            for (var t2 = [], e2 = 0; e2 < arguments.length; e2++) t2[e2] = arguments[e2];
          }, a.prototype.getDefaultFoundation = function() {
            throw new Error("Subclasses must override getDefaultFoundation to return a properly configured foundation class");
          }, a.prototype.initialSyncWithDOM = function() {
          }, a.prototype.destroy = function() {
            this.foundation.destroy();
          }, a.prototype.listen = function(t2, e2, n3) {
            this.root.addEventListener(t2, e2, n3);
          }, a.prototype.unlisten = function(t2, e2, n3) {
            this.root.removeEventListener(t2, e2, n3);
          }, a.prototype.emit = function(t2, e2, n3) {
            var i4;
            void 0 === n3 && (n3 = false), "function" == typeof CustomEvent ? i4 = new CustomEvent(t2, { bubbles: n3, detail: e2 }) : (i4 = document.createEvent("CustomEvent")).initCustomEvent(t2, n3, false, e2), this.root.dispatchEvent(i4);
          }, a);
          function a(t2, e2) {
            for (var n3 = [], i4 = 2; i4 < arguments.length; i4++) n3[i4 - 2] = arguments[i4];
            this.root = t2, this.initialize.apply(this, o([], r2(n3))), this.foundation = void 0 === e2 ? this.getDefaultFoundation() : e2, this.foundation.init(), this.initialSyncWithDOM();
          }
          e.MDCComponent = s, e.default = s;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), s = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), a = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && o(e2, t2, n3);
            return s(e2, t2), e2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCRipple = void 0;
          var c, u = n2(1), l = n2(5), d = n2(3), p2 = n2(4), h = a(n2(19)), f = (c = u.MDCComponent, r2(y, c), y.attachTo = function(t2, e2) {
            void 0 === e2 && (e2 = { isUnbounded: void 0 });
            var n3 = new y(t2);
            return void 0 !== e2.isUnbounded && (n3.unbounded = e2.isUnbounded), n3;
          }, y.createAdapter = function(n3) {
            return { addClass: function(t2) {
              return n3.root.classList.add(t2);
            }, browserSupportsCssVars: function() {
              return h.supportsCssVariables(window);
            }, computeBoundingRect: function() {
              return n3.root.getBoundingClientRect();
            }, containsEventTarget: function(t2) {
              return n3.root.contains(t2);
            }, deregisterDocumentInteractionHandler: function(t2, e2) {
              return document.documentElement.removeEventListener(t2, e2, l.applyPassive());
            }, deregisterInteractionHandler: function(t2, e2) {
              return n3.root.removeEventListener(t2, e2, l.applyPassive());
            }, deregisterResizeHandler: function(t2) {
              return window.removeEventListener("resize", t2);
            }, getWindowPageOffset: function() {
              return { x: window.pageXOffset, y: window.pageYOffset };
            }, isSurfaceActive: function() {
              return d.matches(n3.root, ":active");
            }, isSurfaceDisabled: function() {
              return Boolean(n3.disabled);
            }, isUnbounded: function() {
              return Boolean(n3.unbounded);
            }, registerDocumentInteractionHandler: function(t2, e2) {
              return document.documentElement.addEventListener(t2, e2, l.applyPassive());
            }, registerInteractionHandler: function(t2, e2) {
              return n3.root.addEventListener(t2, e2, l.applyPassive());
            }, registerResizeHandler: function(t2) {
              return window.addEventListener("resize", t2);
            }, removeClass: function(t2) {
              return n3.root.classList.remove(t2);
            }, updateCssVariable: function(t2, e2) {
              return n3.root.style.setProperty(t2, e2);
            } };
          }, Object.defineProperty(y.prototype, "unbounded", { get: function() {
            return Boolean(this.isUnbounded);
          }, set: function(t2) {
            this.isUnbounded = Boolean(t2), this.setUnbounded();
          }, enumerable: false, configurable: true }), y.prototype.activate = function() {
            this.foundation.activate();
          }, y.prototype.deactivate = function() {
            this.foundation.deactivate();
          }, y.prototype.layout = function() {
            this.foundation.layout();
          }, y.prototype.getDefaultFoundation = function() {
            return new p2.MDCRippleFoundation(y.createAdapter(this));
          }, y.prototype.initialSyncWithDOM = function() {
            var t2 = this.root;
            this.isUnbounded = "mdcRippleIsUnbounded" in t2.dataset;
          }, y.prototype.setUnbounded = function() {
            this.foundation.setUnbounded(Boolean(this.isUnbounded));
          }, y);
          function y() {
            var t2 = null !== c && c.apply(this, arguments) || this;
            return t2.disabled = false, t2;
          }
          e.MDCRipple = f;
        }, function(t, e, n2) {
          "use strict";
          function i3(t2, e2) {
            return (t2.matches || t2.webkitMatchesSelector || t2.msMatchesSelector).call(t2, e2);
          }
          Object.defineProperty(e, "__esModule", { value: true }), e.estimateScrollWidth = e.matches = e.closest = void 0, e.closest = function(t2, e2) {
            if (t2.closest) return t2.closest(e2);
            for (var n3 = t2; n3; ) {
              if (i3(n3, e2)) return n3;
              n3 = n3.parentElement;
            }
            return null;
          }, e.matches = i3, e.estimateScrollWidth = function(t2) {
            var e2 = t2;
            if (null !== e2.offsetParent) return e2.scrollWidth;
            var n3 = e2.cloneNode(true);
            n3.style.setProperty("position", "absolute"), n3.style.setProperty("transform", "translate(-9999px, -9999px)"), document.documentElement.appendChild(n3);
            var i4 = n3.scrollWidth;
            return document.documentElement.removeChild(n3), i4;
          };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCRippleFoundation = void 0;
          var a, c = n2(0), u = n2(47), l = n2(19), d = ["touchstart", "pointerdown", "mousedown", "keydown"], p2 = ["touchend", "pointerup", "mouseup", "contextmenu"], h = [], f = (a = c.MDCFoundation, r2(y, a), Object.defineProperty(y, "cssClasses", { get: function() {
            return u.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(y, "strings", { get: function() {
            return u.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(y, "numbers", { get: function() {
            return u.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(y, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, browserSupportsCssVars: function() {
              return true;
            }, computeBoundingRect: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, containsEventTarget: function() {
              return true;
            }, deregisterDocumentInteractionHandler: function() {
            }, deregisterInteractionHandler: function() {
            }, deregisterResizeHandler: function() {
            }, getWindowPageOffset: function() {
              return { x: 0, y: 0 };
            }, isSurfaceActive: function() {
              return true;
            }, isSurfaceDisabled: function() {
              return true;
            }, isUnbounded: function() {
              return true;
            }, registerDocumentInteractionHandler: function() {
            }, registerInteractionHandler: function() {
            }, registerResizeHandler: function() {
            }, removeClass: function() {
            }, updateCssVariable: function() {
            } };
          }, enumerable: false, configurable: true }), y.prototype.init = function() {
            var t2 = this, e2 = this.supportsPressRipple();
            if (this.registerRootHandlers(e2), e2) {
              var n3 = y.cssClasses, i4 = n3.ROOT, r3 = n3.UNBOUNDED;
              requestAnimationFrame(function() {
                t2.adapter.addClass(i4), t2.adapter.isUnbounded() && (t2.adapter.addClass(r3), t2.layoutInternal());
              });
            }
          }, y.prototype.destroy = function() {
            var t2 = this;
            if (this.supportsPressRipple()) {
              this.activationTimer && (clearTimeout(this.activationTimer), this.activationTimer = 0, this.adapter.removeClass(y.cssClasses.FG_ACTIVATION)), this.fgDeactivationRemovalTimer && (clearTimeout(this.fgDeactivationRemovalTimer), this.fgDeactivationRemovalTimer = 0, this.adapter.removeClass(y.cssClasses.FG_DEACTIVATION));
              var e2 = y.cssClasses, n3 = e2.ROOT, i4 = e2.UNBOUNDED;
              requestAnimationFrame(function() {
                t2.adapter.removeClass(n3), t2.adapter.removeClass(i4), t2.removeCssVars();
              });
            }
            this.deregisterRootHandlers(), this.deregisterDeactivationHandlers();
          }, y.prototype.activate = function(t2) {
            this.activateImpl(t2);
          }, y.prototype.deactivate = function() {
            this.deactivateImpl();
          }, y.prototype.layout = function() {
            var t2 = this;
            this.layoutFrame && cancelAnimationFrame(this.layoutFrame), this.layoutFrame = requestAnimationFrame(function() {
              t2.layoutInternal(), t2.layoutFrame = 0;
            });
          }, y.prototype.setUnbounded = function(t2) {
            var e2 = y.cssClasses.UNBOUNDED;
            t2 ? this.adapter.addClass(e2) : this.adapter.removeClass(e2);
          }, y.prototype.handleFocus = function() {
            var t2 = this;
            requestAnimationFrame(function() {
              return t2.adapter.addClass(y.cssClasses.BG_FOCUSED);
            });
          }, y.prototype.handleBlur = function() {
            var t2 = this;
            requestAnimationFrame(function() {
              return t2.adapter.removeClass(y.cssClasses.BG_FOCUSED);
            });
          }, y.prototype.supportsPressRipple = function() {
            return this.adapter.browserSupportsCssVars();
          }, y.prototype.defaultActivationState = function() {
            return { activationEvent: void 0, hasDeactivationUXRun: false, isActivated: false, isProgrammatic: false, wasActivatedByPointer: false, wasElementMadeActive: false };
          }, y.prototype.registerRootHandlers = function(t2) {
            var e2, n3;
            if (t2) {
              try {
                for (var i4 = s(d), r3 = i4.next(); !r3.done; r3 = i4.next()) {
                  var o2 = r3.value;
                  this.adapter.registerInteractionHandler(o2, this.activateHandler);
                }
              } catch (t3) {
                e2 = { error: t3 };
              } finally {
                try {
                  r3 && !r3.done && (n3 = i4.return) && n3.call(i4);
                } finally {
                  if (e2) throw e2.error;
                }
              }
              this.adapter.isUnbounded() && this.adapter.registerResizeHandler(this.resizeHandler);
            }
            this.adapter.registerInteractionHandler("focus", this.focusHandler), this.adapter.registerInteractionHandler("blur", this.blurHandler);
          }, y.prototype.registerDeactivationHandlers = function(t2) {
            var e2, n3;
            if ("keydown" === t2.type) this.adapter.registerInteractionHandler("keyup", this.deactivateHandler);
            else try {
              for (var i4 = s(p2), r3 = i4.next(); !r3.done; r3 = i4.next()) {
                var o2 = r3.value;
                this.adapter.registerDocumentInteractionHandler(o2, this.deactivateHandler);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                r3 && !r3.done && (n3 = i4.return) && n3.call(i4);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, y.prototype.deregisterRootHandlers = function() {
            var e2, t2;
            try {
              for (var n3 = s(d), i4 = n3.next(); !i4.done; i4 = n3.next()) {
                var r3 = i4.value;
                this.adapter.deregisterInteractionHandler(r3, this.activateHandler);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
            this.adapter.deregisterInteractionHandler("focus", this.focusHandler), this.adapter.deregisterInteractionHandler("blur", this.blurHandler), this.adapter.isUnbounded() && this.adapter.deregisterResizeHandler(this.resizeHandler);
          }, y.prototype.deregisterDeactivationHandlers = function() {
            var e2, t2;
            this.adapter.deregisterInteractionHandler("keyup", this.deactivateHandler);
            try {
              for (var n3 = s(p2), i4 = n3.next(); !i4.done; i4 = n3.next()) {
                var r3 = i4.value;
                this.adapter.deregisterDocumentInteractionHandler(r3, this.deactivateHandler);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, y.prototype.removeCssVars = function() {
            var e2 = this, n3 = y.strings;
            Object.keys(n3).forEach(function(t2) {
              0 === t2.indexOf("VAR_") && e2.adapter.updateCssVariable(n3[t2], null);
            });
          }, y.prototype.activateImpl = function(t2) {
            var e2 = this;
            if (!this.adapter.isSurfaceDisabled()) {
              var n3 = this.activationState;
              if (!n3.isActivated) {
                var i4 = this.previousActivationEvent;
                i4 && void 0 !== t2 && i4.type !== t2.type || (n3.isActivated = true, n3.isProgrammatic = void 0 === t2, n3.activationEvent = t2, n3.wasActivatedByPointer = !n3.isProgrammatic && void 0 !== t2 && ("mousedown" === t2.type || "touchstart" === t2.type || "pointerdown" === t2.type), void 0 !== t2 && 0 < h.length && h.some(function(t3) {
                  return e2.adapter.containsEventTarget(t3);
                }) ? this.resetActivationState() : (void 0 !== t2 && (h.push(t2.target), this.registerDeactivationHandlers(t2)), n3.wasElementMadeActive = this.checkElementMadeActive(t2), n3.wasElementMadeActive && this.animateActivation(), requestAnimationFrame(function() {
                  h = [], n3.wasElementMadeActive || void 0 === t2 || " " !== t2.key && 32 !== t2.keyCode || (n3.wasElementMadeActive = e2.checkElementMadeActive(t2), n3.wasElementMadeActive && e2.animateActivation()), n3.wasElementMadeActive || (e2.activationState = e2.defaultActivationState());
                })));
              }
            }
          }, y.prototype.checkElementMadeActive = function(t2) {
            return void 0 === t2 || "keydown" !== t2.type || this.adapter.isSurfaceActive();
          }, y.prototype.animateActivation = function() {
            var t2 = this, e2 = y.strings, n3 = e2.VAR_FG_TRANSLATE_START, i4 = e2.VAR_FG_TRANSLATE_END, r3 = y.cssClasses, o2 = r3.FG_DEACTIVATION, s2 = r3.FG_ACTIVATION, a2 = y.numbers.DEACTIVATION_TIMEOUT_MS;
            this.layoutInternal();
            var c2 = "", u2 = "";
            if (!this.adapter.isUnbounded()) {
              var l2 = this.getFgTranslationCoordinates(), d2 = l2.startPoint, p3 = l2.endPoint;
              c2 = d2.x + "px, " + d2.y + "px", u2 = p3.x + "px, " + p3.y + "px";
            }
            this.adapter.updateCssVariable(n3, c2), this.adapter.updateCssVariable(i4, u2), clearTimeout(this.activationTimer), clearTimeout(this.fgDeactivationRemovalTimer), this.rmBoundedActivationClasses(), this.adapter.removeClass(o2), this.adapter.computeBoundingRect(), this.adapter.addClass(s2), this.activationTimer = setTimeout(function() {
              t2.activationTimerCallback();
            }, a2);
          }, y.prototype.getFgTranslationCoordinates = function() {
            var t2, e2 = this.activationState, n3 = e2.activationEvent;
            return { startPoint: t2 = { x: (t2 = e2.wasActivatedByPointer ? l.getNormalizedEventCoords(n3, this.adapter.getWindowPageOffset(), this.adapter.computeBoundingRect()) : { x: this.frame.width / 2, y: this.frame.height / 2 }).x - this.initialSize / 2, y: t2.y - this.initialSize / 2 }, endPoint: { x: this.frame.width / 2 - this.initialSize / 2, y: this.frame.height / 2 - this.initialSize / 2 } };
          }, y.prototype.runDeactivationUXLogicIfReady = function() {
            var t2 = this, e2 = y.cssClasses.FG_DEACTIVATION, n3 = this.activationState, i4 = n3.hasDeactivationUXRun, r3 = n3.isActivated;
            !i4 && r3 || !this.activationAnimationHasEnded || (this.rmBoundedActivationClasses(), this.adapter.addClass(e2), this.fgDeactivationRemovalTimer = setTimeout(function() {
              t2.adapter.removeClass(e2);
            }, u.numbers.FG_DEACTIVATION_MS));
          }, y.prototype.rmBoundedActivationClasses = function() {
            var t2 = y.cssClasses.FG_ACTIVATION;
            this.adapter.removeClass(t2), this.activationAnimationHasEnded = false, this.adapter.computeBoundingRect();
          }, y.prototype.resetActivationState = function() {
            var t2 = this;
            this.previousActivationEvent = this.activationState.activationEvent, this.activationState = this.defaultActivationState(), setTimeout(function() {
              return t2.previousActivationEvent = void 0;
            }, y.numbers.TAP_DELAY_MS);
          }, y.prototype.deactivateImpl = function() {
            var t2 = this, e2 = this.activationState;
            if (e2.isActivated) {
              var n3 = o({}, e2);
              e2.isProgrammatic ? (requestAnimationFrame(function() {
                t2.animateDeactivation(n3);
              }), this.resetActivationState()) : (this.deregisterDeactivationHandlers(), requestAnimationFrame(function() {
                t2.activationState.hasDeactivationUXRun = true, t2.animateDeactivation(n3), t2.resetActivationState();
              }));
            }
          }, y.prototype.animateDeactivation = function(t2) {
            var e2 = t2.wasActivatedByPointer, n3 = t2.wasElementMadeActive;
            (e2 || n3) && this.runDeactivationUXLogicIfReady();
          }, y.prototype.layoutInternal = function() {
            var t2 = this;
            this.frame = this.adapter.computeBoundingRect();
            var e2 = Math.max(this.frame.height, this.frame.width);
            this.maxRadius = this.adapter.isUnbounded() ? e2 : Math.sqrt(Math.pow(t2.frame.width, 2) + Math.pow(t2.frame.height, 2)) + y.numbers.PADDING;
            var n3 = Math.floor(e2 * y.numbers.INITIAL_ORIGIN_SCALE);
            this.adapter.isUnbounded() && n3 % 2 != 0 ? this.initialSize = n3 - 1 : this.initialSize = n3, this.fgScale = "" + this.maxRadius / this.initialSize, this.updateLayoutCssVars();
          }, y.prototype.updateLayoutCssVars = function() {
            var t2 = y.strings, e2 = t2.VAR_FG_SIZE, n3 = t2.VAR_LEFT, i4 = t2.VAR_TOP, r3 = t2.VAR_FG_SCALE;
            this.adapter.updateCssVariable(e2, this.initialSize + "px"), this.adapter.updateCssVariable(r3, this.fgScale), this.adapter.isUnbounded() && (this.unboundedCoords = { left: Math.round(this.frame.width / 2 - this.initialSize / 2), top: Math.round(this.frame.height / 2 - this.initialSize / 2) }, this.adapter.updateCssVariable(n3, this.unboundedCoords.left + "px"), this.adapter.updateCssVariable(i4, this.unboundedCoords.top + "px"));
          }, y);
          function y(t2) {
            var e2 = a.call(this, o(o({}, y.defaultAdapter), t2)) || this;
            return e2.activationAnimationHasEnded = false, e2.activationTimer = 0, e2.fgDeactivationRemovalTimer = 0, e2.fgScale = "0", e2.frame = { width: 0, height: 0 }, e2.initialSize = 0, e2.layoutFrame = 0, e2.maxRadius = 0, e2.unboundedCoords = { left: 0, top: 0 }, e2.activationState = e2.defaultActivationState(), e2.activationTimerCallback = function() {
              e2.activationAnimationHasEnded = true, e2.runDeactivationUXLogicIfReady();
            }, e2.activateHandler = function(t3) {
              e2.activateImpl(t3);
            }, e2.deactivateHandler = function() {
              e2.deactivateImpl();
            }, e2.focusHandler = function() {
              e2.handleFocus();
            }, e2.blurHandler = function() {
              e2.handleBlur();
            }, e2.resizeHandler = function() {
              e2.layout();
            }, e2;
          }
          e.MDCRippleFoundation = f, e.default = f;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.applyPassive = void 0, e.applyPassive = function(t2) {
            return void 0 === t2 && (t2 = window), !!function(t3) {
              void 0 === t3 && (t3 = window);
              var e2 = false;
              try {
                var n3 = { get passive() {
                  return !(e2 = true);
                } }, i3 = function() {
                };
                t3.document.addEventListener("test", i3, n3), t3.document.removeEventListener("test", i3, n3);
              } catch (t4) {
                e2 = false;
              }
              return e2;
            }(t2) && { passive: true };
          };
        }, function(t, i3, e) {
          "use strict";
          Object.defineProperty(i3, "__esModule", { value: true }), i3.isNavigationEvent = i3.normalizeKey = i3.KEY = void 0, i3.KEY = { UNKNOWN: "Unknown", BACKSPACE: "Backspace", ENTER: "Enter", SPACEBAR: "Spacebar", PAGE_UP: "PageUp", PAGE_DOWN: "PageDown", END: "End", HOME: "Home", ARROW_LEFT: "ArrowLeft", ARROW_UP: "ArrowUp", ARROW_RIGHT: "ArrowRight", ARROW_DOWN: "ArrowDown", DELETE: "Delete", ESCAPE: "Escape", TAB: "Tab" };
          var r2 = /* @__PURE__ */ new Set();
          r2.add(i3.KEY.BACKSPACE), r2.add(i3.KEY.ENTER), r2.add(i3.KEY.SPACEBAR), r2.add(i3.KEY.PAGE_UP), r2.add(i3.KEY.PAGE_DOWN), r2.add(i3.KEY.END), r2.add(i3.KEY.HOME), r2.add(i3.KEY.ARROW_LEFT), r2.add(i3.KEY.ARROW_UP), r2.add(i3.KEY.ARROW_RIGHT), r2.add(i3.KEY.ARROW_DOWN), r2.add(i3.KEY.DELETE), r2.add(i3.KEY.ESCAPE), r2.add(i3.KEY.TAB);
          var n2 = 8, o = 13, s = 32, a = 33, c = 34, u = 35, l = 36, d = 37, p2 = 38, h = 39, f = 40, y = 46, C = 27, E = 9, g = /* @__PURE__ */ new Map();
          g.set(n2, i3.KEY.BACKSPACE), g.set(o, i3.KEY.ENTER), g.set(s, i3.KEY.SPACEBAR), g.set(a, i3.KEY.PAGE_UP), g.set(c, i3.KEY.PAGE_DOWN), g.set(u, i3.KEY.END), g.set(l, i3.KEY.HOME), g.set(d, i3.KEY.ARROW_LEFT), g.set(p2, i3.KEY.ARROW_UP), g.set(h, i3.KEY.ARROW_RIGHT), g.set(f, i3.KEY.ARROW_DOWN), g.set(y, i3.KEY.DELETE), g.set(C, i3.KEY.ESCAPE), g.set(E, i3.KEY.TAB);
          var _ = /* @__PURE__ */ new Set();
          function m(t2) {
            var e2 = t2.key;
            if (r2.has(e2)) return e2;
            var n3 = g.get(t2.keyCode);
            return n3 || i3.KEY.UNKNOWN;
          }
          _.add(i3.KEY.PAGE_UP), _.add(i3.KEY.PAGE_DOWN), _.add(i3.KEY.END), _.add(i3.KEY.HOME), _.add(i3.KEY.ARROW_LEFT), _.add(i3.KEY.ARROW_UP), _.add(i3.KEY.ARROW_RIGHT), _.add(i3.KEY.ARROW_DOWN), i3.normalizeKey = m, i3.isNavigationEvent = function(t2) {
            return _.has(m(t2));
          };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2;
          Object.defineProperty(e, "__esModule", { value: true }), e.evolutionClassNameMap = e.evolutionAttribute = e.deprecatedClassNameMap = e.numbers = e.cssClasses = e.strings = void 0;
          var o = { LIST_ITEM_ACTIVATED_CLASS: "mdc-list-item--activated", LIST_ITEM_CLASS: "mdc-list-item", LIST_ITEM_DISABLED_CLASS: "mdc-list-item--disabled", LIST_ITEM_SELECTED_CLASS: "mdc-list-item--selected", LIST_ITEM_TEXT_CLASS: "mdc-list-item__text", LIST_ITEM_PRIMARY_TEXT_CLASS: "mdc-list-item__primary-text", ROOT: "mdc-list" }, s = ((i3 = {})["" + (e.cssClasses = o).LIST_ITEM_ACTIVATED_CLASS] = "mdc-list-item--activated", i3["" + o.LIST_ITEM_CLASS] = "mdc-list-item", i3["" + o.LIST_ITEM_DISABLED_CLASS] = "mdc-list-item--disabled", i3["" + o.LIST_ITEM_SELECTED_CLASS] = "mdc-list-item--selected", i3["" + o.LIST_ITEM_PRIMARY_TEXT_CLASS] = "mdc-list-item__primary-text", i3["" + o.ROOT] = "mdc-list", i3);
          e.evolutionClassNameMap = s;
          var a = ((r2 = {})["" + o.LIST_ITEM_ACTIVATED_CLASS] = "mdc-deprecated-list-item--activated", r2["" + o.LIST_ITEM_CLASS] = "mdc-deprecated-list-item", r2["" + o.LIST_ITEM_DISABLED_CLASS] = "mdc-deprecated-list-item--disabled", r2["" + o.LIST_ITEM_SELECTED_CLASS] = "mdc-deprecated-list-item--selected", r2["" + o.LIST_ITEM_TEXT_CLASS] = "mdc-deprecated-list-item__text", r2["" + o.LIST_ITEM_PRIMARY_TEXT_CLASS] = "mdc-deprecated-list-item__primary-text", r2["" + o.ROOT] = "mdc-deprecated-list", r2);
          e.deprecatedClassNameMap = a;
          var c = { ACTION_EVENT: "MDCList:action", SELECTION_CHANGE_EVENT: "MDCList:selectionChange", ARIA_CHECKED: "aria-checked", ARIA_CHECKED_CHECKBOX_SELECTOR: '[role="checkbox"][aria-checked="true"]', ARIA_CHECKED_RADIO_SELECTOR: '[role="radio"][aria-checked="true"]', ARIA_CURRENT: "aria-current", ARIA_DISABLED: "aria-disabled", ARIA_ORIENTATION: "aria-orientation", ARIA_ORIENTATION_HORIZONTAL: "horizontal", ARIA_ROLE_CHECKBOX_SELECTOR: '[role="checkbox"]', ARIA_SELECTED: "aria-selected", ARIA_INTERACTIVE_ROLES_SELECTOR: '[role="listbox"], [role="menu"]', ARIA_MULTI_SELECTABLE_SELECTOR: '[aria-multiselectable="true"]', CHECKBOX_RADIO_SELECTOR: 'input[type="checkbox"], input[type="radio"]', CHECKBOX_SELECTOR: 'input[type="checkbox"]', CHILD_ELEMENTS_TO_TOGGLE_TABINDEX: "\n    ." + o.LIST_ITEM_CLASS + " button:not(:disabled),\n    ." + o.LIST_ITEM_CLASS + " a,\n    ." + a[o.LIST_ITEM_CLASS] + " button:not(:disabled),\n    ." + a[o.LIST_ITEM_CLASS] + " a\n  ", DEPRECATED_SELECTOR: ".mdc-deprecated-list", FOCUSABLE_CHILD_ELEMENTS: "\n    ." + o.LIST_ITEM_CLASS + " button:not(:disabled),\n    ." + o.LIST_ITEM_CLASS + " a,\n    ." + o.LIST_ITEM_CLASS + ' input[type="radio"]:not(:disabled),\n    .' + o.LIST_ITEM_CLASS + ' input[type="checkbox"]:not(:disabled),\n    .' + a[o.LIST_ITEM_CLASS] + " button:not(:disabled),\n    ." + a[o.LIST_ITEM_CLASS] + " a,\n    ." + a[o.LIST_ITEM_CLASS] + ' input[type="radio"]:not(:disabled),\n    .' + a[o.LIST_ITEM_CLASS] + ' input[type="checkbox"]:not(:disabled)\n  ', RADIO_SELECTOR: 'input[type="radio"]', SELECTED_ITEM_SELECTOR: '[aria-selected="true"], [aria-current="true"]' };
          e.strings = c;
          e.numbers = { UNSET_INDEX: -1, TYPEAHEAD_BUFFER_CLEAR_TIMEOUT_MS: 300 };
          e.evolutionAttribute = "evolution";
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.Corner = e.CornerBit = e.numbers = e.strings = e.cssClasses = void 0;
          e.cssClasses = { ANCHOR: "mdc-menu-surface--anchor", ANIMATING_CLOSED: "mdc-menu-surface--animating-closed", ANIMATING_OPEN: "mdc-menu-surface--animating-open", FIXED: "mdc-menu-surface--fixed", IS_OPEN_BELOW: "mdc-menu-surface--is-open-below", OPEN: "mdc-menu-surface--open", ROOT: "mdc-menu-surface" };
          var i3 = { CLOSED_EVENT: "MDCMenuSurface:closed", CLOSING_EVENT: "MDCMenuSurface:closing", OPENED_EVENT: "MDCMenuSurface:opened", OPENING_EVENT: "MDCMenuSurface:opening", FOCUSABLE_ELEMENTS: ["button:not(:disabled)", '[href]:not([aria-disabled="true"])', "input:not(:disabled)", "select:not(:disabled)", "textarea:not(:disabled)", '[tabindex]:not([tabindex="-1"]):not([aria-disabled="true"])'].join(", ") };
          e.strings = i3;
          var r2, o, s, a;
          e.numbers = { TRANSITION_OPEN_DURATION: 120, TRANSITION_CLOSE_DURATION: 75, MARGIN_TO_EDGE: 32, ANCHOR_TO_MENU_SURFACE_WIDTH_RATIO: 0.67, TOUCH_EVENT_WAIT_MS: 30 }, (o = r2 = r2 || {})[o.BOTTOM = 1] = "BOTTOM", o[o.CENTER = 2] = "CENTER", o[o.RIGHT = 4] = "RIGHT", o[o.FLIP_RTL = 8] = "FLIP_RTL", e.CornerBit = r2, (a = s = s || {})[a.TOP_LEFT = 0] = "TOP_LEFT", a[a.TOP_RIGHT = 4] = "TOP_RIGHT", a[a.BOTTOM_LEFT = 1] = "BOTTOM_LEFT", a[a.BOTTOM_RIGHT = 5] = "BOTTOM_RIGHT", a[a.TOP_START = 8] = "TOP_START", a[a.TOP_END = 12] = "TOP_END", a[a.BOTTOM_START = 9] = "BOTTOM_START", a[a.BOTTOM_END = 13] = "BOTTOM_END", e.Corner = s;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.numbers = e.cssClasses = void 0;
          e.cssClasses = { FIXED_CLASS: "mdc-top-app-bar--fixed", FIXED_SCROLLED_CLASS: "mdc-top-app-bar--fixed-scrolled", SHORT_CLASS: "mdc-top-app-bar--short", SHORT_COLLAPSED_CLASS: "mdc-top-app-bar--short-collapsed", SHORT_HAS_ACTION_ITEM_CLASS: "mdc-top-app-bar--short-has-action-item" };
          e.numbers = { DEBOUNCE_THROTTLE_RESIZE_TIME_MS: 100, MAX_TOP_APP_BAR_HEIGHT: 128 };
          e.strings = { ACTION_ITEM_SELECTOR: ".mdc-top-app-bar__action-item", NAVIGATION_EVENT: "MDCTopAppBar:nav", NAVIGATION_ICON_SELECTOR: ".mdc-top-app-bar__navigation-icon", ROOT_SELECTOR: ".mdc-top-app-bar", TITLE_SELECTOR: ".mdc-top-app-bar__title" };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.getCorrectEventName = e.getCorrectPropertyName = void 0;
          var s = { animation: { prefixed: "-webkit-animation", standard: "animation" }, transform: { prefixed: "-webkit-transform", standard: "transform" }, transition: { prefixed: "-webkit-transition", standard: "transition" } }, a = { animationend: { cssProperty: "animation", prefixed: "webkitAnimationEnd", standard: "animationend" }, animationiteration: { cssProperty: "animation", prefixed: "webkitAnimationIteration", standard: "animationiteration" }, animationstart: { cssProperty: "animation", prefixed: "webkitAnimationStart", standard: "animationstart" }, transitionend: { cssProperty: "transition", prefixed: "webkitTransitionEnd", standard: "transitionend" } };
          function c(t2) {
            return Boolean(t2.document) && "function" == typeof t2.document.createElement;
          }
          e.getCorrectPropertyName = function(t2, e2) {
            if (c(t2) && e2 in s) {
              var n3 = t2.document.createElement("div"), i3 = s[e2], r2 = i3.standard, o = i3.prefixed;
              return r2 in n3.style ? r2 : o;
            }
            return e2;
          }, e.getCorrectEventName = function(t2, e2) {
            if (c(t2) && e2 in a) {
              var n3 = t2.document.createElement("div"), i3 = a[e2], r2 = i3.standard, o = i3.prefixed;
              return i3.cssProperty in n3.style ? r2 : o;
            }
            return e2;
          };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.FocusTrap = void 0;
          var o = "mdc-dom-focus-sentinel", i3 = (r2.prototype.trapFocus = function() {
            var t2 = this.getFocusableElements(this.root);
            if (0 === t2.length) throw new Error("FocusTrap: Element must have at least one focusable child.");
            this.elFocusedBeforeTrapFocus = document.activeElement instanceof HTMLElement ? document.activeElement : null, this.wrapTabFocus(this.root), this.options.skipInitialFocus || this.focusInitialElement(t2, this.options.initialFocusEl);
          }, r2.prototype.releaseFocus = function() {
            [].slice.call(this.root.querySelectorAll("." + o)).forEach(function(t2) {
              t2.parentElement.removeChild(t2);
            }), !this.options.skipRestoreFocus && this.elFocusedBeforeTrapFocus && this.elFocusedBeforeTrapFocus.focus();
          }, r2.prototype.wrapTabFocus = function(e2) {
            var n3 = this, t2 = this.createSentinel(), i4 = this.createSentinel();
            t2.addEventListener("focus", function() {
              var t3 = n3.getFocusableElements(e2);
              0 < t3.length && t3[t3.length - 1].focus();
            }), i4.addEventListener("focus", function() {
              var t3 = n3.getFocusableElements(e2);
              0 < t3.length && t3[0].focus();
            }), e2.insertBefore(t2, e2.children[0]), e2.appendChild(i4);
          }, r2.prototype.focusInitialElement = function(t2, e2) {
            var n3 = 0;
            e2 && (n3 = Math.max(t2.indexOf(e2), 0)), t2[n3].focus();
          }, r2.prototype.getFocusableElements = function(t2) {
            return [].slice.call(t2.querySelectorAll("[autofocus], [tabindex], a, input, textarea, select, button")).filter(function(t3) {
              var e2 = "true" === t3.getAttribute("aria-disabled") || null != t3.getAttribute("disabled") || null != t3.getAttribute("hidden") || "true" === t3.getAttribute("aria-hidden"), n3 = 0 <= t3.tabIndex && 0 < t3.getBoundingClientRect().width && !t3.classList.contains(o) && !e2, i4 = false;
              if (n3) {
                var r3 = getComputedStyle(t3);
                i4 = "none" === r3.display || "hidden" === r3.visibility;
              }
              return n3 && !i4;
            });
          }, r2.prototype.createSentinel = function() {
            var t2 = document.createElement("div");
            return t2.setAttribute("tabindex", "0"), t2.setAttribute("aria-hidden", "true"), t2.classList.add(o), t2;
          }, r2);
          function r2(t2, e2) {
            void 0 === e2 && (e2 = {}), this.root = t2, this.options = e2, this.elFocusedBeforeTrapFocus = null;
          }
          e.FocusTrap = i3;
        }, function(t, e, n2) {
          "use strict";
          var i3;
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.InteractionTrigger = void 0, (i3 = e.InteractionTrigger || (e.InteractionTrigger = {}))[i3.UNSPECIFIED = 0] = "UNSPECIFIED", i3[i3.CLICK = 1] = "CLICK", i3[i3.BACKSPACE_KEY = 2] = "BACKSPACE_KEY", i3[i3.DELETE_KEY = 3] = "DELETE_KEY", i3[i3.SPACEBAR_KEY = 4] = "SPACEBAR_KEY", i3[i3.ENTER_KEY = 5] = "ENTER_KEY", e.strings = { ARIA_HIDDEN: "aria-hidden", INTERACTION_EVENT: "MDCChipTrailingAction:interaction", NAVIGATION_EVENT: "MDCChipTrailingAction:navigation", TAB_INDEX: "tabindex" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2;
          Object.defineProperty(e, "__esModule", { value: true }), e.jumpChipKeys = e.navigationKeys = e.cssClasses = e.strings = e.EventSource = e.Direction = void 0, (i3 = e.Direction || (e.Direction = {})).LEFT = "left", i3.RIGHT = "right", (r2 = e.EventSource || (e.EventSource = {})).PRIMARY = "primary", r2.TRAILING = "trailing", r2.NONE = "none", e.strings = { ADDED_ANNOUNCEMENT_ATTRIBUTE: "data-mdc-chip-added-announcement", ARIA_CHECKED: "aria-checked", ARROW_DOWN_KEY: "ArrowDown", ARROW_LEFT_KEY: "ArrowLeft", ARROW_RIGHT_KEY: "ArrowRight", ARROW_UP_KEY: "ArrowUp", BACKSPACE_KEY: "Backspace", CHECKMARK_SELECTOR: ".mdc-chip__checkmark", DELETE_KEY: "Delete", END_KEY: "End", ENTER_KEY: "Enter", ENTRY_ANIMATION_NAME: "mdc-chip-entry", HOME_KEY: "Home", IE_ARROW_DOWN_KEY: "Down", IE_ARROW_LEFT_KEY: "Left", IE_ARROW_RIGHT_KEY: "Right", IE_ARROW_UP_KEY: "Up", IE_DELETE_KEY: "Del", INTERACTION_EVENT: "MDCChip:interaction", LEADING_ICON_SELECTOR: ".mdc-chip__icon--leading", NAVIGATION_EVENT: "MDCChip:navigation", PRIMARY_ACTION_SELECTOR: ".mdc-chip__primary-action", REMOVED_ANNOUNCEMENT_ATTRIBUTE: "data-mdc-chip-removed-announcement", REMOVAL_EVENT: "MDCChip:removal", SELECTION_EVENT: "MDCChip:selection", SPACEBAR_KEY: " ", TAB_INDEX: "tabindex", TRAILING_ACTION_SELECTOR: ".mdc-chip-trailing-action", TRAILING_ICON_INTERACTION_EVENT: "MDCChip:trailingIconInteraction", TRAILING_ICON_SELECTOR: ".mdc-chip__icon--trailing" }, e.cssClasses = { CHECKMARK: "mdc-chip__checkmark", CHIP_EXIT: "mdc-chip--exit", DELETABLE: "mdc-chip--deletable", EDITABLE: "mdc-chip--editable", EDITING: "mdc-chip--editing", HIDDEN_LEADING_ICON: "mdc-chip__icon--leading-hidden", LEADING_ICON: "mdc-chip__icon--leading", PRIMARY_ACTION: "mdc-chip__primary-action", PRIMARY_ACTION_FOCUSED: "mdc-chip--primary-action-focused", SELECTED: "mdc-chip--selected", TEXT: "mdc-chip__text", TRAILING_ACTION: "mdc-chip__trailing-action", TRAILING_ICON: "mdc-chip__icon--trailing" }, e.navigationKeys = /* @__PURE__ */ new Set(), e.navigationKeys.add(e.strings.ARROW_LEFT_KEY), e.navigationKeys.add(e.strings.ARROW_RIGHT_KEY), e.navigationKeys.add(e.strings.ARROW_DOWN_KEY), e.navigationKeys.add(e.strings.ARROW_UP_KEY), e.navigationKeys.add(e.strings.END_KEY), e.navigationKeys.add(e.strings.HOME_KEY), e.navigationKeys.add(e.strings.IE_ARROW_LEFT_KEY), e.navigationKeys.add(e.strings.IE_ARROW_RIGHT_KEY), e.navigationKeys.add(e.strings.IE_ARROW_DOWN_KEY), e.navigationKeys.add(e.strings.IE_ARROW_UP_KEY), e.jumpChipKeys = /* @__PURE__ */ new Set(), e.jumpChipKeys.add(e.strings.ARROW_UP_KEY), e.jumpChipKeys.add(e.strings.ARROW_DOWN_KEY), e.jumpChipKeys.add(e.strings.HOME_KEY), e.jumpChipKeys.add(e.strings.END_KEY), e.jumpChipKeys.add(e.strings.IE_ARROW_UP_KEY), e.jumpChipKeys.add(e.strings.IE_ARROW_DOWN_KEY);
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, h = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCMenuSurfaceFoundation = void 0;
          var s, a = n2(0), E = n2(8), c = (s = a.MDCFoundation, r2(g, s), Object.defineProperty(g, "cssClasses", { get: function() {
            return E.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(g, "strings", { get: function() {
            return E.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(g, "numbers", { get: function() {
            return E.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(g, "Corner", { get: function() {
            return E.Corner;
          }, enumerable: false, configurable: true }), Object.defineProperty(g, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return false;
            }, hasAnchor: function() {
              return false;
            }, isElementInContainer: function() {
              return false;
            }, isFocused: function() {
              return false;
            }, isRtl: function() {
              return false;
            }, getInnerDimensions: function() {
              return { height: 0, width: 0 };
            }, getAnchorDimensions: function() {
              return null;
            }, getWindowDimensions: function() {
              return { height: 0, width: 0 };
            }, getBodyDimensions: function() {
              return { height: 0, width: 0 };
            }, getWindowScroll: function() {
              return { x: 0, y: 0 };
            }, setPosition: function() {
            }, setMaxHeight: function() {
            }, setTransformOrigin: function() {
            }, saveFocus: function() {
            }, restoreFocus: function() {
            }, notifyClose: function() {
            }, notifyClosing: function() {
            }, notifyOpen: function() {
            }, notifyOpening: function() {
            } };
          }, enumerable: false, configurable: true }), g.prototype.init = function() {
            var t2 = g.cssClasses, e2 = t2.ROOT, n3 = t2.OPEN;
            if (!this.adapter.hasClass(e2)) throw new Error(e2 + " class required in root element.");
            this.adapter.hasClass(n3) && (this.isSurfaceOpen = true);
          }, g.prototype.destroy = function() {
            clearTimeout(this.openAnimationEndTimerId), clearTimeout(this.closeAnimationEndTimerId), cancelAnimationFrame(this.animationRequestId);
          }, g.prototype.setAnchorCorner = function(t2) {
            this.anchorCorner = t2;
          }, g.prototype.flipCornerHorizontally = function() {
            this.originCorner = this.originCorner ^ E.CornerBit.RIGHT;
          }, g.prototype.setAnchorMargin = function(t2) {
            this.anchorMargin.top = t2.top || 0, this.anchorMargin.right = t2.right || 0, this.anchorMargin.bottom = t2.bottom || 0, this.anchorMargin.left = t2.left || 0;
          }, g.prototype.setIsHoisted = function(t2) {
            this.isHoistedElement = t2;
          }, g.prototype.setFixedPosition = function(t2) {
            this.isFixedPosition = t2;
          }, g.prototype.isFixed = function() {
            return this.isFixedPosition;
          }, g.prototype.setAbsolutePosition = function(t2, e2) {
            this.position.x = this.isFinite(t2) ? t2 : 0, this.position.y = this.isFinite(e2) ? e2 : 0;
          }, g.prototype.setIsHorizontallyCenteredOnViewport = function(t2) {
            this.isHorizontallyCenteredOnViewport = t2;
          }, g.prototype.setQuickOpen = function(t2) {
            this.isQuickOpen = t2;
          }, g.prototype.setMaxHeight = function(t2) {
            this.maxHeight = t2;
          }, g.prototype.setOpenBottomBias = function(t2) {
            this.openBottomBias = t2;
          }, g.prototype.isOpen = function() {
            return this.isSurfaceOpen;
          }, g.prototype.open = function() {
            var t2 = this;
            this.isSurfaceOpen || (this.adapter.notifyOpening(), this.adapter.saveFocus(), this.isQuickOpen ? (this.isSurfaceOpen = true, this.adapter.addClass(g.cssClasses.OPEN), this.dimensions = this.adapter.getInnerDimensions(), this.autoposition(), this.adapter.notifyOpen()) : (this.adapter.addClass(g.cssClasses.ANIMATING_OPEN), this.animationRequestId = requestAnimationFrame(function() {
              t2.dimensions = t2.adapter.getInnerDimensions(), t2.autoposition(), t2.adapter.addClass(g.cssClasses.OPEN), t2.openAnimationEndTimerId = setTimeout(function() {
                t2.openAnimationEndTimerId = 0, t2.adapter.removeClass(g.cssClasses.ANIMATING_OPEN), t2.adapter.notifyOpen();
              }, E.numbers.TRANSITION_OPEN_DURATION);
            }), this.isSurfaceOpen = true));
          }, g.prototype.close = function(t2) {
            var e2 = this;
            if (void 0 === t2 && (t2 = false), this.isSurfaceOpen) {
              if (this.adapter.notifyClosing(), this.isQuickOpen) return this.isSurfaceOpen = false, t2 || this.maybeRestoreFocus(), this.adapter.removeClass(g.cssClasses.OPEN), this.adapter.removeClass(g.cssClasses.IS_OPEN_BELOW), void this.adapter.notifyClose();
              this.adapter.addClass(g.cssClasses.ANIMATING_CLOSED), requestAnimationFrame(function() {
                e2.adapter.removeClass(g.cssClasses.OPEN), e2.adapter.removeClass(g.cssClasses.IS_OPEN_BELOW), e2.closeAnimationEndTimerId = setTimeout(function() {
                  e2.closeAnimationEndTimerId = 0, e2.adapter.removeClass(g.cssClasses.ANIMATING_CLOSED), e2.adapter.notifyClose();
                }, E.numbers.TRANSITION_CLOSE_DURATION);
              }), this.isSurfaceOpen = false, t2 || this.maybeRestoreFocus();
            }
          }, g.prototype.handleBodyClick = function(t2) {
            var e2 = t2.target;
            this.adapter.isElementInContainer(e2) || this.close();
          }, g.prototype.handleKeydown = function(t2) {
            var e2 = t2.keyCode;
            "Escape" !== t2.key && 27 !== e2 || this.close();
          }, g.prototype.autoposition = function() {
            var t2;
            this.measurements = this.getAutoLayoutmeasurements();
            var e2 = this.getoriginCorner(), n3 = this.getMenuSurfaceMaxHeight(e2), i4 = this.hasBit(e2, E.CornerBit.BOTTOM) ? "bottom" : "top", r3 = this.hasBit(e2, E.CornerBit.RIGHT) ? "right" : "left", o2 = this.getHorizontalOriginOffset(e2), s2 = this.getVerticalOriginOffset(e2), a2 = this.measurements, c2 = a2.anchorSize, u = a2.surfaceSize, l = ((t2 = {})[r3] = o2, t2[i4] = s2, t2);
            c2.width / u.width > E.numbers.ANCHOR_TO_MENU_SURFACE_WIDTH_RATIO && (r3 = "center"), (this.isHoistedElement || this.isFixedPosition) && this.adjustPositionForHoistedElement(l), this.adapter.setTransformOrigin(r3 + " " + i4), this.adapter.setPosition(l), this.adapter.setMaxHeight(n3 ? n3 + "px" : ""), this.hasBit(e2, E.CornerBit.BOTTOM) || this.adapter.addClass(g.cssClasses.IS_OPEN_BELOW);
          }, g.prototype.getAutoLayoutmeasurements = function() {
            var t2 = this.adapter.getAnchorDimensions(), e2 = this.adapter.getBodyDimensions(), n3 = this.adapter.getWindowDimensions(), i4 = this.adapter.getWindowScroll();
            return { anchorSize: t2 = t2 || { top: this.position.y, right: this.position.x, bottom: this.position.y, left: this.position.x, width: 0, height: 0 }, bodySize: e2, surfaceSize: this.dimensions, viewportDistance: { top: t2.top, right: n3.width - t2.right, bottom: n3.height - t2.bottom, left: t2.left }, viewportSize: n3, windowScroll: i4 };
          }, g.prototype.getoriginCorner = function() {
            var t2, e2, n3 = this.originCorner, i4 = this.measurements, r3 = i4.viewportDistance, o2 = i4.anchorSize, s2 = i4.surfaceSize, a2 = g.numbers.MARGIN_TO_EDGE;
            !(0 < (e2 = this.hasBit(this.anchorCorner, E.CornerBit.BOTTOM) ? (t2 = r3.top - a2 + this.anchorMargin.bottom, r3.bottom - a2 - this.anchorMargin.bottom) : (t2 = r3.top - a2 + this.anchorMargin.top, r3.bottom - a2 + o2.height - this.anchorMargin.top)) - s2.height) && t2 > e2 + this.openBottomBias && (n3 = this.setBit(n3, E.CornerBit.BOTTOM));
            var c2, u, l = this.adapter.isRtl(), d = this.hasBit(this.anchorCorner, E.CornerBit.FLIP_RTL), p2 = this.hasBit(this.anchorCorner, E.CornerBit.RIGHT) || this.hasBit(n3, E.CornerBit.RIGHT), h7 = false;
            u = (h7 = l && d ? !p2 : p2) ? (c2 = r3.left + o2.width + this.anchorMargin.right, r3.right - this.anchorMargin.right) : (c2 = r3.left + this.anchorMargin.left, r3.right + o2.width - this.anchorMargin.left);
            var f = 0 < c2 - s2.width, y = 0 < u - s2.width, C = this.hasBit(n3, E.CornerBit.FLIP_RTL) && this.hasBit(n3, E.CornerBit.RIGHT);
            return y && C && l || !f && C ? n3 = this.unsetBit(n3, E.CornerBit.RIGHT) : (f && h7 && l || f && !h7 && p2 || !y && u <= c2) && (n3 = this.setBit(n3, E.CornerBit.RIGHT)), n3;
          }, g.prototype.getMenuSurfaceMaxHeight = function(t2) {
            if (0 < this.maxHeight) return this.maxHeight;
            var e2 = this.measurements.viewportDistance, n3 = 0, i4 = this.hasBit(t2, E.CornerBit.BOTTOM), r3 = this.hasBit(this.anchorCorner, E.CornerBit.BOTTOM), o2 = g.numbers.MARGIN_TO_EDGE;
            return i4 ? (n3 = e2.top + this.anchorMargin.top - o2, r3 || (n3 += this.measurements.anchorSize.height)) : (n3 = e2.bottom - this.anchorMargin.bottom + this.measurements.anchorSize.height - o2, r3 && (n3 -= this.measurements.anchorSize.height)), n3;
          }, g.prototype.getHorizontalOriginOffset = function(t2) {
            var e2 = this.measurements.anchorSize, n3 = this.hasBit(t2, E.CornerBit.RIGHT), i4 = this.hasBit(this.anchorCorner, E.CornerBit.RIGHT);
            if (n3) {
              var r3 = i4 ? e2.width - this.anchorMargin.left : this.anchorMargin.right;
              return this.isHoistedElement || this.isFixedPosition ? r3 - (this.measurements.viewportSize.width - this.measurements.bodySize.width) : r3;
            }
            return i4 ? e2.width - this.anchorMargin.right : this.anchorMargin.left;
          }, g.prototype.getVerticalOriginOffset = function(t2) {
            var e2 = this.measurements.anchorSize, n3 = this.hasBit(t2, E.CornerBit.BOTTOM), i4 = this.hasBit(this.anchorCorner, E.CornerBit.BOTTOM);
            return n3 ? i4 ? e2.height - this.anchorMargin.top : -this.anchorMargin.bottom : i4 ? e2.height + this.anchorMargin.bottom : this.anchorMargin.top;
          }, g.prototype.adjustPositionForHoistedElement = function(t2) {
            var e2, n3, i4 = this.measurements, r3 = i4.windowScroll, o2 = i4.viewportDistance, s2 = i4.surfaceSize, a2 = i4.viewportSize, c2 = Object.keys(t2);
            try {
              for (var u = h(c2), l = u.next(); !l.done; l = u.next()) {
                var d = l.value, p2 = t2[d] || 0;
                !this.isHorizontallyCenteredOnViewport || "left" !== d && "right" !== d ? (p2 += o2[d], this.isFixedPosition || ("top" === d ? p2 += r3.y : "bottom" === d ? p2 -= r3.y : "left" === d ? p2 += r3.x : p2 -= r3.x), t2[d] = p2) : t2[d] = (a2.width - s2.width) / 2;
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                l && !l.done && (n3 = u.return) && n3.call(u);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, g.prototype.maybeRestoreFocus = function() {
            var t2 = this, e2 = this.adapter.isFocused(), n3 = this.adapter.getOwnerDocument ? this.adapter.getOwnerDocument() : document, i4 = n3.activeElement && this.adapter.isElementInContainer(n3.activeElement);
            (e2 || i4) && setTimeout(function() {
              t2.adapter.restoreFocus();
            }, E.numbers.TOUCH_EVENT_WAIT_MS);
          }, g.prototype.hasBit = function(t2, e2) {
            return Boolean(t2 & e2);
          }, g.prototype.setBit = function(t2, e2) {
            return t2 | e2;
          }, g.prototype.unsetBit = function(t2, e2) {
            return t2 ^ e2;
          }, g.prototype.isFinite = function(t2) {
            return "number" == typeof t2 && isFinite(t2);
          }, g);
          function g(t2) {
            var e2 = s.call(this, o(o({}, g.defaultAdapter), t2)) || this;
            return e2.isSurfaceOpen = false, e2.isQuickOpen = false, e2.isHoistedElement = false, e2.isFixedPosition = false, e2.isHorizontallyCenteredOnViewport = false, e2.maxHeight = 0, e2.openBottomBias = 0, e2.openAnimationEndTimerId = 0, e2.closeAnimationEndTimerId = 0, e2.animationRequestId = 0, e2.anchorCorner = E.Corner.TOP_START, e2.originCorner = E.Corner.TOP_START, e2.anchorMargin = { top: 0, right: 0, bottom: 0, left: 0 }, e2.position = { x: 0, y: 0 }, e2;
          }
          e.MDCMenuSurfaceFoundation = c, e.default = c;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.DefaultFocusState = e.numbers = e.strings = e.cssClasses = void 0;
          e.cssClasses = { MENU_SELECTED_LIST_ITEM: "mdc-menu-item--selected", MENU_SELECTION_GROUP: "mdc-menu__selection-group", ROOT: "mdc-menu" };
          e.strings = { ARIA_CHECKED_ATTR: "aria-checked", ARIA_DISABLED_ATTR: "aria-disabled", CHECKBOX_SELECTOR: 'input[type="checkbox"]', LIST_SELECTOR: ".mdc-list,.mdc-deprecated-list", SELECTED_EVENT: "MDCMenu:selected", SKIP_RESTORE_FOCUS: "data-menu-item-skip-restore-focus" };
          var i3, r2;
          e.numbers = { FOCUS_ROOT_INDEX: -1 }, (r2 = i3 = i3 || {})[r2.NONE = 0] = "NONE", r2[r2.LIST_ROOT = 1] = "LIST_ROOT", r2[r2.FIRST_ITEM = 2] = "FIRST_ITEM", r2[r2.LAST_ITEM = 3] = "LAST_ITEM", e.DefaultFocusState = i3;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.numbers = e.strings = e.cssClasses = void 0;
          e.cssClasses = { CLOSING: "mdc-snackbar--closing", OPEN: "mdc-snackbar--open", OPENING: "mdc-snackbar--opening" };
          e.strings = { ACTION_SELECTOR: ".mdc-snackbar__action", ARIA_LIVE_LABEL_TEXT_ATTR: "data-mdc-snackbar-label-text", CLOSED_EVENT: "MDCSnackbar:closed", CLOSING_EVENT: "MDCSnackbar:closing", DISMISS_SELECTOR: ".mdc-snackbar__dismiss", LABEL_SELECTOR: ".mdc-snackbar__label", OPENED_EVENT: "MDCSnackbar:opened", OPENING_EVENT: "MDCSnackbar:opening", REASON_ACTION: "action", REASON_DISMISS: "dismiss", SURFACE_SELECTOR: ".mdc-snackbar__surface" };
          e.numbers = { DEFAULT_AUTO_DISMISS_TIMEOUT_MS: 5e3, INDETERMINATE: -1, MAX_AUTO_DISMISS_TIMEOUT_MS: 1e4, MIN_AUTO_DISMISS_TIMEOUT_MS: 4e3, SNACKBAR_ANIMATION_CLOSE_TIME_MS: 75, SNACKBAR_ANIMATION_OPEN_TIME_MS: 150, ARIA_LIVE_DELAY_MS: 1e3 };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabIndicatorFoundation = void 0;
          var s, a = n2(0), c = n2(104), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, computeContentClientRect: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, setContentStyleProperty: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.computeContentClientRect = function() {
            return this.adapter.computeContentClientRect();
          }, l);
          function l(t2) {
            return s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
          }
          e.MDCTabIndicatorFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2;
          Object.defineProperty(e, "__esModule", { value: true }), e.Action = e.CloseReason = e.selectors = e.events = e.numbers = e.cssClasses = void 0, e.cssClasses = { CLOSING: "mdc-banner--closing", OPEN: "mdc-banner--open", OPENING: "mdc-banner--opening" }, e.numbers = { BANNER_ANIMATION_CLOSE_TIME_MS: 250, BANNER_ANIMATION_OPEN_TIME_MS: 300 }, e.events = { CLOSED: "MDCBanner:closed", CLOSING: "MDCBanner:closing", OPENED: "MDCBanner:opened", OPENING: "MDCBanner:opening", ACTION_CLICKED: "MDCBanner:actionClicked" }, e.selectors = { CONTENT: ".mdc-banner__content", PRIMARY_ACTION: ".mdc-banner__primary-action", SECONDARY_ACTION: ".mdc-banner__secondary-action", TEXT: ".mdc-banner__text" }, (i3 = e.CloseReason || (e.CloseReason = {}))[i3.PRIMARY = 0] = "PRIMARY", i3[i3.SECONDARY = 1] = "SECONDARY", i3[i3.UNSPECIFIED = 2] = "UNSPECIFIED", (r2 = e.Action || (e.Action = {}))[r2.PRIMARY = 0] = "PRIMARY", r2[r2.SECONDARY = 1] = "SECONDARY", r2[r2.UNKNOWN = 2] = "UNKNOWN";
        }, function(t, e, n2) {
          "use strict";
          var s;
          Object.defineProperty(e, "__esModule", { value: true }), e.getNormalizedEventCoords = e.supportsCssVariables = void 0, e.supportsCssVariables = function(t2, e2) {
            void 0 === e2 && (e2 = false);
            var n3, i3 = t2.CSS;
            if ("boolean" == typeof s && !e2) return s;
            if (!(i3 && "function" == typeof i3.supports)) return false;
            var r2 = i3.supports("--css-vars", "yes"), o = i3.supports("(--css-vars: yes)") && i3.supports("color", "#00000000");
            return n3 = r2 || o, e2 || (s = n3), n3;
          }, e.getNormalizedEventCoords = function(t2, e2, n3) {
            if (!t2) return { x: 0, y: 0 };
            var i3, r2, o = e2.x, s2 = e2.y, a = o + n3.left, c = s2 + n3.top;
            if ("touchstart" === t2.type) {
              var u = t2;
              i3 = u.changedTouches[0].pageX - a, r2 = u.changedTouches[0].pageY - c;
            } else {
              var l = t2;
              i3 = l.pageX - a, r2 = l.pageY - c;
            }
            return { x: i3, y: r2 };
          };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.numbers = e.strings = e.cssClasses = void 0, e.cssClasses = { ANIM_CHECKED_INDETERMINATE: "mdc-checkbox--anim-checked-indeterminate", ANIM_CHECKED_UNCHECKED: "mdc-checkbox--anim-checked-unchecked", ANIM_INDETERMINATE_CHECKED: "mdc-checkbox--anim-indeterminate-checked", ANIM_INDETERMINATE_UNCHECKED: "mdc-checkbox--anim-indeterminate-unchecked", ANIM_UNCHECKED_CHECKED: "mdc-checkbox--anim-unchecked-checked", ANIM_UNCHECKED_INDETERMINATE: "mdc-checkbox--anim-unchecked-indeterminate", BACKGROUND: "mdc-checkbox__background", CHECKED: "mdc-checkbox--checked", CHECKMARK: "mdc-checkbox__checkmark", CHECKMARK_PATH: "mdc-checkbox__checkmark-path", DISABLED: "mdc-checkbox--disabled", INDETERMINATE: "mdc-checkbox--indeterminate", MIXEDMARK: "mdc-checkbox__mixedmark", NATIVE_CONTROL: "mdc-checkbox__native-control", ROOT: "mdc-checkbox", SELECTED: "mdc-checkbox--selected", UPGRADED: "mdc-checkbox--upgraded" }, e.strings = { ARIA_CHECKED_ATTR: "aria-checked", ARIA_CHECKED_INDETERMINATE_VALUE: "mixed", DATA_INDETERMINATE_ATTR: "data-indeterminate", NATIVE_CONTROL_SELECTOR: ".mdc-checkbox__native-control", TRANSITION_STATE_CHECKED: "checked", TRANSITION_STATE_INDETERMINATE: "indeterminate", TRANSITION_STATE_INIT: "init", TRANSITION_STATE_UNCHECKED: "unchecked" }, e.numbers = { ANIM_END_LATCH_MS: 250 };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCChipFoundation = void 0;
          var s, a, c = n2(0), u = n2(13), l = { bottom: 0, height: 0, left: 0, right: 0, top: 0, width: 0 };
          (a = s = s || {})[a.SHOULD_FOCUS = 0] = "SHOULD_FOCUS", a[a.SHOULD_NOT_FOCUS = 1] = "SHOULD_NOT_FOCUS";
          var d, p2 = (d = c.MDCFoundation, r2(h, d), Object.defineProperty(h, "strings", { get: function() {
            return u.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "cssClasses", { get: function() {
            return u.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, addClassToLeadingIcon: function() {
            }, eventTargetHasClass: function() {
              return false;
            }, focusPrimaryAction: function() {
            }, focusTrailingAction: function() {
            }, getAttribute: function() {
              return null;
            }, getCheckmarkBoundingClientRect: function() {
              return l;
            }, getComputedStyleValue: function() {
              return "";
            }, getRootBoundingClientRect: function() {
              return l;
            }, hasClass: function() {
              return false;
            }, hasLeadingIcon: function() {
              return false;
            }, isRTL: function() {
              return false;
            }, isTrailingActionNavigable: function() {
              return false;
            }, notifyEditFinish: function() {
            }, notifyEditStart: function() {
            }, notifyInteraction: function() {
            }, notifyNavigation: function() {
            }, notifyRemoval: function() {
            }, notifySelection: function() {
            }, notifyTrailingIconInteraction: function() {
            }, removeClass: function() {
            }, removeClassFromLeadingIcon: function() {
            }, removeTrailingActionFocus: function() {
            }, setPrimaryActionAttr: function() {
            }, setStyleProperty: function() {
            } };
          }, enumerable: false, configurable: true }), h.prototype.isSelected = function() {
            return this.adapter.hasClass(u.cssClasses.SELECTED);
          }, h.prototype.isEditable = function() {
            return this.adapter.hasClass(u.cssClasses.EDITABLE);
          }, h.prototype.isEditing = function() {
            return this.adapter.hasClass(u.cssClasses.EDITING);
          }, h.prototype.setSelected = function(t2) {
            this.setSelectedImpl(t2), this.notifySelection(t2);
          }, h.prototype.setSelectedFromChipSet = function(t2, e2) {
            this.setSelectedImpl(t2), e2 && this.notifyIgnoredSelection(t2);
          }, h.prototype.getShouldRemoveOnTrailingIconClick = function() {
            return this.shouldRemoveOnTrailingIconClick;
          }, h.prototype.setShouldRemoveOnTrailingIconClick = function(t2) {
            this.shouldRemoveOnTrailingIconClick = t2;
          }, h.prototype.setShouldFocusPrimaryActionOnClick = function(t2) {
            this.shouldFocusPrimaryActionOnClick = t2;
          }, h.prototype.getDimensions = function() {
            function t2() {
              return e2.adapter.getRootBoundingClientRect();
            }
            var e2 = this;
            if (!this.adapter.hasLeadingIcon()) {
              var n3 = e2.adapter.getCheckmarkBoundingClientRect();
              if (n3) {
                var i4 = t2();
                return { bottom: i4.bottom, height: i4.height, left: i4.left, right: i4.right, top: i4.top, width: i4.width + n3.height };
              }
            }
            return t2();
          }, h.prototype.beginExit = function() {
            this.adapter.addClass(u.cssClasses.CHIP_EXIT);
          }, h.prototype.handleClick = function() {
            this.adapter.notifyInteraction(), this.setPrimaryActionFocusable(this.getFocusBehavior());
          }, h.prototype.handleDoubleClick = function() {
            this.isEditable() && this.startEditing();
          }, h.prototype.handleTransitionEnd = function(t2) {
            var e2 = this, n3 = this.adapter.eventTargetHasClass(t2.target, u.cssClasses.CHIP_EXIT), i4 = "width" === t2.propertyName, r3 = "opacity" === t2.propertyName;
            if (n3 && r3) {
              var o2 = this.adapter.getComputedStyleValue("width");
              requestAnimationFrame(function() {
                e2.adapter.setStyleProperty("width", o2), e2.adapter.setStyleProperty("padding", "0"), e2.adapter.setStyleProperty("margin", "0"), requestAnimationFrame(function() {
                  e2.adapter.setStyleProperty("width", "0");
                });
              });
            } else {
              if (n3 && i4) {
                this.removeFocus();
                var s2 = this.adapter.getAttribute(u.strings.REMOVED_ANNOUNCEMENT_ATTRIBUTE);
                this.adapter.notifyRemoval(s2);
              }
              if (r3) {
                var a2 = this.adapter.eventTargetHasClass(t2.target, u.cssClasses.LEADING_ICON) && this.adapter.hasClass(u.cssClasses.SELECTED), c2 = this.adapter.eventTargetHasClass(t2.target, u.cssClasses.CHECKMARK) && !this.adapter.hasClass(u.cssClasses.SELECTED);
                a2 ? this.adapter.addClassToLeadingIcon(u.cssClasses.HIDDEN_LEADING_ICON) : c2 && this.adapter.removeClassFromLeadingIcon(u.cssClasses.HIDDEN_LEADING_ICON);
              }
            }
          }, h.prototype.handleFocusIn = function(t2) {
            this.eventFromPrimaryAction(t2) && this.adapter.addClass(u.cssClasses.PRIMARY_ACTION_FOCUSED);
          }, h.prototype.handleFocusOut = function(t2) {
            this.eventFromPrimaryAction(t2) && (this.isEditing() && this.finishEditing(), this.adapter.removeClass(u.cssClasses.PRIMARY_ACTION_FOCUSED));
          }, h.prototype.handleTrailingActionInteraction = function() {
            this.adapter.notifyTrailingIconInteraction(), this.removeChip();
          }, h.prototype.handleKeydown = function(t2) {
            if (!this.isEditing()) return this.isEditable() && this.shouldStartEditing(t2) && (t2.preventDefault(), this.startEditing()), this.shouldNotifyInteraction(t2) ? (this.adapter.notifyInteraction(), void this.setPrimaryActionFocusable(this.getFocusBehavior())) : this.isDeleteAction(t2) ? (t2.preventDefault(), void this.removeChip()) : void (u.navigationKeys.has(t2.key) && (t2.preventDefault(), this.focusNextAction(t2.key, u.EventSource.PRIMARY)));
            this.shouldFinishEditing(t2) && (t2.preventDefault(), this.finishEditing());
          }, h.prototype.handleTrailingActionNavigation = function(t2) {
            this.focusNextAction(t2.detail.key, u.EventSource.TRAILING);
          }, h.prototype.removeFocus = function() {
            this.adapter.setPrimaryActionAttr(u.strings.TAB_INDEX, "-1"), this.adapter.removeTrailingActionFocus();
          }, h.prototype.focusPrimaryAction = function() {
            this.setPrimaryActionFocusable(s.SHOULD_FOCUS);
          }, h.prototype.focusTrailingAction = function() {
            if (this.adapter.isTrailingActionNavigable()) return this.adapter.setPrimaryActionAttr(u.strings.TAB_INDEX, "-1"), void this.adapter.focusTrailingAction();
            this.focusPrimaryAction();
          }, h.prototype.setPrimaryActionFocusable = function(t2) {
            this.adapter.setPrimaryActionAttr(u.strings.TAB_INDEX, "0"), t2 === s.SHOULD_FOCUS && this.adapter.focusPrimaryAction(), this.adapter.removeTrailingActionFocus();
          }, h.prototype.getFocusBehavior = function() {
            return this.shouldFocusPrimaryActionOnClick ? s.SHOULD_FOCUS : s.SHOULD_NOT_FOCUS;
          }, h.prototype.focusNextAction = function(t2, e2) {
            var n3 = this.adapter.isTrailingActionNavigable(), i4 = this.getDirection(t2);
            !u.jumpChipKeys.has(t2) && n3 ? e2 !== u.EventSource.PRIMARY || i4 !== u.Direction.RIGHT ? e2 !== u.EventSource.TRAILING || i4 !== u.Direction.LEFT ? this.adapter.notifyNavigation(t2, u.EventSource.NONE) : this.focusPrimaryAction() : this.focusTrailingAction() : this.adapter.notifyNavigation(t2, e2);
          }, h.prototype.getDirection = function(t2) {
            var e2 = this.adapter.isRTL(), n3 = t2 === u.strings.ARROW_LEFT_KEY || t2 === u.strings.IE_ARROW_LEFT_KEY, i4 = t2 === u.strings.ARROW_RIGHT_KEY || t2 === u.strings.IE_ARROW_RIGHT_KEY;
            return !e2 && n3 || e2 && i4 ? u.Direction.LEFT : u.Direction.RIGHT;
          }, h.prototype.removeChip = function() {
            this.shouldRemoveOnTrailingIconClick && this.beginExit();
          }, h.prototype.shouldStartEditing = function(t2) {
            return this.eventFromPrimaryAction(t2) && t2.key === u.strings.ENTER_KEY;
          }, h.prototype.shouldFinishEditing = function(t2) {
            return t2.key === u.strings.ENTER_KEY;
          }, h.prototype.shouldNotifyInteraction = function(t2) {
            return t2.key === u.strings.ENTER_KEY || t2.key === u.strings.SPACEBAR_KEY;
          }, h.prototype.isDeleteAction = function(t2) {
            return this.adapter.hasClass(u.cssClasses.DELETABLE) && (t2.key === u.strings.BACKSPACE_KEY || t2.key === u.strings.DELETE_KEY || t2.key === u.strings.IE_DELETE_KEY);
          }, h.prototype.setSelectedImpl = function(t2) {
            t2 ? (this.adapter.addClass(u.cssClasses.SELECTED), this.adapter.setPrimaryActionAttr(u.strings.ARIA_CHECKED, "true")) : (this.adapter.removeClass(u.cssClasses.SELECTED), this.adapter.setPrimaryActionAttr(u.strings.ARIA_CHECKED, "false"));
          }, h.prototype.notifySelection = function(t2) {
            this.adapter.notifySelection(t2, false);
          }, h.prototype.notifyIgnoredSelection = function(t2) {
            this.adapter.notifySelection(t2, true);
          }, h.prototype.eventFromPrimaryAction = function(t2) {
            return this.adapter.eventTargetHasClass(t2.target, u.cssClasses.PRIMARY_ACTION);
          }, h.prototype.startEditing = function() {
            this.adapter.addClass(u.cssClasses.EDITING), this.adapter.notifyEditStart();
          }, h.prototype.finishEditing = function() {
            this.adapter.removeClass(u.cssClasses.EDITING), this.adapter.notifyEditFinish();
          }, h);
          function h(t2) {
            var e2 = d.call(this, o(o({}, h.defaultAdapter), t2)) || this;
            return e2.shouldRemoveOnTrailingIconClick = true, e2.shouldFocusPrimaryActionOnClick = true, e2;
          }
          e.MDCChipFoundation = p2, e.default = p2;
        }, function(t, e, n2) {
          "use strict";
          var i3;
          Object.defineProperty(e, "__esModule", { value: true }), e.events = e.SortValue = e.strings = e.messages = e.selectors = e.dataAttributes = e.attributes = e.cssClasses = void 0, e.cssClasses = { CELL: "mdc-data-table__cell", CELL_NUMERIC: "mdc-data-table__cell--numeric", CONTENT: "mdc-data-table__content", HEADER_CELL: "mdc-data-table__header-cell", HEADER_CELL_LABEL: "mdc-data-table__header-cell-label", HEADER_CELL_SORTED: "mdc-data-table__header-cell--sorted", HEADER_CELL_SORTED_DESCENDING: "mdc-data-table__header-cell--sorted-descending", HEADER_CELL_WITH_SORT: "mdc-data-table__header-cell--with-sort", HEADER_CELL_WRAPPER: "mdc-data-table__header-cell-wrapper", HEADER_ROW: "mdc-data-table__header-row", HEADER_ROW_CHECKBOX: "mdc-data-table__header-row-checkbox", IN_PROGRESS: "mdc-data-table--in-progress", LINEAR_PROGRESS: "mdc-data-table__linear-progress", PAGINATION_ROWS_PER_PAGE_LABEL: "mdc-data-table__pagination-rows-per-page-label", PAGINATION_ROWS_PER_PAGE_SELECT: "mdc-data-table__pagination-rows-per-page-select", PROGRESS_INDICATOR: "mdc-data-table__progress-indicator", ROOT: "mdc-data-table", ROW: "mdc-data-table__row", ROW_CHECKBOX: "mdc-data-table__row-checkbox", ROW_SELECTED: "mdc-data-table__row--selected", SORT_ICON_BUTTON: "mdc-data-table__sort-icon-button", SORT_STATUS_LABEL: "mdc-data-table__sort-status-label", TABLE_CONTAINER: "mdc-data-table__table-container" }, e.attributes = { ARIA_SELECTED: "aria-selected", ARIA_SORT: "aria-sort" }, e.dataAttributes = { COLUMN_ID: "data-column-id", ROW_ID: "data-row-id" }, e.selectors = { CONTENT: "." + e.cssClasses.CONTENT, HEADER_CELL: "." + e.cssClasses.HEADER_CELL, HEADER_CELL_WITH_SORT: "." + e.cssClasses.HEADER_CELL_WITH_SORT, HEADER_ROW: "." + e.cssClasses.HEADER_ROW, HEADER_ROW_CHECKBOX: "." + e.cssClasses.HEADER_ROW_CHECKBOX, PROGRESS_INDICATOR: "." + e.cssClasses.PROGRESS_INDICATOR, ROW: "." + e.cssClasses.ROW, ROW_CHECKBOX: "." + e.cssClasses.ROW_CHECKBOX, ROW_SELECTED: "." + e.cssClasses.ROW_SELECTED, SORT_ICON_BUTTON: "." + e.cssClasses.SORT_ICON_BUTTON, SORT_STATUS_LABEL: "." + e.cssClasses.SORT_STATUS_LABEL }, e.messages = { SORTED_IN_DESCENDING: "Sorted in descending order", SORTED_IN_ASCENDING: "Sorted in ascending order" }, e.strings = { ARIA_SELECTED: e.attributes.ARIA_SELECTED, ARIA_SORT: e.attributes.ARIA_SORT, DATA_ROW_ID_ATTR: e.dataAttributes.ROW_ID, HEADER_ROW_CHECKBOX_SELECTOR: e.selectors.HEADER_ROW_CHECKBOX, ROW_CHECKBOX_SELECTOR: e.selectors.ROW_CHECKBOX, ROW_SELECTED_SELECTOR: e.selectors.ROW_SELECTED, ROW_SELECTOR: e.selectors.ROW }, (i3 = e.SortValue || (e.SortValue = {})).ASCENDING = "ascending", i3.DESCENDING = "descending", i3.NONE = "none", i3.OTHER = "other", e.events = { ROW_CLICK: "MDCDataTable:rowClick", ROW_SELECTION_CHANGED: "MDCDataTable:rowSelectionChanged", SELECTED_ALL: "MDCDataTable:selectedAll", SORTED: "MDCDataTable:sorted", UNSELECTED_ALL: "MDCDataTable:unselectedAll" };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.AnimationFrame = void 0;
          var i3 = (r2.prototype.request = function(e2, n3) {
            var i4 = this;
            this.cancel(e2);
            var t2 = requestAnimationFrame(function(t3) {
              i4.rafIDs.delete(e2), n3(t3);
            });
            this.rafIDs.set(e2, t2);
          }, r2.prototype.cancel = function(t2) {
            var e2 = this.rafIDs.get(t2);
            e2 && (cancelAnimationFrame(e2), this.rafIDs.delete(t2));
          }, r2.prototype.cancelAll = function() {
            var n3 = this;
            this.rafIDs.forEach(function(t2, e2) {
              n3.cancel(e2);
            });
          }, r2.prototype.getQueue = function() {
            var n3 = [];
            return this.rafIDs.forEach(function(t2, e2) {
              n3.push(e2);
            }), n3;
          }, r2);
          function r2() {
            this.rafIDs = /* @__PURE__ */ new Map();
          }
          e.AnimationFrame = i3;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCList = void 0;
          var o, s = n2(1), a = n2(3), c = n2(7), u = n2(25), l = (o = s.MDCComponent, r2(d, o), Object.defineProperty(d.prototype, "vertical", { set: function(t2) {
            this.foundation.setVerticalOrientation(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(d.prototype, "listElements", { get: function() {
            return Array.from(this.root.querySelectorAll("." + this.classNameMap[c.cssClasses.LIST_ITEM_CLASS]));
          }, enumerable: false, configurable: true }), Object.defineProperty(d.prototype, "wrapFocus", { set: function(t2) {
            this.foundation.setWrapFocus(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(d.prototype, "typeaheadInProgress", { get: function() {
            return this.foundation.isTypeaheadInProgress();
          }, enumerable: false, configurable: true }), Object.defineProperty(d.prototype, "hasTypeahead", { set: function(t2) {
            this.foundation.setHasTypeahead(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(d.prototype, "singleSelection", { set: function(t2) {
            this.foundation.setSingleSelection(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(d.prototype, "disabledItemsFocusable", { set: function(t2) {
            this.foundation.setDisabledItemsFocusable(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(d.prototype, "selectedIndex", { get: function() {
            return this.foundation.getSelectedIndex();
          }, set: function(t2) {
            this.foundation.setSelectedIndex(t2);
          }, enumerable: false, configurable: true }), d.attachTo = function(t2) {
            return new d(t2);
          }, d.prototype.initialSyncWithDOM = function() {
            this.isEvolutionEnabled = c.evolutionAttribute in this.root.dataset, this.isEvolutionEnabled ? this.classNameMap = c.evolutionClassNameMap : a.matches(this.root, c.strings.DEPRECATED_SELECTOR) ? this.classNameMap = c.deprecatedClassNameMap : this.classNameMap = Object.values(c.cssClasses).reduce(function(t2, e2) {
              return t2[e2] = e2, t2;
            }, {}), this.handleClick = this.handleClickEvent.bind(this), this.handleKeydown = this.handleKeydownEvent.bind(this), this.focusInEventListener = this.handleFocusInEvent.bind(this), this.focusOutEventListener = this.handleFocusOutEvent.bind(this), this.listen("keydown", this.handleKeydown), this.listen("click", this.handleClick), this.listen("focusin", this.focusInEventListener), this.listen("focusout", this.focusOutEventListener), this.layout(), this.initializeListType(), this.ensureFocusable();
          }, d.prototype.destroy = function() {
            this.unlisten("keydown", this.handleKeydown), this.unlisten("click", this.handleClick), this.unlisten("focusin", this.focusInEventListener), this.unlisten("focusout", this.focusOutEventListener);
          }, d.prototype.layout = function() {
            var t2 = this.root.getAttribute(c.strings.ARIA_ORIENTATION);
            this.vertical = t2 !== c.strings.ARIA_ORIENTATION_HORIZONTAL;
            var e2 = "." + this.classNameMap[c.cssClasses.LIST_ITEM_CLASS] + ":not([tabindex])", n3 = c.strings.FOCUSABLE_CHILD_ELEMENTS, i4 = this.root.querySelectorAll(e2);
            i4.length && Array.prototype.forEach.call(i4, function(t3) {
              t3.setAttribute("tabindex", "-1");
            });
            var r3 = this.root.querySelectorAll(n3);
            r3.length && Array.prototype.forEach.call(r3, function(t3) {
              t3.setAttribute("tabindex", "-1");
            }), this.isEvolutionEnabled && this.foundation.setUseSelectedAttribute(true), this.foundation.layout();
          }, d.prototype.getPrimaryText = function(t2) {
            var e2, n3 = t2.querySelector("." + this.classNameMap[c.cssClasses.LIST_ITEM_PRIMARY_TEXT_CLASS]);
            if (this.isEvolutionEnabled || n3) return null !== (e2 = null == n3 ? void 0 : n3.textContent) && void 0 !== e2 ? e2 : "";
            var i4 = t2.querySelector("." + this.classNameMap[c.cssClasses.LIST_ITEM_TEXT_CLASS]);
            return i4 && i4.textContent || "";
          }, d.prototype.initializeListType = function() {
            var e2 = this;
            if (this.isInteractive = a.matches(this.root, c.strings.ARIA_INTERACTIVE_ROLES_SELECTOR), this.isEvolutionEnabled && this.isInteractive) {
              var t2 = Array.from(this.root.querySelectorAll(c.strings.SELECTED_ITEM_SELECTOR), function(t3) {
                return e2.listElements.indexOf(t3);
              });
              a.matches(this.root, c.strings.ARIA_MULTI_SELECTABLE_SELECTOR) ? this.selectedIndex = t2 : 0 < t2.length && (this.selectedIndex = t2[0]);
            } else {
              var n3 = this.root.querySelectorAll(c.strings.ARIA_ROLE_CHECKBOX_SELECTOR), i4 = this.root.querySelector(c.strings.ARIA_CHECKED_RADIO_SELECTOR);
              if (n3.length) {
                var r3 = this.root.querySelectorAll(c.strings.ARIA_CHECKED_CHECKBOX_SELECTOR);
                this.selectedIndex = Array.from(r3, function(t3) {
                  return e2.listElements.indexOf(t3);
                });
              } else i4 && (this.selectedIndex = this.listElements.indexOf(i4));
            }
          }, d.prototype.setEnabled = function(t2, e2) {
            this.foundation.setEnabled(t2, e2);
          }, d.prototype.typeaheadMatchItem = function(t2, e2) {
            return this.foundation.typeaheadMatchItem(t2, e2, true);
          }, d.prototype.getDefaultFoundation = function() {
            var r3 = this, t2 = { addClassForElementIndex: function(t3, e2) {
              var n3 = r3.listElements[t3];
              n3 && n3.classList.add(r3.classNameMap[e2]);
            }, focusItemAtIndex: function(t3) {
              var e2 = r3.listElements[t3];
              e2 && e2.focus();
            }, getAttributeForElementIndex: function(t3, e2) {
              return r3.listElements[t3].getAttribute(e2);
            }, getFocusedElementIndex: function() {
              return r3.listElements.indexOf(document.activeElement);
            }, getListItemCount: function() {
              return r3.listElements.length;
            }, getPrimaryTextAtIndex: function(t3) {
              return r3.getPrimaryText(r3.listElements[t3]);
            }, hasCheckboxAtIndex: function(t3) {
              return !!r3.listElements[t3].querySelector(c.strings.CHECKBOX_SELECTOR);
            }, hasRadioAtIndex: function(t3) {
              return !!r3.listElements[t3].querySelector(c.strings.RADIO_SELECTOR);
            }, isCheckboxCheckedAtIndex: function(t3) {
              return r3.listElements[t3].querySelector(c.strings.CHECKBOX_SELECTOR).checked;
            }, isFocusInsideList: function() {
              return r3.root !== document.activeElement && r3.root.contains(document.activeElement);
            }, isRootFocused: function() {
              return document.activeElement === r3.root;
            }, listItemAtIndexHasClass: function(t3, e2) {
              return r3.listElements[t3].classList.contains(r3.classNameMap[e2]);
            }, notifyAction: function(t3) {
              r3.emit(c.strings.ACTION_EVENT, { index: t3 }, true);
            }, notifySelectionChange: function(t3) {
              r3.emit(c.strings.SELECTION_CHANGE_EVENT, { changedIndices: t3 }, true);
            }, removeClassForElementIndex: function(t3, e2) {
              var n3 = r3.listElements[t3];
              n3 && n3.classList.remove(r3.classNameMap[e2]);
            }, setAttributeForElementIndex: function(t3, e2, n3) {
              var i4 = r3.listElements[t3];
              i4 && i4.setAttribute(e2, n3);
            }, setCheckedCheckboxOrRadioAtIndex: function(t3, e2) {
              var n3 = r3.listElements[t3].querySelector(c.strings.CHECKBOX_RADIO_SELECTOR);
              n3.checked = e2;
              var i4 = document.createEvent("Event");
              i4.initEvent("change", true, true), n3.dispatchEvent(i4);
            }, setTabIndexForListItemChildren: function(t3, e2) {
              var n3 = r3.listElements[t3], i4 = c.strings.CHILD_ELEMENTS_TO_TOGGLE_TABINDEX;
              Array.prototype.forEach.call(n3.querySelectorAll(i4), function(t4) {
                t4.setAttribute("tabindex", e2);
              });
            } };
            return new u.MDCListFoundation(t2);
          }, d.prototype.ensureFocusable = function() {
            if (this.isEvolutionEnabled && this.isInteractive && !this.root.querySelector("." + this.classNameMap[c.cssClasses.LIST_ITEM_CLASS] + '[tabindex="0"]')) {
              var t2 = this.initialFocusIndex();
              -1 !== t2 && (this.listElements[t2].tabIndex = 0);
            }
          }, d.prototype.initialFocusIndex = function() {
            if (this.selectedIndex instanceof Array && 0 < this.selectedIndex.length) return this.selectedIndex[0];
            if ("number" == typeof this.selectedIndex && this.selectedIndex !== c.numbers.UNSET_INDEX) return this.selectedIndex;
            var t2 = this.root.querySelector("." + this.classNameMap[c.cssClasses.LIST_ITEM_CLASS] + ":not(." + this.classNameMap[c.cssClasses.LIST_ITEM_DISABLED_CLASS] + ")");
            return null === t2 ? -1 : this.getListItemIndex(t2);
          }, d.prototype.getListItemIndex = function(t2) {
            var e2 = a.closest(t2, "." + this.classNameMap[c.cssClasses.LIST_ITEM_CLASS] + ", ." + this.classNameMap[c.cssClasses.ROOT]);
            return e2 && a.matches(e2, "." + this.classNameMap[c.cssClasses.LIST_ITEM_CLASS]) ? this.listElements.indexOf(e2) : -1;
          }, d.prototype.handleFocusInEvent = function(t2) {
            var e2 = this.getListItemIndex(t2.target);
            this.foundation.handleFocusIn(e2);
          }, d.prototype.handleFocusOutEvent = function(t2) {
            var e2 = this.getListItemIndex(t2.target);
            this.foundation.handleFocusOut(e2);
          }, d.prototype.handleKeydownEvent = function(t2) {
            var e2 = this.getListItemIndex(t2.target), n3 = t2.target;
            this.foundation.handleKeydown(t2, n3.classList.contains(this.classNameMap[c.cssClasses.LIST_ITEM_CLASS]), e2);
          }, d.prototype.handleClickEvent = function(t2) {
            var e2 = this.getListItemIndex(t2.target), n3 = t2.target, i4 = !a.matches(n3, c.strings.CHECKBOX_RADIO_SELECTOR);
            this.foundation.handleClick(e2, i4, t2);
          }, d);
          function d() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCList = l;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), a = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), c = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && s(e2, t2, n3);
            return a(e2, t2), e2;
          }, d = this && this.__read || function(t2, e2) {
            var n3 = "function" == typeof Symbol && t2[Symbol.iterator];
            if (!n3) return t2;
            var i4, r3, o2 = n3.call(t2), s2 = [];
            try {
              for (; (void 0 === e2 || 0 < e2--) && !(i4 = o2.next()).done; ) s2.push(i4.value);
            } catch (t3) {
              r3 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (n3 = o2.return) && n3.call(o2);
              } finally {
                if (r3) throw r3.error;
              }
            }
            return s2;
          }, p2 = this && this.__spreadArray || function(t2, e2) {
            for (var n3 = 0, i4 = e2.length, r3 = t2.length; n3 < i4; n3++, r3++) t2[r3] = e2[n3];
            return t2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCListFoundation = void 0;
          var u = n2(0), v = n2(6), T = n2(7), b = n2(64), A = c(n2(156));
          var l = ["Alt", "Control", "Meta", "Shift"];
          function O(e2) {
            var n3 = new Set(e2 ? l.filter(function(t2) {
              return e2.getModifierState(t2);
            }) : []);
            return function(t2) {
              return t2.every(function(t3) {
                return n3.has(t3);
              }) && t2.length === n3.size;
            };
          }
          var h, f = (h = u.MDCFoundation, r2(y, h), Object.defineProperty(y, "strings", { get: function() {
            return T.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(y, "cssClasses", { get: function() {
            return T.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(y, "numbers", { get: function() {
            return T.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(y, "defaultAdapter", { get: function() {
            return { addClassForElementIndex: function() {
            }, focusItemAtIndex: function() {
            }, getAttributeForElementIndex: function() {
              return null;
            }, getFocusedElementIndex: function() {
              return 0;
            }, getListItemCount: function() {
              return 0;
            }, hasCheckboxAtIndex: function() {
              return false;
            }, hasRadioAtIndex: function() {
              return false;
            }, isCheckboxCheckedAtIndex: function() {
              return false;
            }, isFocusInsideList: function() {
              return false;
            }, isRootFocused: function() {
              return false;
            }, listItemAtIndexHasClass: function() {
              return false;
            }, notifyAction: function() {
            }, notifySelectionChange: function() {
            }, removeClassForElementIndex: function() {
            }, setAttributeForElementIndex: function() {
            }, setCheckedCheckboxOrRadioAtIndex: function() {
            }, setTabIndexForListItemChildren: function() {
            }, getPrimaryTextAtIndex: function() {
              return "";
            } };
          }, enumerable: false, configurable: true }), y.prototype.layout = function() {
            0 !== this.adapter.getListItemCount() && (this.adapter.hasCheckboxAtIndex(0) ? this.isCheckboxList = true : this.adapter.hasRadioAtIndex(0) ? this.isRadioList = true : this.maybeInitializeSingleSelection(), this.hasTypeahead && (this.sortedIndexByFirstChar = this.typeaheadInitSortedIndex()));
          }, y.prototype.getFocusedItemIndex = function() {
            return this.focusedItemIndex;
          }, y.prototype.setWrapFocus = function(t2) {
            this.wrapFocus = t2;
          }, y.prototype.setVerticalOrientation = function(t2) {
            this.isVertical = t2;
          }, y.prototype.setSingleSelection = function(t2) {
            (this.isSingleSelectionList = t2) && (this.maybeInitializeSingleSelection(), this.selectedIndex = this.getSelectedIndexFromDOM());
          }, y.prototype.setDisabledItemsFocusable = function(t2) {
            this.areDisabledItemsFocusable = t2;
          }, y.prototype.maybeInitializeSingleSelection = function() {
            var t2 = this.getSelectedIndexFromDOM();
            t2 !== T.numbers.UNSET_INDEX && (this.adapter.listItemAtIndexHasClass(t2, T.cssClasses.LIST_ITEM_ACTIVATED_CLASS) && this.setUseActivatedClass(true), this.isSingleSelectionList = true, this.selectedIndex = t2);
          }, y.prototype.getSelectedIndexFromDOM = function() {
            for (var t2 = T.numbers.UNSET_INDEX, e2 = this.adapter.getListItemCount(), n3 = 0; n3 < e2; n3++) {
              var i4 = this.adapter.listItemAtIndexHasClass(n3, T.cssClasses.LIST_ITEM_SELECTED_CLASS), r3 = this.adapter.listItemAtIndexHasClass(n3, T.cssClasses.LIST_ITEM_ACTIVATED_CLASS);
              if (i4 || r3) {
                t2 = n3;
                break;
              }
            }
            return t2;
          }, y.prototype.setHasTypeahead = function(t2) {
            (this.hasTypeahead = t2) && (this.sortedIndexByFirstChar = this.typeaheadInitSortedIndex());
          }, y.prototype.isTypeaheadInProgress = function() {
            return this.hasTypeahead && A.isTypingInProgress(this.typeaheadState);
          }, y.prototype.setUseActivatedClass = function(t2) {
            this.useActivatedClass = t2;
          }, y.prototype.setUseSelectedAttribute = function(t2) {
            this.useSelectedAttr = t2;
          }, y.prototype.getSelectedIndex = function() {
            return this.selectedIndex;
          }, y.prototype.setSelectedIndex = function(t2, e2) {
            void 0 === e2 && (e2 = {}), this.isIndexValid(t2) && (this.isCheckboxList ? this.setCheckboxAtIndex(t2, e2) : this.isRadioList ? this.setRadioAtIndex(t2, e2) : this.setSingleSelectionAtIndex(t2, e2));
          }, y.prototype.handleFocusIn = function(t2) {
            0 <= t2 && (this.focusedItemIndex = t2, this.adapter.setAttributeForElementIndex(t2, "tabindex", "0"), this.adapter.setTabIndexForListItemChildren(t2, "0"));
          }, y.prototype.handleFocusOut = function(t2) {
            var e2 = this;
            0 <= t2 && (this.adapter.setAttributeForElementIndex(t2, "tabindex", "-1"), this.adapter.setTabIndexForListItemChildren(t2, "-1")), setTimeout(function() {
              e2.adapter.isFocusInsideList() || e2.setTabindexToFirstSelectedOrFocusedItem();
            }, 0);
          }, y.prototype.isIndexDisabled = function(t2) {
            return this.adapter.listItemAtIndexHasClass(t2, T.cssClasses.LIST_ITEM_DISABLED_CLASS);
          }, y.prototype.handleKeydown = function(t2, e2, n3) {
            var i4, r3 = this, o2 = "ArrowLeft" === v.normalizeKey(t2), s2 = "ArrowUp" === v.normalizeKey(t2), a2 = "ArrowRight" === v.normalizeKey(t2), c2 = "ArrowDown" === v.normalizeKey(t2), u2 = "Home" === v.normalizeKey(t2), l2 = "End" === v.normalizeKey(t2), d2 = "Enter" === v.normalizeKey(t2), p3 = "Spacebar" === v.normalizeKey(t2), h7 = this.isVertical && c2 || !this.isVertical && a2, f2 = this.isVertical && s2 || !this.isVertical && o2, y2 = "A" === t2.key || "a" === t2.key, C = O(t2);
            if (this.adapter.isRootFocused()) {
              if ((f2 || l2) && C([]) ? (t2.preventDefault(), this.focusLastElement()) : (h7 || u2) && C([]) ? (t2.preventDefault(), this.focusFirstElement()) : f2 && C(["Shift"]) && this.isCheckboxList ? (t2.preventDefault(), -1 !== (_ = this.focusLastElement()) && this.setSelectedIndexOnAction(_, false)) : h7 && C(["Shift"]) && this.isCheckboxList && (t2.preventDefault(), -1 !== (_ = this.focusFirstElement()) && this.setSelectedIndexOnAction(_, false)), this.hasTypeahead) {
                var E = { event: t2, focusItemAtIndex: function(t3) {
                  r3.focusItemAtIndex(t3);
                }, focusedItemIndex: -1, isTargetListItem: e2, sortedIndexByFirstChar: this.sortedIndexByFirstChar, isItemAtIndexDisabled: function(t3) {
                  return r3.isIndexDisabled(t3);
                } };
                A.handleKeydown(E, this.typeaheadState);
              }
            } else {
              var g = this.adapter.getFocusedElementIndex();
              if (!(-1 === g && (g = n3) < 0)) {
                if (h7 && C([])) b.preventDefaultEvent(t2), this.focusNextElement(g);
                else if (f2 && C([])) b.preventDefaultEvent(t2), this.focusPrevElement(g);
                else if (h7 && C(["Shift"]) && this.isCheckboxList) b.preventDefaultEvent(t2), -1 !== (_ = this.focusNextElement(g)) && this.setSelectedIndexOnAction(_, false);
                else if (f2 && C(["Shift"]) && this.isCheckboxList) {
                  var _;
                  b.preventDefaultEvent(t2), -1 !== (_ = this.focusPrevElement(g)) && this.setSelectedIndexOnAction(_, false);
                } else if (u2 && C([])) b.preventDefaultEvent(t2), this.focusFirstElement();
                else if (l2 && C([])) b.preventDefaultEvent(t2), this.focusLastElement();
                else if (u2 && C(["Control", "Shift"]) && this.isCheckboxList) {
                  if (b.preventDefaultEvent(t2), this.isIndexDisabled(g)) return;
                  this.focusFirstElement(), this.toggleCheckboxRange(0, g, g);
                } else if (l2 && C(["Control", "Shift"]) && this.isCheckboxList) {
                  if (b.preventDefaultEvent(t2), this.isIndexDisabled(g)) return;
                  this.focusLastElement(), this.toggleCheckboxRange(g, this.adapter.getListItemCount() - 1, g);
                } else if (y2 && C(["Control"]) && this.isCheckboxList) t2.preventDefault(), this.checkboxListToggleAll(this.selectedIndex === T.numbers.UNSET_INDEX ? [] : this.selectedIndex, true);
                else if ((d2 || p3) && C([])) {
                  if (e2) {
                    if ((m = t2.target) && "A" === m.tagName && d2) return;
                    if (b.preventDefaultEvent(t2), this.isIndexDisabled(g)) return;
                    this.isTypeaheadInProgress() || (this.isSelectableList() && this.setSelectedIndexOnAction(g, false), this.adapter.notifyAction(g));
                  }
                } else if ((d2 || p3) && C(["Shift"]) && this.isCheckboxList) {
                  var m;
                  if ((m = t2.target) && "A" === m.tagName && d2) return;
                  if (b.preventDefaultEvent(t2), this.isIndexDisabled(g)) return;
                  this.isTypeaheadInProgress() || (this.toggleCheckboxRange(null !== (i4 = this.lastSelectedIndex) && void 0 !== i4 ? i4 : g, g, g), this.adapter.notifyAction(g));
                }
                this.hasTypeahead && (E = { event: t2, focusItemAtIndex: function(t3) {
                  r3.focusItemAtIndex(t3);
                }, focusedItemIndex: this.focusedItemIndex, isTargetListItem: e2, sortedIndexByFirstChar: this.sortedIndexByFirstChar, isItemAtIndexDisabled: function(t3) {
                  return r3.isIndexDisabled(t3);
                } }, A.handleKeydown(E, this.typeaheadState));
              }
            }
          }, y.prototype.handleClick = function(t2, e2, n3) {
            var i4, r3 = O(n3);
            t2 !== T.numbers.UNSET_INDEX && (this.isIndexDisabled(t2) || (r3([]) ? (this.isSelectableList() && this.setSelectedIndexOnAction(t2, e2), this.adapter.notifyAction(t2)) : this.isCheckboxList && r3(["Shift"]) && (this.toggleCheckboxRange(null !== (i4 = this.lastSelectedIndex) && void 0 !== i4 ? i4 : t2, t2, t2), this.adapter.notifyAction(t2))));
          }, y.prototype.focusNextElement = function(t2) {
            var e2 = this.adapter.getListItemCount(), n3 = t2, i4 = null;
            do {
              if (e2 <= ++n3) {
                if (!this.wrapFocus) return t2;
                n3 = 0;
              }
              if (n3 === i4) return -1;
              i4 = null != i4 ? i4 : n3;
            } while (!this.areDisabledItemsFocusable && this.isIndexDisabled(n3));
            return this.focusItemAtIndex(n3), n3;
          }, y.prototype.focusPrevElement = function(t2) {
            var e2 = this.adapter.getListItemCount(), n3 = t2, i4 = null;
            do {
              if (--n3 < 0) {
                if (!this.wrapFocus) return t2;
                n3 = e2 - 1;
              }
              if (n3 === i4) return -1;
              i4 = null != i4 ? i4 : n3;
            } while (!this.areDisabledItemsFocusable && this.isIndexDisabled(n3));
            return this.focusItemAtIndex(n3), n3;
          }, y.prototype.focusFirstElement = function() {
            return this.focusNextElement(-1);
          }, y.prototype.focusLastElement = function() {
            return this.focusPrevElement(this.adapter.getListItemCount());
          }, y.prototype.focusInitialElement = function() {
            var t2 = this.getFirstSelectedOrFocusedItemIndex();
            return this.focusItemAtIndex(t2), t2;
          }, y.prototype.setEnabled = function(t2, e2) {
            this.isIndexValid(t2, false) && (e2 ? (this.adapter.removeClassForElementIndex(t2, T.cssClasses.LIST_ITEM_DISABLED_CLASS), this.adapter.setAttributeForElementIndex(t2, T.strings.ARIA_DISABLED, "false")) : (this.adapter.addClassForElementIndex(t2, T.cssClasses.LIST_ITEM_DISABLED_CLASS), this.adapter.setAttributeForElementIndex(t2, T.strings.ARIA_DISABLED, "true")));
          }, y.prototype.setSingleSelectionAtIndex = function(t2, e2) {
            if (void 0 === e2 && (e2 = {}), this.selectedIndex !== t2 || e2.forceUpdate) {
              var n3 = T.cssClasses.LIST_ITEM_SELECTED_CLASS;
              this.useActivatedClass && (n3 = T.cssClasses.LIST_ITEM_ACTIVATED_CLASS), this.selectedIndex !== T.numbers.UNSET_INDEX && this.adapter.removeClassForElementIndex(this.selectedIndex, n3), this.setAriaForSingleSelectionAtIndex(t2), this.setTabindexAtIndex(t2), t2 !== T.numbers.UNSET_INDEX && this.adapter.addClassForElementIndex(t2, n3), this.selectedIndex = t2, e2.isUserInteraction && !e2.forceUpdate && this.adapter.notifySelectionChange([t2]);
            }
          }, y.prototype.setAriaForSingleSelectionAtIndex = function(t2) {
            this.selectedIndex === T.numbers.UNSET_INDEX && (this.ariaCurrentAttrValue = this.adapter.getAttributeForElementIndex(t2, T.strings.ARIA_CURRENT));
            var e2 = null !== this.ariaCurrentAttrValue, n3 = e2 ? T.strings.ARIA_CURRENT : T.strings.ARIA_SELECTED;
            if (this.selectedIndex !== T.numbers.UNSET_INDEX && this.adapter.setAttributeForElementIndex(this.selectedIndex, n3, "false"), t2 !== T.numbers.UNSET_INDEX) {
              var i4 = e2 ? this.ariaCurrentAttrValue : "true";
              this.adapter.setAttributeForElementIndex(t2, n3, i4);
            }
          }, y.prototype.getSelectionAttribute = function() {
            return this.useSelectedAttr ? T.strings.ARIA_SELECTED : T.strings.ARIA_CHECKED;
          }, y.prototype.setRadioAtIndex = function(t2, e2) {
            void 0 === e2 && (e2 = {});
            var n3 = this.getSelectionAttribute();
            this.adapter.setCheckedCheckboxOrRadioAtIndex(t2, true), this.selectedIndex === t2 && !e2.forceUpdate || (this.selectedIndex !== T.numbers.UNSET_INDEX && this.adapter.setAttributeForElementIndex(this.selectedIndex, n3, "false"), this.adapter.setAttributeForElementIndex(t2, n3, "true"), this.selectedIndex = t2, e2.isUserInteraction && !e2.forceUpdate && this.adapter.notifySelectionChange([t2]));
          }, y.prototype.setCheckboxAtIndex = function(t2, e2) {
            void 0 === e2 && (e2 = {});
            for (var n3 = this.selectedIndex, i4 = e2.isUserInteraction ? new Set(n3 === T.numbers.UNSET_INDEX ? [] : n3) : null, r3 = this.getSelectionAttribute(), o2 = [], s2 = 0; s2 < this.adapter.getListItemCount(); s2++) {
              var a2 = null == i4 ? void 0 : i4.has(s2), c2 = 0 <= t2.indexOf(s2);
              c2 !== a2 && o2.push(s2), this.adapter.setCheckedCheckboxOrRadioAtIndex(s2, c2), this.adapter.setAttributeForElementIndex(s2, r3, c2 ? "true" : "false");
            }
            this.selectedIndex = t2, e2.isUserInteraction && o2.length && this.adapter.notifySelectionChange(o2);
          }, y.prototype.toggleCheckboxRange = function(t2, e2, n3) {
            this.lastSelectedIndex = n3;
            for (var i4 = new Set(this.selectedIndex === T.numbers.UNSET_INDEX ? [] : this.selectedIndex), r3 = !(null == i4 ? void 0 : i4.has(n3)), o2 = d([t2, e2].sort(), 2), s2 = o2[0], a2 = o2[1], c2 = this.getSelectionAttribute(), u2 = [], l2 = s2; l2 <= a2; l2++) this.isIndexDisabled(l2) || r3 !== i4.has(l2) && (u2.push(l2), this.adapter.setCheckedCheckboxOrRadioAtIndex(l2, r3), this.adapter.setAttributeForElementIndex(l2, c2, "" + r3), r3 ? i4.add(l2) : i4.delete(l2));
            u2.length && (this.selectedIndex = p2([], d(i4)), this.adapter.notifySelectionChange(u2));
          }, y.prototype.setTabindexAtIndex = function(t2) {
            this.focusedItemIndex === T.numbers.UNSET_INDEX && 0 !== t2 ? this.adapter.setAttributeForElementIndex(0, "tabindex", "-1") : 0 <= this.focusedItemIndex && this.focusedItemIndex !== t2 && this.adapter.setAttributeForElementIndex(this.focusedItemIndex, "tabindex", "-1"), this.selectedIndex instanceof Array || this.selectedIndex === t2 || this.adapter.setAttributeForElementIndex(this.selectedIndex, "tabindex", "-1"), t2 !== T.numbers.UNSET_INDEX && this.adapter.setAttributeForElementIndex(t2, "tabindex", "0");
          }, y.prototype.isSelectableList = function() {
            return this.isSingleSelectionList || this.isCheckboxList || this.isRadioList;
          }, y.prototype.setTabindexToFirstSelectedOrFocusedItem = function() {
            var t2 = this.getFirstSelectedOrFocusedItemIndex();
            this.setTabindexAtIndex(t2);
          }, y.prototype.getFirstSelectedOrFocusedItemIndex = function() {
            return this.isSelectableList() ? "number" == typeof this.selectedIndex && this.selectedIndex !== T.numbers.UNSET_INDEX ? this.selectedIndex : function(t2) {
              return t2 instanceof Array;
            }(this.selectedIndex) && 0 < this.selectedIndex.length ? this.selectedIndex.reduce(function(t2, e2) {
              return Math.min(t2, e2);
            }) : 0 : Math.max(this.focusedItemIndex, 0);
          }, y.prototype.isIndexValid = function(t2, e2) {
            var n3 = this;
            if (void 0 === e2 && (e2 = true), t2 instanceof Array) {
              if (!this.isCheckboxList && e2) throw new Error("MDCListFoundation: Array of index is only supported for checkbox based list");
              return 0 === t2.length || t2.some(function(t3) {
                return n3.isIndexInRange(t3);
              });
            }
            if ("number" != typeof t2) return false;
            if (this.isCheckboxList && e2) throw new Error("MDCListFoundation: Expected array of index for checkbox based list but got number: " + t2);
            return this.isIndexInRange(t2) || this.isSingleSelectionList && t2 === T.numbers.UNSET_INDEX;
          }, y.prototype.isIndexInRange = function(t2) {
            var e2 = this.adapter.getListItemCount();
            return 0 <= t2 && t2 < e2;
          }, y.prototype.setSelectedIndexOnAction = function(t2, e2) {
            this.lastSelectedIndex = t2, this.isCheckboxList ? (this.toggleCheckboxAtIndex(t2, e2), this.adapter.notifySelectionChange([t2])) : this.setSelectedIndex(t2, { isUserInteraction: true });
          }, y.prototype.toggleCheckboxAtIndex = function(e2, t2) {
            var n3, i4 = this.getSelectionAttribute(), r3 = this.adapter.isCheckboxCheckedAtIndex(e2);
            t2 ? n3 = r3 : (n3 = !r3, this.adapter.setCheckedCheckboxOrRadioAtIndex(e2, n3)), this.adapter.setAttributeForElementIndex(e2, i4, n3 ? "true" : "false");
            var o2 = this.selectedIndex === T.numbers.UNSET_INDEX ? [] : this.selectedIndex.slice();
            n3 ? o2.push(e2) : o2 = o2.filter(function(t3) {
              return t3 !== e2;
            }), this.selectedIndex = o2;
          }, y.prototype.focusItemAtIndex = function(t2) {
            this.adapter.focusItemAtIndex(t2), this.focusedItemIndex = t2;
          }, y.prototype.checkboxListToggleAll = function(t2, e2) {
            var n3 = this.adapter.getListItemCount();
            if (t2.length === n3) this.setCheckboxAtIndex([], { isUserInteraction: e2 });
            else {
              for (var i4 = [], r3 = 0; r3 < n3; r3++) (!this.isIndexDisabled(r3) || -1 < t2.indexOf(r3)) && i4.push(r3);
              this.setCheckboxAtIndex(i4, { isUserInteraction: e2 });
            }
          }, y.prototype.typeaheadMatchItem = function(t2, e2, n3) {
            var i4 = this;
            void 0 === n3 && (n3 = false);
            var r3 = { focusItemAtIndex: function(t3) {
              i4.focusItemAtIndex(t3);
            }, focusedItemIndex: e2 || this.focusedItemIndex, nextChar: t2, sortedIndexByFirstChar: this.sortedIndexByFirstChar, skipFocus: n3, isItemAtIndexDisabled: function(t3) {
              return i4.isIndexDisabled(t3);
            } };
            return A.matchItem(r3, this.typeaheadState);
          }, y.prototype.typeaheadInitSortedIndex = function() {
            return A.initSortedIndex(this.adapter.getListItemCount(), this.adapter.getPrimaryTextAtIndex);
          }, y.prototype.clearTypeaheadBuffer = function() {
            A.clearBuffer(this.typeaheadState);
          }, y);
          function y(t2) {
            var e2 = h.call(this, o(o({}, y.defaultAdapter), t2)) || this;
            return e2.wrapFocus = false, e2.isVertical = true, e2.isSingleSelectionList = false, e2.areDisabledItemsFocusable = true, e2.selectedIndex = T.numbers.UNSET_INDEX, e2.focusedItemIndex = T.numbers.UNSET_INDEX, e2.useActivatedClass = false, e2.useSelectedAttr = false, e2.ariaCurrentAttrValue = null, e2.isCheckboxList = false, e2.isRadioList = false, e2.lastSelectedIndex = null, e2.hasTypeahead = false, e2.typeaheadState = A.initState(), e2.sortedIndexByFirstChar = /* @__PURE__ */ new Map(), e2;
          }
          e.MDCListFoundation = f, e.default = f;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCDismissibleDrawerFoundation = void 0;
          var s, a = n2(0), c = n2(65), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return false;
            }, elementHasClass: function() {
              return false;
            }, notifyClose: function() {
            }, notifyOpen: function() {
            }, saveFocus: function() {
            }, restoreFocus: function() {
            }, focusActiveNavigationItem: function() {
            }, trapFocus: function() {
            }, releaseFocus: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.destroy = function() {
            this.animationFrame && cancelAnimationFrame(this.animationFrame), this.animationTimer && clearTimeout(this.animationTimer);
          }, l.prototype.open = function() {
            var t2 = this;
            this.isOpen() || this.isOpening() || this.isClosing() || (this.adapter.addClass(c.cssClasses.OPEN), this.adapter.addClass(c.cssClasses.ANIMATE), this.runNextAnimationFrame(function() {
              t2.adapter.addClass(c.cssClasses.OPENING);
            }), this.adapter.saveFocus());
          }, l.prototype.close = function() {
            !this.isOpen() || this.isOpening() || this.isClosing() || this.adapter.addClass(c.cssClasses.CLOSING);
          }, l.prototype.isOpen = function() {
            return this.adapter.hasClass(c.cssClasses.OPEN);
          }, l.prototype.isOpening = function() {
            return this.adapter.hasClass(c.cssClasses.OPENING) || this.adapter.hasClass(c.cssClasses.ANIMATE);
          }, l.prototype.isClosing = function() {
            return this.adapter.hasClass(c.cssClasses.CLOSING);
          }, l.prototype.handleKeydown = function(t2) {
            var e2 = t2.keyCode;
            "Escape" !== t2.key && 27 !== e2 || this.close();
          }, l.prototype.handleTransitionEnd = function(t2) {
            var e2 = c.cssClasses.OPENING, n3 = c.cssClasses.CLOSING, i4 = c.cssClasses.OPEN, r3 = c.cssClasses.ANIMATE, o2 = c.cssClasses.ROOT;
            this.isElement(t2.target) && this.adapter.elementHasClass(t2.target, o2) && (this.isClosing() ? (this.adapter.removeClass(i4), this.closed(), this.adapter.restoreFocus(), this.adapter.notifyClose()) : (this.adapter.focusActiveNavigationItem(), this.opened(), this.adapter.notifyOpen()), this.adapter.removeClass(r3), this.adapter.removeClass(e2), this.adapter.removeClass(n3));
          }, l.prototype.opened = function() {
          }, l.prototype.closed = function() {
          }, l.prototype.runNextAnimationFrame = function(t2) {
            var e2 = this;
            cancelAnimationFrame(this.animationFrame), this.animationFrame = requestAnimationFrame(function() {
              e2.animationFrame = 0, clearTimeout(e2.animationTimer), e2.animationTimer = setTimeout(t2, 0);
            });
          }, l.prototype.isElement = function(t2) {
            return Boolean(t2.classList);
          }, l);
          function l(t2) {
            var e2 = s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
            return e2.animationFrame = 0, e2.animationTimer = 0, e2;
          }
          e.MDCDismissibleDrawerFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCFloatingLabel = void 0;
          var o, s = n2(1), a = n2(3), c = n2(28), u = (o = s.MDCComponent, r2(l, o), l.attachTo = function(t2) {
            return new l(t2);
          }, l.prototype.shake = function(t2) {
            this.foundation.shake(t2);
          }, l.prototype.float = function(t2) {
            this.foundation.float(t2);
          }, l.prototype.setRequired = function(t2) {
            this.foundation.setRequired(t2);
          }, l.prototype.getWidth = function() {
            return this.foundation.getWidth();
          }, l.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, getWidth: function() {
              return a.estimateScrollWidth(n3.root);
            }, registerInteractionHandler: function(t3, e2) {
              return n3.listen(t3, e2);
            }, deregisterInteractionHandler: function(t3, e2) {
              return n3.unlisten(t3, e2);
            } };
            return new c.MDCFloatingLabelFoundation(t2);
          }, l);
          function l() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCFloatingLabel = u;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCFloatingLabelFoundation = void 0;
          var s, a = n2(0), c = n2(67), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, getWidth: function() {
              return 0;
            }, registerInteractionHandler: function() {
            }, deregisterInteractionHandler: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.init = function() {
            this.adapter.registerInteractionHandler("animationend", this.shakeAnimationEndHandler);
          }, l.prototype.destroy = function() {
            this.adapter.deregisterInteractionHandler("animationend", this.shakeAnimationEndHandler);
          }, l.prototype.getWidth = function() {
            return this.adapter.getWidth();
          }, l.prototype.shake = function(t2) {
            var e2 = l.cssClasses.LABEL_SHAKE;
            t2 ? this.adapter.addClass(e2) : this.adapter.removeClass(e2);
          }, l.prototype.float = function(t2) {
            var e2 = l.cssClasses, n3 = e2.LABEL_FLOAT_ABOVE, i4 = e2.LABEL_SHAKE;
            t2 ? this.adapter.addClass(n3) : (this.adapter.removeClass(n3), this.adapter.removeClass(i4));
          }, l.prototype.setRequired = function(t2) {
            var e2 = l.cssClasses.LABEL_REQUIRED;
            t2 ? this.adapter.addClass(e2) : this.adapter.removeClass(e2);
          }, l.prototype.handleShakeAnimationEnd = function() {
            var t2 = l.cssClasses.LABEL_SHAKE;
            this.adapter.removeClass(t2);
          }, l);
          function l(t2) {
            var e2 = s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
            return e2.shakeAnimationEndHandler = function() {
              e2.handleShakeAnimationEnd();
            }, e2;
          }
          e.MDCFloatingLabelFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCLineRipple = void 0;
          var o, s = n2(1), a = n2(72), c = (o = s.MDCComponent, r2(u, o), u.attachTo = function(t2) {
            return new u(t2);
          }, u.prototype.activate = function() {
            this.foundation.activate();
          }, u.prototype.deactivate = function() {
            this.foundation.deactivate();
          }, u.prototype.setRippleCenter = function(t2) {
            this.foundation.setRippleCenter(t2);
          }, u.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, setStyle: function(t3, e2) {
              return n3.root.style.setProperty(t3, e2);
            }, registerEventHandler: function(t3, e2) {
              return n3.listen(t3, e2);
            }, deregisterEventHandler: function(t3, e2) {
              return n3.unlisten(t3, e2);
            } };
            return new a.MDCLineRippleFoundation(t2);
          }, u);
          function u() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCLineRipple = c;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCNotchedOutline = void 0;
          var o, s = n2(1), a = n2(28), c = n2(31), u = n2(77), l = (o = s.MDCComponent, r2(d, o), d.attachTo = function(t2) {
            return new d(t2);
          }, d.prototype.initialSyncWithDOM = function() {
            this.notchElement = this.root.querySelector(c.strings.NOTCH_ELEMENT_SELECTOR);
            var t2 = this.root.querySelector("." + a.MDCFloatingLabelFoundation.cssClasses.ROOT);
            t2 ? (t2.style.transitionDuration = "0s", this.root.classList.add(c.cssClasses.OUTLINE_UPGRADED), requestAnimationFrame(function() {
              t2.style.transitionDuration = "";
            })) : this.root.classList.add(c.cssClasses.NO_LABEL);
          }, d.prototype.notch = function(t2) {
            this.foundation.notch(t2);
          }, d.prototype.closeNotch = function() {
            this.foundation.closeNotch();
          }, d.prototype.getDefaultFoundation = function() {
            var e2 = this, t2 = { addClass: function(t3) {
              return e2.root.classList.add(t3);
            }, removeClass: function(t3) {
              return e2.root.classList.remove(t3);
            }, setNotchWidthProperty: function(t3) {
              e2.notchElement.style.setProperty("width", t3 + "px");
            }, removeNotchWidthProperty: function() {
              e2.notchElement.style.removeProperty("width");
            } };
            return new u.MDCNotchedOutlineFoundation(t2);
          }, d);
          function d() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCNotchedOutline = l;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.numbers = e.cssClasses = void 0;
          e.strings = { NOTCH_ELEMENT_SELECTOR: ".mdc-notched-outline__notch" };
          e.numbers = { NOTCH_ELEMENT_PADDING: 8 };
          e.cssClasses = { NO_LABEL: "mdc-notched-outline--no-label", OUTLINE_NOTCHED: "mdc-notched-outline--notched", OUTLINE_UPGRADED: "mdc-notched-outline--upgraded" };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.numbers = e.strings = e.cssClasses = void 0;
          e.cssClasses = { ACTIVATED: "mdc-select--activated", DISABLED: "mdc-select--disabled", FOCUSED: "mdc-select--focused", INVALID: "mdc-select--invalid", MENU_INVALID: "mdc-select__menu--invalid", OUTLINED: "mdc-select--outlined", REQUIRED: "mdc-select--required", ROOT: "mdc-select", WITH_LEADING_ICON: "mdc-select--with-leading-icon" };
          e.strings = { ARIA_CONTROLS: "aria-controls", ARIA_DESCRIBEDBY: "aria-describedby", ARIA_SELECTED_ATTR: "aria-selected", CHANGE_EVENT: "MDCSelect:change", HIDDEN_INPUT_SELECTOR: 'input[type="hidden"]', LABEL_SELECTOR: ".mdc-floating-label", LEADING_ICON_SELECTOR: ".mdc-select__icon", LINE_RIPPLE_SELECTOR: ".mdc-line-ripple", MENU_SELECTOR: ".mdc-select__menu", OUTLINE_SELECTOR: ".mdc-notched-outline", SELECTED_TEXT_SELECTOR: ".mdc-select__selected-text", SELECT_ANCHOR_SELECTOR: ".mdc-select__anchor", VALUE_ATTR: "data-value" };
          e.numbers = { LABEL_SCALE: 0.75, UNSET_INDEX: -1, CLICK_DEBOUNCE_TIMEOUT_MS: 330 };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.events = e.attributes = e.numbers = e.cssClasses = void 0, e.cssClasses = { DISABLED: "mdc-slider--disabled", DISCRETE: "mdc-slider--discrete", INPUT: "mdc-slider__input", RANGE: "mdc-slider--range", THUMB: "mdc-slider__thumb", THUMB_FOCUSED: "mdc-slider__thumb--focused", THUMB_KNOB: "mdc-slider__thumb-knob", THUMB_TOP: "mdc-slider__thumb--top", THUMB_WITH_INDICATOR: "mdc-slider__thumb--with-indicator", TICK_MARKS: "mdc-slider--tick-marks", TICK_MARKS_CONTAINER: "mdc-slider__tick-marks", TICK_MARK_ACTIVE: "mdc-slider__tick-mark--active", TICK_MARK_INACTIVE: "mdc-slider__tick-mark--inactive", TRACK: "mdc-slider__track", TRACK_ACTIVE: "mdc-slider__track--active_fill", VALUE_INDICATOR_CONTAINER: "mdc-slider__value-indicator-container", VALUE_INDICATOR_TEXT: "mdc-slider__value-indicator-text" }, e.numbers = { STEP_SIZE: 1, MIN_RANGE: 0, THUMB_UPDATE_MIN_PX: 5 }, e.attributes = { ARIA_VALUETEXT: "aria-valuetext", INPUT_DISABLED: "disabled", INPUT_MIN: "min", INPUT_MAX: "max", INPUT_VALUE: "value", INPUT_STEP: "step", DATA_MIN_RANGE: "data-min-range" }, e.events = { CHANGE: "MDCSlider:change", INPUT: "MDCSlider:input" }, e.strings = { VAR_VALUE_INDICATOR_CARET_LEFT: "--slider-value-indicator-caret-left", VAR_VALUE_INDICATOR_CARET_RIGHT: "--slider-value-indicator-caret-right", VAR_VALUE_INDICATOR_CARET_TRANSFORM: "--slider-value-indicator-caret-transform", VAR_VALUE_INDICATOR_CONTAINER_LEFT: "--slider-value-indicator-container-left", VAR_VALUE_INDICATOR_CONTAINER_RIGHT: "--slider-value-indicator-container-right", VAR_VALUE_INDICATOR_CONTAINER_TRANSFORM: "--slider-value-indicator-container-transform" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2;
          Object.defineProperty(e, "__esModule", { value: true }), e.Thumb = e.TickMark = void 0, (i3 = e.TickMark || (e.TickMark = {}))[i3.ACTIVE = 0] = "ACTIVE", i3[i3.INACTIVE = 1] = "INACTIVE", (r2 = e.Thumb || (e.Thumb = {}))[r2.START = 1] = "START", r2[r2.END = 2] = "END";
        }, function(t, e, n2) {
          "use strict";
          var i3;
          Object.defineProperty(e, "__esModule", { value: true }), e.Selectors = e.CssClasses = void 0, (i3 = e.CssClasses || (e.CssClasses = {})).PROCESSING = "mdc-switch--processing", i3.SELECTED = "mdc-switch--selected", i3.UNSELECTED = "mdc-switch--unselected", (e.Selectors || (e.Selectors = {})).RIPPLE = ".mdc-switch__ripple";
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.cssClasses = void 0;
          e.cssClasses = { ANIMATING: "mdc-tab-scroller--animating", SCROLL_AREA_SCROLL: "mdc-tab-scroller__scroll-area--scroll", SCROLL_TEST: "mdc-tab-scroller__test" };
          e.strings = { AREA_SELECTOR: ".mdc-tab-scroller__scroll-area", CONTENT_SELECTOR: ".mdc-tab-scroller__scroll-content" };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabScrollerRTL = void 0;
          function i3(t2) {
            this.adapter = t2;
          }
          e.MDCTabScrollerRTL = i3, e.default = i3;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabFoundation = void 0;
          var s, a = n2(0), c = n2(106), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return false;
            }, setAttr: function() {
            }, activateIndicator: function() {
            }, deactivateIndicator: function() {
            }, notifyInteracted: function() {
            }, getOffsetLeft: function() {
              return 0;
            }, getOffsetWidth: function() {
              return 0;
            }, getContentOffsetLeft: function() {
              return 0;
            }, getContentOffsetWidth: function() {
              return 0;
            }, focus: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.handleClick = function() {
            this.adapter.notifyInteracted();
          }, l.prototype.isActive = function() {
            return this.adapter.hasClass(c.cssClasses.ACTIVE);
          }, l.prototype.setFocusOnActivate = function(t2) {
            this.focusOnActivate = t2;
          }, l.prototype.activate = function(t2) {
            this.adapter.addClass(c.cssClasses.ACTIVE), this.adapter.setAttr(c.strings.ARIA_SELECTED, "true"), this.adapter.setAttr(c.strings.TABINDEX, "0"), this.adapter.activateIndicator(t2), this.focusOnActivate && this.adapter.focus();
          }, l.prototype.deactivate = function() {
            this.isActive() && (this.adapter.removeClass(c.cssClasses.ACTIVE), this.adapter.setAttr(c.strings.ARIA_SELECTED, "false"), this.adapter.setAttr(c.strings.TABINDEX, "-1"), this.adapter.deactivateIndicator());
          }, l.prototype.computeDimensions = function() {
            var t2 = this.adapter.getOffsetWidth(), e2 = this.adapter.getOffsetLeft(), n3 = this.adapter.getContentOffsetWidth(), i4 = this.adapter.getContentOffsetLeft();
            return { contentLeft: e2 + i4, contentRight: e2 + i4 + n3, rootLeft: e2, rootRight: e2 + t2 };
          }, l);
          function l(t2) {
            var e2 = s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
            return e2.focusOnActivate = true, e2;
          }
          e.MDCTabFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTextFieldCharacterCounterFoundation = void 0;
          var s, a = n2(0), c = n2(110), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { setContent: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.setCounterValue = function(t2, e2) {
            t2 = Math.min(t2, e2), this.adapter.setContent(t2 + " / " + e2);
          }, l);
          function l(t2) {
            return s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
          }
          e.MDCTextFieldCharacterCounterFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.ALWAYS_FLOAT_TYPES = e.VALIDATION_ATTR_WHITELIST = e.numbers = e.strings = e.cssClasses = void 0;
          e.strings = { ARIA_CONTROLS: "aria-controls", ARIA_DESCRIBEDBY: "aria-describedby", INPUT_SELECTOR: ".mdc-text-field__input", LABEL_SELECTOR: ".mdc-floating-label", LEADING_ICON_SELECTOR: ".mdc-text-field__icon--leading", LINE_RIPPLE_SELECTOR: ".mdc-line-ripple", OUTLINE_SELECTOR: ".mdc-notched-outline", PREFIX_SELECTOR: ".mdc-text-field__affix--prefix", SUFFIX_SELECTOR: ".mdc-text-field__affix--suffix", TRAILING_ICON_SELECTOR: ".mdc-text-field__icon--trailing" };
          e.cssClasses = { DISABLED: "mdc-text-field--disabled", FOCUSED: "mdc-text-field--focused", HELPER_LINE: "mdc-text-field-helper-line", INVALID: "mdc-text-field--invalid", LABEL_FLOATING: "mdc-text-field--label-floating", NO_LABEL: "mdc-text-field--no-label", OUTLINED: "mdc-text-field--outlined", ROOT: "mdc-text-field", TEXTAREA: "mdc-text-field--textarea", WITH_LEADING_ICON: "mdc-text-field--with-leading-icon", WITH_TRAILING_ICON: "mdc-text-field--with-trailing-icon", WITH_INTERNAL_COUNTER: "mdc-text-field--with-internal-counter" };
          e.numbers = { LABEL_SCALE: 0.75 };
          e.VALIDATION_ATTR_WHITELIST = ["pattern", "min", "max", "required", "step", "minlength", "maxlength"];
          e.ALWAYS_FLOAT_TYPES = ["color", "date", "datetime-local", "month", "range", "time", "week"];
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTextFieldHelperTextFoundation = void 0;
          var s, a = n2(0), c = n2(113), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return false;
            }, getAttr: function() {
              return null;
            }, setAttr: function() {
            }, removeAttr: function() {
            }, setContent: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.getId = function() {
            return this.adapter.getAttr("id");
          }, l.prototype.isVisible = function() {
            return "true" !== this.adapter.getAttr(c.strings.ARIA_HIDDEN);
          }, l.prototype.setContent = function(t2) {
            this.adapter.setContent(t2);
          }, l.prototype.isPersistent = function() {
            return this.adapter.hasClass(c.cssClasses.HELPER_TEXT_PERSISTENT);
          }, l.prototype.setPersistent = function(t2) {
            t2 ? this.adapter.addClass(c.cssClasses.HELPER_TEXT_PERSISTENT) : this.adapter.removeClass(c.cssClasses.HELPER_TEXT_PERSISTENT);
          }, l.prototype.isValidation = function() {
            return this.adapter.hasClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG);
          }, l.prototype.setValidation = function(t2) {
            t2 ? this.adapter.addClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG) : this.adapter.removeClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG);
          }, l.prototype.showToScreenReader = function() {
            this.adapter.removeAttr(c.strings.ARIA_HIDDEN);
          }, l.prototype.setValidity = function(t2) {
            var e2 = this.adapter.hasClass(c.cssClasses.HELPER_TEXT_PERSISTENT), n3 = this.adapter.hasClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG) && !t2;
            n3 ? (this.showToScreenReader(), "alert" === this.adapter.getAttr(c.strings.ROLE) ? this.refreshAlertRole() : this.adapter.setAttr(c.strings.ROLE, "alert")) : this.adapter.removeAttr(c.strings.ROLE), e2 || n3 || this.hide();
          }, l.prototype.hide = function() {
            this.adapter.setAttr(c.strings.ARIA_HIDDEN, "true");
          }, l.prototype.refreshAlertRole = function() {
            var t2 = this;
            this.adapter.removeAttr(c.strings.ROLE), requestAnimationFrame(function() {
              t2.adapter.setAttr(c.strings.ROLE, "alert");
            });
          }, l);
          function l(t2) {
            return s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
          }
          e.MDCTextFieldHelperTextFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2;
          Object.defineProperty(e, "__esModule", { value: true }), e.XPositionWithCaret = e.YPositionWithCaret = e.PositionWithCaret = e.strings = e.YPosition = e.AnchorBoundaryType = e.XPosition = e.events = e.attributes = e.numbers = e.CssClasses = void 0, (r2 = i3 = i3 || {}).RICH = "mdc-tooltip--rich", r2.SHOWN = "mdc-tooltip--shown", r2.SHOWING = "mdc-tooltip--showing", r2.SHOWING_TRANSITION = "mdc-tooltip--showing-transition", r2.HIDE = "mdc-tooltip--hide", r2.HIDE_TRANSITION = "mdc-tooltip--hide-transition", r2.MULTILINE_TOOLTIP = "mdc-tooltip--multiline", r2.SURFACE = "mdc-tooltip__surface", r2.SURFACE_ANIMATION = "mdc-tooltip__surface-animation", r2.TOOLTIP_CARET_TOP = "mdc-tooltip__caret-surface-top", r2.TOOLTIP_CARET_BOTTOM = "mdc-tooltip__caret-surface-bottom", e.CssClasses = i3;
          e.numbers = { BOUNDED_ANCHOR_GAP: 4, UNBOUNDED_ANCHOR_GAP: 8, MIN_VIEWPORT_TOOLTIP_THRESHOLD: 8, HIDE_DELAY_MS: 600, SHOW_DELAY_MS: 500, MIN_HEIGHT: 24, MAX_WIDTH: 200, CARET_INDENTATION: 24, ANIMATION_SCALE: 0.8 };
          e.attributes = { ARIA_EXPANDED: "aria-expanded", ARIA_HASPOPUP: "aria-haspopup", PERSISTENT: "data-mdc-tooltip-persistent", SCROLLABLE_ANCESTOR: "tooltip-scrollable-ancestor", HAS_CARET: "data-mdc-tooltip-has-caret" };
          var o, s, a, c, u, l;
          e.events = { HIDDEN: "MDCTooltip:hidden" }, (s = o = o || {})[s.DETECTED = 0] = "DETECTED", s[s.START = 1] = "START", s[s.CENTER = 2] = "CENTER", s[s.END = 3] = "END", e.XPosition = o, (c = a = a || {})[c.DETECTED = 0] = "DETECTED", c[c.ABOVE = 1] = "ABOVE", c[c.BELOW = 2] = "BELOW", e.YPosition = a, (l = u = u || {})[l.BOUNDED = 0] = "BOUNDED", l[l.UNBOUNDED = 1] = "UNBOUNDED", e.AnchorBoundaryType = u;
          var d, p2, h, f, y, C;
          e.strings = { LEFT: "left", RIGHT: "right", CENTER: "center", TOP: "top", BOTTOM: "bottom" }, (p2 = d = d || {})[p2.DETECTED = 0] = "DETECTED", p2[p2.ABOVE_START = 1] = "ABOVE_START", p2[p2.ABOVE_CENTER = 2] = "ABOVE_CENTER", p2[p2.ABOVE_END = 3] = "ABOVE_END", p2[p2.TOP_SIDE_START = 4] = "TOP_SIDE_START", p2[p2.CENTER_SIDE_START = 5] = "CENTER_SIDE_START", p2[p2.BOTTOM_SIDE_START = 6] = "BOTTOM_SIDE_START", p2[p2.TOP_SIDE_END = 7] = "TOP_SIDE_END", p2[p2.CENTER_SIDE_END = 8] = "CENTER_SIDE_END", p2[p2.BOTTOM_SIDE_END = 9] = "BOTTOM_SIDE_END", p2[p2.BELOW_START = 10] = "BELOW_START", p2[p2.BELOW_CENTER = 11] = "BELOW_CENTER", p2[p2.BELOW_END = 12] = "BELOW_END", e.PositionWithCaret = d, (f = h = h || {})[f.ABOVE = 1] = "ABOVE", f[f.BELOW = 2] = "BELOW", f[f.SIDE_TOP = 3] = "SIDE_TOP", f[f.SIDE_CENTER = 4] = "SIDE_CENTER", f[f.SIDE_BOTTOM = 5] = "SIDE_BOTTOM", e.YPositionWithCaret = h, (C = y = y || {})[C.START = 1] = "START", C[C.CENTER = 2] = "CENTER", C[C.END = 3] = "END", C[C.SIDE_START = 4] = "SIDE_START", C[C.SIDE_END = 5] = "SIDE_END", e.XPositionWithCaret = y;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTopAppBarFoundation = void 0;
          var o, s = n2(9), a = n2(44), c = (o = a.MDCTopAppBarBaseFoundation, r2(u, o), u.prototype.destroy = function() {
            o.prototype.destroy.call(this), this.adapter.setStyle("top", "");
          }, u.prototype.handleTargetScroll = function() {
            var t2 = Math.max(this.adapter.getViewportScrollY(), 0), e2 = t2 - this.lastScrollPosition;
            this.lastScrollPosition = t2, this.isCurrentlyBeingResized || (this.currentAppBarOffsetTop -= e2, 0 < this.currentAppBarOffsetTop ? this.currentAppBarOffsetTop = 0 : Math.abs(this.currentAppBarOffsetTop) > this.topAppBarHeight && (this.currentAppBarOffsetTop = -this.topAppBarHeight), this.moveTopAppBar());
          }, u.prototype.handleWindowResize = function() {
            var t2 = this;
            this.resizeThrottleId || (this.resizeThrottleId = setTimeout(function() {
              t2.resizeThrottleId = 0, t2.throttledResizeHandler();
            }, s.numbers.DEBOUNCE_THROTTLE_RESIZE_TIME_MS)), this.isCurrentlyBeingResized = true, this.resizeDebounceId && clearTimeout(this.resizeDebounceId), this.resizeDebounceId = setTimeout(function() {
              t2.handleTargetScroll(), t2.isCurrentlyBeingResized = false, t2.resizeDebounceId = 0;
            }, s.numbers.DEBOUNCE_THROTTLE_RESIZE_TIME_MS);
          }, u.prototype.checkForUpdate = function() {
            var t2 = -this.topAppBarHeight, e2 = this.currentAppBarOffsetTop < 0, n3 = this.currentAppBarOffsetTop > t2, i4 = e2 && n3;
            if (i4) this.wasDocked = false;
            else {
              if (!this.wasDocked) return this.wasDocked = true;
              if (this.isDockedShowing !== n3) return this.isDockedShowing = n3, true;
            }
            return i4;
          }, u.prototype.moveTopAppBar = function() {
            if (this.checkForUpdate()) {
              var t2 = this.currentAppBarOffsetTop;
              Math.abs(t2) >= this.topAppBarHeight && (t2 = -s.numbers.MAX_TOP_APP_BAR_HEIGHT), this.adapter.setStyle("top", t2 + "px");
            }
          }, u.prototype.throttledResizeHandler = function() {
            var t2 = this.adapter.getTopAppBarHeight();
            this.topAppBarHeight !== t2 && (this.wasDocked = false, this.currentAppBarOffsetTop -= this.topAppBarHeight - t2, this.topAppBarHeight = t2), this.handleTargetScroll();
          }, u);
          function u(t2) {
            var e2 = o.call(this, t2) || this;
            return e2.wasDocked = true, e2.isDockedShowing = true, e2.currentAppBarOffsetTop = 0, e2.isCurrentlyBeingResized = false, e2.resizeThrottleId = 0, e2.resizeDebounceId = 0, e2.lastScrollPosition = e2.adapter.getViewportScrollY(), e2.topAppBarHeight = e2.adapter.getTopAppBarHeight(), e2;
          }
          e.MDCTopAppBarFoundation = c, e.default = c;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTopAppBarBaseFoundation = void 0;
          var s, a = n2(0), c = n2(9), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "numbers", { get: function() {
            return c.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return false;
            }, setStyle: function() {
            }, getTopAppBarHeight: function() {
              return 0;
            }, notifyNavigationIconClicked: function() {
            }, getViewportScrollY: function() {
              return 0;
            }, getTotalActionItems: function() {
              return 0;
            } };
          }, enumerable: false, configurable: true }), l.prototype.handleTargetScroll = function() {
          }, l.prototype.handleWindowResize = function() {
          }, l.prototype.handleNavigationClick = function() {
            this.adapter.notifyNavigationIconClicked();
          }, l);
          function l(t2) {
            return s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
          }
          e.MDCTopAppBarBaseFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCBannerFoundation = void 0;
          var s, a = n2(0), c = n2(18), u = c.cssClasses.OPENING, l = c.cssClasses.OPEN, d = c.cssClasses.CLOSING, p2 = (s = a.MDCFoundation, r2(h, s), Object.defineProperty(h, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, getContentHeight: function() {
              return 0;
            }, notifyClosed: function() {
            }, notifyClosing: function() {
            }, notifyOpened: function() {
            }, notifyOpening: function() {
            }, notifyActionClicked: function() {
            }, releaseFocus: function() {
            }, removeClass: function() {
            }, setStyleProperty: function() {
            }, trapFocus: function() {
            } };
          }, enumerable: false, configurable: true }), h.prototype.destroy = function() {
            cancelAnimationFrame(this.animationFrame), this.animationFrame = 0, clearTimeout(this.animationTimer), this.animationTimer = 0;
          }, h.prototype.open = function() {
            var t2 = this;
            this.isOpened = true, this.adapter.notifyOpening(), this.adapter.removeClass(d), this.adapter.addClass(u);
            var e2 = this.adapter.getContentHeight();
            this.animationFrame = requestAnimationFrame(function() {
              t2.adapter.addClass(l), t2.adapter.setStyleProperty("height", e2 + "px"), t2.animationTimer = setTimeout(function() {
                t2.handleAnimationTimerEnd(), t2.adapter.trapFocus(), t2.adapter.notifyOpened();
              }, c.numbers.BANNER_ANIMATION_OPEN_TIME_MS);
            });
          }, h.prototype.close = function(t2) {
            var e2 = this;
            this.isOpened && (cancelAnimationFrame(this.animationFrame), this.animationFrame = 0, this.isOpened = false, this.adapter.notifyClosing(t2), this.adapter.addClass(d), this.adapter.setStyleProperty("height", "0"), this.adapter.removeClass(l), this.adapter.removeClass(u), clearTimeout(this.animationTimer), this.animationTimer = setTimeout(function() {
              e2.adapter.releaseFocus(), e2.handleAnimationTimerEnd(), e2.adapter.notifyClosed(t2);
            }, c.numbers.BANNER_ANIMATION_CLOSE_TIME_MS));
          }, h.prototype.isOpen = function() {
            return this.isOpened;
          }, h.prototype.handlePrimaryActionClick = function(t2) {
            void 0 === t2 && (t2 = false), t2 ? this.adapter.notifyActionClicked(0) : this.close(c.CloseReason.PRIMARY);
          }, h.prototype.handleSecondaryActionClick = function(t2) {
            void 0 === t2 && (t2 = false), t2 ? this.adapter.notifyActionClicked(1) : this.close(c.CloseReason.SECONDARY);
          }, h.prototype.layout = function() {
            var t2 = this.adapter.getContentHeight();
            this.adapter.setStyleProperty("height", t2 + "px");
          }, h.prototype.handleAnimationTimerEnd = function() {
            this.animationTimer = 0, this.adapter.removeClass(u), this.adapter.removeClass(d);
          }, h);
          function h(t2) {
            var e2 = s.call(this, o(o({}, h.defaultAdapter), t2)) || this;
            return e2.isOpened = false, e2.animationFrame = 0, e2.animationTimer = 0, e2;
          }
          e.MDCBannerFoundation = p2;
        }, function(t, e, n2) {
          "use strict";
          var i3, u = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(t2) {
            return typeof t2;
          } : function(t2) {
            return t2 && "function" == typeof Symbol && t2.constructor === Symbol && t2 !== Symbol.prototype ? "symbol" : typeof t2;
          }, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, l = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCCheckbox = void 0;
          var s, a = n2(10), c = n2(1), d = n2(5), p2 = n2(3), h = n2(2), f = n2(4), y = n2(20), C = n2(48), E = ["checked", "indeterminate"], g = (s = c.MDCComponent, r2(_, s), _.attachTo = function(t2) {
            return new _(t2);
          }, Object.defineProperty(_.prototype, "ripple", { get: function() {
            return this.rippleSurface;
          }, enumerable: false, configurable: true }), Object.defineProperty(_.prototype, "checked", { get: function() {
            return this.getNativeControl().checked;
          }, set: function(t2) {
            this.getNativeControl().checked = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(_.prototype, "indeterminate", { get: function() {
            return this.getNativeControl().indeterminate;
          }, set: function(t2) {
            this.getNativeControl().indeterminate = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(_.prototype, "disabled", { get: function() {
            return this.getNativeControl().disabled;
          }, set: function(t2) {
            this.foundation.setDisabled(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(_.prototype, "value", { get: function() {
            return this.getNativeControl().value;
          }, set: function(t2) {
            this.getNativeControl().value = t2;
          }, enumerable: false, configurable: true }), _.prototype.initialize = function() {
            var t2 = y.strings.DATA_INDETERMINATE_ATTR;
            this.getNativeControl().indeterminate = "true" === this.getNativeControl().getAttribute(t2), this.getNativeControl().removeAttribute(t2);
          }, _.prototype.initialSyncWithDOM = function() {
            var t2 = this;
            this.handleChange = function() {
              t2.foundation.handleChange();
            }, this.handleAnimationEnd = function() {
              t2.foundation.handleAnimationEnd();
            }, this.getNativeControl().addEventListener("change", this.handleChange), this.listen(a.getCorrectEventName(window, "animationend"), this.handleAnimationEnd), this.installPropertyChangeHooks();
          }, _.prototype.destroy = function() {
            this.rippleSurface.destroy(), this.getNativeControl().removeEventListener("change", this.handleChange), this.unlisten(a.getCorrectEventName(window, "animationend"), this.handleAnimationEnd), this.uninstallPropertyChangeHooks(), s.prototype.destroy.call(this);
          }, _.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, forceLayout: function() {
              return n3.root.offsetWidth;
            }, hasNativeControl: function() {
              return !!n3.getNativeControl();
            }, isAttachedToDOM: function() {
              return Boolean(n3.root.parentNode);
            }, isChecked: function() {
              return n3.checked;
            }, isIndeterminate: function() {
              return n3.indeterminate;
            }, removeClass: function(t3) {
              n3.root.classList.remove(t3);
            }, removeNativeControlAttr: function(t3) {
              n3.getNativeControl().removeAttribute(t3);
            }, setNativeControlAttr: function(t3, e2) {
              n3.getNativeControl().setAttribute(t3, e2);
            }, setNativeControlDisabled: function(t3) {
              n3.getNativeControl().disabled = t3;
            } };
            return new C.MDCCheckboxFoundation(t2);
          }, _.prototype.createRipple = function() {
            var n3 = this, t2 = o(o({}, h.MDCRipple.createAdapter(this)), { deregisterInteractionHandler: function(t3, e2) {
              n3.getNativeControl().removeEventListener(t3, e2, d.applyPassive());
            }, isSurfaceActive: function() {
              return p2.matches(n3.getNativeControl(), ":active");
            }, isUnbounded: function() {
              return true;
            }, registerInteractionHandler: function(t3, e2) {
              n3.getNativeControl().addEventListener(t3, e2, d.applyPassive());
            } });
            return new h.MDCRipple(this.root, new f.MDCRippleFoundation(t2));
          }, _.prototype.installPropertyChangeHooks = function() {
            function t2(t3) {
              var e3 = Object.getOwnPropertyDescriptor(s2, t3);
              if (!m(e3)) return { value: void 0 };
              var n4 = e3.get, i5 = { configurable: e3.configurable, enumerable: e3.enumerable, get: n4, set: function(t4) {
                e3.set.call(o2, t4), r3.foundation.handleChange();
              } };
              Object.defineProperty(o2, t3, i5);
            }
            var e2, n3, r3 = this, o2 = this.getNativeControl(), s2 = Object.getPrototypeOf(o2);
            try {
              for (var i4 = l(E), a2 = i4.next(); !a2.done; a2 = i4.next()) {
                var c2 = t2(a2.value);
                if ("object" === (void 0 === c2 ? "undefined" : u(c2))) return c2.value;
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                a2 && !a2.done && (n3 = i4.return) && n3.call(i4);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, _.prototype.uninstallPropertyChangeHooks = function() {
            var e2, t2, n3 = this.getNativeControl(), i4 = Object.getPrototypeOf(n3);
            try {
              for (var r3 = l(E), o2 = r3.next(); !o2.done; o2 = r3.next()) {
                var s2 = o2.value, a2 = Object.getOwnPropertyDescriptor(i4, s2);
                if (!m(a2)) return;
                Object.defineProperty(n3, s2, a2);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                o2 && !o2.done && (t2 = r3.return) && t2.call(r3);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, _.prototype.getNativeControl = function() {
            var t2 = y.strings.NATIVE_CONTROL_SELECTOR, e2 = this.root.querySelector(t2);
            if (!e2) throw new Error("Checkbox component requires a " + t2 + " element");
            return e2;
          }, _);
          function _() {
            var t2 = null !== s && s.apply(this, arguments) || this;
            return t2.rippleSurface = t2.createRipple(), t2;
          }
          function m(t2) {
            return !!t2 && "function" == typeof t2.set;
          }
          e.MDCCheckbox = g;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.numbers = e.strings = e.cssClasses = void 0, e.cssClasses = { BG_FOCUSED: "mdc-ripple-upgraded--background-focused", FG_ACTIVATION: "mdc-ripple-upgraded--foreground-activation", FG_DEACTIVATION: "mdc-ripple-upgraded--foreground-deactivation", ROOT: "mdc-ripple-upgraded", UNBOUNDED: "mdc-ripple-upgraded--unbounded" }, e.strings = { VAR_FG_SCALE: "--mdc-ripple-fg-scale", VAR_FG_SIZE: "--mdc-ripple-fg-size", VAR_FG_TRANSLATE_END: "--mdc-ripple-fg-translate-end", VAR_FG_TRANSLATE_START: "--mdc-ripple-fg-translate-start", VAR_LEFT: "--mdc-ripple-left", VAR_TOP: "--mdc-ripple-top" }, e.numbers = { DEACTIVATION_TIMEOUT_MS: 225, FG_DEACTIVATION_MS: 150, INITIAL_ORIGIN_SCALE: 0.6, PADDING: 10, TAP_DELAY_MS: 300 };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCCheckboxFoundation = void 0;
          var s, a = n2(0), p2 = n2(20), c = (s = a.MDCFoundation, r2(h, s), Object.defineProperty(h, "cssClasses", { get: function() {
            return p2.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "strings", { get: function() {
            return p2.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "numbers", { get: function() {
            return p2.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, forceLayout: function() {
            }, hasNativeControl: function() {
              return false;
            }, isAttachedToDOM: function() {
              return false;
            }, isChecked: function() {
              return false;
            }, isIndeterminate: function() {
              return false;
            }, removeClass: function() {
            }, removeNativeControlAttr: function() {
            }, setNativeControlAttr: function() {
            }, setNativeControlDisabled: function() {
            } };
          }, enumerable: false, configurable: true }), h.prototype.init = function() {
            this.currentCheckState = this.determineCheckState(), this.updateAriaChecked(), this.adapter.addClass(p2.cssClasses.UPGRADED);
          }, h.prototype.destroy = function() {
            clearTimeout(this.animEndLatchTimer);
          }, h.prototype.setDisabled = function(t2) {
            this.adapter.setNativeControlDisabled(t2), t2 ? this.adapter.addClass(p2.cssClasses.DISABLED) : this.adapter.removeClass(p2.cssClasses.DISABLED);
          }, h.prototype.handleAnimationEnd = function() {
            var t2 = this;
            this.enableAnimationEndHandler && (clearTimeout(this.animEndLatchTimer), this.animEndLatchTimer = setTimeout(function() {
              t2.adapter.removeClass(t2.currentAnimationClass), t2.enableAnimationEndHandler = false;
            }, p2.numbers.ANIM_END_LATCH_MS));
          }, h.prototype.handleChange = function() {
            this.transitionCheckState();
          }, h.prototype.transitionCheckState = function() {
            if (this.adapter.hasNativeControl()) {
              var t2 = this.currentCheckState, e2 = this.determineCheckState();
              if (t2 !== e2) {
                this.updateAriaChecked();
                var n3 = p2.strings.TRANSITION_STATE_UNCHECKED, i4 = p2.cssClasses.SELECTED;
                e2 === n3 ? this.adapter.removeClass(i4) : this.adapter.addClass(i4), 0 < this.currentAnimationClass.length && (clearTimeout(this.animEndLatchTimer), this.adapter.forceLayout(), this.adapter.removeClass(this.currentAnimationClass)), this.currentAnimationClass = this.getTransitionAnimationClass(t2, e2), this.currentCheckState = e2, this.adapter.isAttachedToDOM() && 0 < this.currentAnimationClass.length && (this.adapter.addClass(this.currentAnimationClass), this.enableAnimationEndHandler = true);
              }
            }
          }, h.prototype.determineCheckState = function() {
            var t2 = p2.strings.TRANSITION_STATE_INDETERMINATE, e2 = p2.strings.TRANSITION_STATE_CHECKED, n3 = p2.strings.TRANSITION_STATE_UNCHECKED;
            return this.adapter.isIndeterminate() ? t2 : this.adapter.isChecked() ? e2 : n3;
          }, h.prototype.getTransitionAnimationClass = function(t2, e2) {
            var n3 = p2.strings.TRANSITION_STATE_INIT, i4 = p2.strings.TRANSITION_STATE_CHECKED, r3 = p2.strings.TRANSITION_STATE_UNCHECKED, o2 = h.cssClasses, s2 = o2.ANIM_UNCHECKED_CHECKED, a2 = o2.ANIM_UNCHECKED_INDETERMINATE, c2 = o2.ANIM_CHECKED_UNCHECKED, u = o2.ANIM_CHECKED_INDETERMINATE, l = o2.ANIM_INDETERMINATE_CHECKED, d = o2.ANIM_INDETERMINATE_UNCHECKED;
            switch (t2) {
              case n3:
                return e2 === r3 ? "" : e2 === i4 ? l : d;
              case r3:
                return e2 === i4 ? s2 : a2;
              case i4:
                return e2 === r3 ? c2 : u;
              default:
                return e2 === i4 ? l : d;
            }
          }, h.prototype.updateAriaChecked = function() {
            this.adapter.isIndeterminate() ? this.adapter.setNativeControlAttr(p2.strings.ARIA_CHECKED_ATTR, p2.strings.ARIA_CHECKED_INDETERMINATE_VALUE) : this.adapter.removeNativeControlAttr(p2.strings.ARIA_CHECKED_ATTR);
          }, h);
          function h(t2) {
            var e2 = s.call(this, o(o({}, h.defaultAdapter), t2)) || this;
            return e2.currentCheckState = p2.strings.TRANSITION_STATE_INIT, e2.currentAnimationClass = "", e2.animEndLatchTimer = 0, e2.enableAnimationEndHandler = false, e2;
          }
          e.MDCCheckboxFoundation = c, e.default = c;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCChipTrailingAction = void 0;
          var o, s = n2(1), a = n2(2), c = n2(4), u = n2(12), l = n2(50), d = (o = s.MDCComponent, r2(p2, o), Object.defineProperty(p2.prototype, "ripple", { get: function() {
            return this.rippleSurface;
          }, enumerable: false, configurable: true }), p2.attachTo = function(t2) {
            return new p2(t2);
          }, p2.prototype.initialize = function(t2) {
            void 0 === t2 && (t2 = function(t3, e3) {
              return new a.MDCRipple(t3, e3);
            });
            var e2 = a.MDCRipple.createAdapter(this);
            this.rippleSurface = t2(this.root, new c.MDCRippleFoundation(e2));
          }, p2.prototype.initialSyncWithDOM = function() {
            var e2 = this;
            this.handleClick = function(t2) {
              e2.foundation.handleClick(t2);
            }, this.handleKeydown = function(t2) {
              e2.foundation.handleKeydown(t2);
            }, this.listen("click", this.handleClick), this.listen("keydown", this.handleKeydown);
          }, p2.prototype.destroy = function() {
            this.rippleSurface.destroy(), this.unlisten("click", this.handleClick), this.unlisten("keydown", this.handleKeydown), o.prototype.destroy.call(this);
          }, p2.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { focus: function() {
              n3.root.focus();
            }, getAttribute: function(t3) {
              return n3.root.getAttribute(t3);
            }, notifyInteraction: function(t3) {
              return n3.emit(u.strings.INTERACTION_EVENT, { trigger: t3 }, true);
            }, notifyNavigation: function(t3) {
              n3.emit(u.strings.NAVIGATION_EVENT, { key: t3 }, true);
            }, setAttribute: function(t3, e2) {
              n3.root.setAttribute(t3, e2);
            } };
            return new l.MDCChipTrailingActionFoundation(t2);
          }, p2.prototype.isNavigable = function() {
            return this.foundation.isNavigable();
          }, p2.prototype.focus = function() {
            this.foundation.focus();
          }, p2.prototype.removeFocus = function() {
            this.foundation.removeFocus();
          }, p2);
          function p2() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCChipTrailingAction = d;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCChipTrailingActionFoundation = void 0;
          var s, a = n2(0), c = n2(6), u = n2(12), l = (s = a.MDCFoundation, r2(d, s), Object.defineProperty(d, "strings", { get: function() {
            return u.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(d, "defaultAdapter", { get: function() {
            return { focus: function() {
            }, getAttribute: function() {
              return null;
            }, setAttribute: function() {
            }, notifyInteraction: function() {
            }, notifyNavigation: function() {
            } };
          }, enumerable: false, configurable: true }), d.prototype.handleClick = function(t2) {
            t2.stopPropagation(), this.adapter.notifyInteraction(u.InteractionTrigger.CLICK);
          }, d.prototype.handleKeydown = function(t2) {
            t2.stopPropagation();
            var e2 = c.normalizeKey(t2);
            if (this.shouldNotifyInteractionFromKey(e2)) {
              var n3 = this.getTriggerFromKey(e2);
              this.adapter.notifyInteraction(n3);
            } else c.isNavigationEvent(t2) && this.adapter.notifyNavigation(e2);
          }, d.prototype.removeFocus = function() {
            this.adapter.setAttribute(u.strings.TAB_INDEX, "-1");
          }, d.prototype.focus = function() {
            this.adapter.setAttribute(u.strings.TAB_INDEX, "0"), this.adapter.focus();
          }, d.prototype.isNavigable = function() {
            return "true" !== this.adapter.getAttribute(u.strings.ARIA_HIDDEN);
          }, d.prototype.shouldNotifyInteractionFromKey = function(t2) {
            var e2 = t2 === c.KEY.ENTER || t2 === c.KEY.SPACEBAR, n3 = t2 === c.KEY.BACKSPACE || t2 === c.KEY.DELETE;
            return e2 || n3;
          }, d.prototype.getTriggerFromKey = function(t2) {
            return t2 === c.KEY.SPACEBAR ? u.InteractionTrigger.SPACEBAR_KEY : t2 === c.KEY.ENTER ? u.InteractionTrigger.ENTER_KEY : t2 === c.KEY.DELETE ? u.InteractionTrigger.DELETE_KEY : t2 === c.KEY.BACKSPACE ? u.InteractionTrigger.BACKSPACE_KEY : u.InteractionTrigger.UNSPECIFIED;
          }, d);
          function d(t2) {
            return s.call(this, o(o({}, d.defaultAdapter), t2)) || this;
          }
          e.MDCChipTrailingActionFoundation = l, e.default = l;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCChip = void 0;
          var s, a = n2(1), c = n2(2), u = n2(4), l = n2(49), d = n2(12), p2 = n2(13), h = n2(21), f = (s = a.MDCComponent, r2(y, s), Object.defineProperty(y.prototype, "selected", { get: function() {
            return this.foundation.isSelected();
          }, set: function(t2) {
            this.foundation.setSelected(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(y.prototype, "shouldRemoveOnTrailingIconClick", { get: function() {
            return this.foundation.getShouldRemoveOnTrailingIconClick();
          }, set: function(t2) {
            this.foundation.setShouldRemoveOnTrailingIconClick(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(y.prototype, "setShouldFocusPrimaryActionOnClick", { set: function(t2) {
            this.foundation.setShouldFocusPrimaryActionOnClick(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(y.prototype, "ripple", { get: function() {
            return this.rippleSurface;
          }, enumerable: false, configurable: true }), Object.defineProperty(y.prototype, "id", { get: function() {
            return this.root.id;
          }, enumerable: false, configurable: true }), y.attachTo = function(t2) {
            return new y(t2);
          }, y.prototype.initialize = function(t2, e2) {
            var n3 = this;
            void 0 === t2 && (t2 = function(t3, e3) {
              return new c.MDCRipple(t3, e3);
            }), void 0 === e2 && (e2 = function(t3) {
              return new l.MDCChipTrailingAction(t3);
            }), this.leadingIcon = this.root.querySelector(p2.strings.LEADING_ICON_SELECTOR), this.checkmark = this.root.querySelector(p2.strings.CHECKMARK_SELECTOR), this.primaryAction = this.root.querySelector(p2.strings.PRIMARY_ACTION_SELECTOR);
            var i4 = this.root.querySelector(p2.strings.TRAILING_ACTION_SELECTOR);
            i4 && (this.trailingAction = e2(i4));
            var r3 = o(o({}, c.MDCRipple.createAdapter(this)), { computeBoundingRect: function() {
              return n3.foundation.getDimensions();
            } });
            this.rippleSurface = t2(this.root, new u.MDCRippleFoundation(r3));
          }, y.prototype.initialSyncWithDOM = function() {
            var e2 = this;
            this.handleTrailingActionInteraction = function() {
              e2.foundation.handleTrailingActionInteraction();
            }, this.handleTrailingActionNavigation = function(t2) {
              e2.foundation.handleTrailingActionNavigation(t2);
            }, this.handleClick = function() {
              e2.foundation.handleClick();
            }, this.handleKeydown = function(t2) {
              e2.foundation.handleKeydown(t2);
            }, this.handleTransitionEnd = function(t2) {
              e2.foundation.handleTransitionEnd(t2);
            }, this.handleFocusIn = function(t2) {
              e2.foundation.handleFocusIn(t2);
            }, this.handleFocusOut = function(t2) {
              e2.foundation.handleFocusOut(t2);
            }, this.listen("transitionend", this.handleTransitionEnd), this.listen("click", this.handleClick), this.listen("keydown", this.handleKeydown), this.listen("focusin", this.handleFocusIn), this.listen("focusout", this.handleFocusOut), this.trailingAction && (this.listen(d.strings.INTERACTION_EVENT, this.handleTrailingActionInteraction), this.listen(d.strings.NAVIGATION_EVENT, this.handleTrailingActionNavigation));
          }, y.prototype.destroy = function() {
            this.rippleSurface.destroy(), this.unlisten("transitionend", this.handleTransitionEnd), this.unlisten("keydown", this.handleKeydown), this.unlisten("click", this.handleClick), this.unlisten("focusin", this.handleFocusIn), this.unlisten("focusout", this.handleFocusOut), this.trailingAction && (this.unlisten(d.strings.INTERACTION_EVENT, this.handleTrailingActionInteraction), this.unlisten(d.strings.NAVIGATION_EVENT, this.handleTrailingActionNavigation)), s.prototype.destroy.call(this);
          }, y.prototype.beginExit = function() {
            this.foundation.beginExit();
          }, y.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, addClassToLeadingIcon: function(t3) {
              n3.leadingIcon && n3.leadingIcon.classList.add(t3);
            }, eventTargetHasClass: function(t3, e2) {
              return !!t3 && t3.classList.contains(e2);
            }, focusPrimaryAction: function() {
              n3.primaryAction && n3.primaryAction.focus();
            }, focusTrailingAction: function() {
              n3.trailingAction && n3.trailingAction.focus();
            }, getAttribute: function(t3) {
              return n3.root.getAttribute(t3);
            }, getCheckmarkBoundingClientRect: function() {
              return n3.checkmark ? n3.checkmark.getBoundingClientRect() : null;
            }, getComputedStyleValue: function(t3) {
              return window.getComputedStyle(n3.root).getPropertyValue(t3);
            }, getRootBoundingClientRect: function() {
              return n3.root.getBoundingClientRect();
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, hasLeadingIcon: function() {
              return !!n3.leadingIcon;
            }, isRTL: function() {
              return "rtl" === window.getComputedStyle(n3.root).getPropertyValue("direction");
            }, isTrailingActionNavigable: function() {
              return !!n3.trailingAction && n3.trailingAction.isNavigable();
            }, notifyInteraction: function() {
              return n3.emit(p2.strings.INTERACTION_EVENT, { chipId: n3.id }, true);
            }, notifyNavigation: function(t3, e2) {
              return n3.emit(p2.strings.NAVIGATION_EVENT, { chipId: n3.id, key: t3, source: e2 }, true);
            }, notifyRemoval: function(t3) {
              n3.emit(p2.strings.REMOVAL_EVENT, { chipId: n3.id, removedAnnouncement: t3 }, true);
            }, notifySelection: function(t3, e2) {
              return n3.emit(p2.strings.SELECTION_EVENT, { chipId: n3.id, selected: t3, shouldIgnore: e2 }, true);
            }, notifyTrailingIconInteraction: function() {
              return n3.emit(p2.strings.TRAILING_ICON_INTERACTION_EVENT, { chipId: n3.id }, true);
            }, notifyEditStart: function() {
            }, notifyEditFinish: function() {
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, removeClassFromLeadingIcon: function(t3) {
              n3.leadingIcon && n3.leadingIcon.classList.remove(t3);
            }, removeTrailingActionFocus: function() {
              n3.trailingAction && n3.trailingAction.removeFocus();
            }, setPrimaryActionAttr: function(t3, e2) {
              n3.primaryAction && n3.primaryAction.setAttribute(t3, e2);
            }, setStyleProperty: function(t3, e2) {
              return n3.root.style.setProperty(t3, e2);
            } };
            return new h.MDCChipFoundation(t2);
          }, y.prototype.setSelectedFromChipSet = function(t2, e2) {
            this.foundation.setSelectedFromChipSet(t2, e2);
          }, y.prototype.focusPrimaryAction = function() {
            this.foundation.focusPrimaryAction();
          }, y.prototype.focusTrailingAction = function() {
            this.foundation.focusTrailingAction();
          }, y.prototype.removeFocus = function() {
            this.foundation.removeFocus();
          }, y.prototype.remove = function() {
            var t2 = this.root.parentNode;
            null !== t2 && t2.removeChild(this.root);
          }, y);
          function y() {
            return null !== s && s.apply(this, arguments) || this;
          }
          e.MDCChip = f;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCChipSetFoundation = void 0;
          var s, a = n2(0), h = n2(13), c = n2(53), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { announceMessage: function() {
            }, focusChipPrimaryActionAtIndex: function() {
            }, focusChipTrailingActionAtIndex: function() {
            }, getChipListCount: function() {
              return -1;
            }, getIndexOfChipById: function() {
              return -1;
            }, hasClass: function() {
              return false;
            }, isRTL: function() {
              return false;
            }, removeChipAtIndex: function() {
            }, removeFocusFromChipAtIndex: function() {
            }, selectChipAtIndex: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.getSelectedChipIds = function() {
            return this.selectedChipIds.slice();
          }, l.prototype.select = function(t2) {
            this.selectImpl(t2, false);
          }, l.prototype.handleChipInteraction = function(t2) {
            var e2 = t2.chipId, n3 = this.adapter.getIndexOfChipById(e2);
            this.removeFocusFromChipsExcept(n3), (this.adapter.hasClass(c.cssClasses.CHOICE) || this.adapter.hasClass(c.cssClasses.FILTER)) && this.toggleSelect(e2);
          }, l.prototype.handleChipSelection = function(t2) {
            var e2 = t2.chipId, n3 = t2.selected;
            if (!t2.shouldIgnore) {
              var i4 = 0 <= this.selectedChipIds.indexOf(e2);
              n3 && !i4 ? this.select(e2) : !n3 && i4 && this.deselectImpl(e2);
            }
          }, l.prototype.handleChipRemoval = function(t2) {
            var e2 = t2.chipId, n3 = t2.removedAnnouncement;
            n3 && this.adapter.announceMessage(n3);
            var i4 = this.adapter.getIndexOfChipById(e2);
            this.deselectAndNotifyClients(e2), this.adapter.removeChipAtIndex(i4);
            var r3 = this.adapter.getChipListCount() - 1;
            if (!(r3 < 0)) {
              var o2 = Math.min(i4, r3);
              this.removeFocusFromChipsExcept(o2), this.adapter.focusChipTrailingActionAtIndex(o2);
            }
          }, l.prototype.handleChipNavigation = function(t2) {
            var e2 = t2.chipId, n3 = t2.key, i4 = t2.source, r3 = this.adapter.getChipListCount() - 1, o2 = this.adapter.getIndexOfChipById(e2);
            if (-1 !== o2 && h.navigationKeys.has(n3)) {
              var s2 = this.adapter.isRTL(), a2 = n3 === h.strings.ARROW_LEFT_KEY || n3 === h.strings.IE_ARROW_LEFT_KEY, c2 = n3 === h.strings.ARROW_RIGHT_KEY || n3 === h.strings.IE_ARROW_RIGHT_KEY, u2 = n3 === h.strings.ARROW_DOWN_KEY || n3 === h.strings.IE_ARROW_DOWN_KEY, l2 = !s2 && c2 || s2 && a2 || u2, d = n3 === h.strings.HOME_KEY, p2 = n3 === h.strings.END_KEY;
              l2 ? o2++ : d ? o2 = 0 : p2 ? o2 = r3 : o2--, o2 < 0 || r3 < o2 || (this.removeFocusFromChipsExcept(o2), this.focusChipAction(o2, n3, i4));
            }
          }, l.prototype.focusChipAction = function(t2, e2, n3) {
            var i4 = h.jumpChipKeys.has(e2);
            if (i4 && n3 === h.EventSource.PRIMARY) return this.adapter.focusChipPrimaryActionAtIndex(t2);
            if (i4 && n3 === h.EventSource.TRAILING) return this.adapter.focusChipTrailingActionAtIndex(t2);
            var r3 = this.getDirection(e2);
            return r3 === h.Direction.LEFT ? this.adapter.focusChipTrailingActionAtIndex(t2) : r3 === h.Direction.RIGHT ? this.adapter.focusChipPrimaryActionAtIndex(t2) : void 0;
          }, l.prototype.getDirection = function(t2) {
            var e2 = this.adapter.isRTL(), n3 = t2 === h.strings.ARROW_LEFT_KEY || t2 === h.strings.IE_ARROW_LEFT_KEY, i4 = t2 === h.strings.ARROW_RIGHT_KEY || t2 === h.strings.IE_ARROW_RIGHT_KEY;
            return !e2 && n3 || e2 && i4 ? h.Direction.LEFT : h.Direction.RIGHT;
          }, l.prototype.deselectImpl = function(t2, e2) {
            void 0 === e2 && (e2 = false);
            var n3 = this.selectedChipIds.indexOf(t2);
            if (0 <= n3) {
              this.selectedChipIds.splice(n3, 1);
              var i4 = this.adapter.getIndexOfChipById(t2);
              this.adapter.selectChipAtIndex(i4, false, e2);
            }
          }, l.prototype.deselectAndNotifyClients = function(t2) {
            this.deselectImpl(t2, true);
          }, l.prototype.toggleSelect = function(t2) {
            0 <= this.selectedChipIds.indexOf(t2) ? this.deselectAndNotifyClients(t2) : this.selectAndNotifyClients(t2);
          }, l.prototype.removeFocusFromChipsExcept = function(t2) {
            for (var e2 = this.adapter.getChipListCount(), n3 = 0; n3 < e2; n3++) n3 !== t2 && this.adapter.removeFocusFromChipAtIndex(n3);
          }, l.prototype.selectAndNotifyClients = function(t2) {
            this.selectImpl(t2, true);
          }, l.prototype.selectImpl = function(t2, e2) {
            if (!(0 <= this.selectedChipIds.indexOf(t2))) {
              if (this.adapter.hasClass(c.cssClasses.CHOICE) && 0 < this.selectedChipIds.length) {
                var n3 = this.selectedChipIds[0], i4 = this.adapter.getIndexOfChipById(n3);
                this.selectedChipIds = [], this.adapter.selectChipAtIndex(i4, false, e2);
              }
              this.selectedChipIds.push(t2);
              var r3 = this.adapter.getIndexOfChipById(t2);
              this.adapter.selectChipAtIndex(r3, true, e2);
            }
          }, l);
          function l(t2) {
            var e2 = s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
            return e2.selectedChipIds = [], e2;
          }
          e.MDCChipSetFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = e.strings = void 0, e.strings = { CHIP_SELECTOR: ".mdc-chip" }, e.cssClasses = { CHOICE: "mdc-chip-set--choice", FILTER: "mdc-chip-set--filter" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCCircularProgressFoundation = void 0;
          var s, a = n2(0), c = n2(55), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, getDeterminateCircleAttribute: function() {
              return null;
            }, hasClass: function() {
              return false;
            }, removeClass: function() {
            }, removeAttribute: function() {
            }, setAttribute: function() {
            }, setDeterminateCircleAttribute: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.init = function() {
            this.closed = this.adapter.hasClass(c.cssClasses.CLOSED_CLASS), this.determinate = !this.adapter.hasClass(c.cssClasses.INDETERMINATE_CLASS), this.progress = 0, this.determinate && this.adapter.setAttribute(c.strings.ARIA_VALUENOW, this.progress.toString()), this.radius = Number(this.adapter.getDeterminateCircleAttribute(c.strings.RADIUS));
          }, l.prototype.setDeterminate = function(t2) {
            this.determinate = t2, this.determinate ? (this.adapter.removeClass(c.cssClasses.INDETERMINATE_CLASS), this.setProgress(this.progress)) : (this.adapter.addClass(c.cssClasses.INDETERMINATE_CLASS), this.adapter.removeAttribute(c.strings.ARIA_VALUENOW));
          }, l.prototype.isDeterminate = function() {
            return this.determinate;
          }, l.prototype.setProgress = function(t2) {
            if (this.progress = t2, this.determinate) {
              var e2 = (1 - this.progress) * (2 * Math.PI * this.radius);
              this.adapter.setDeterminateCircleAttribute(c.strings.STROKE_DASHOFFSET, "" + e2), this.adapter.setAttribute(c.strings.ARIA_VALUENOW, this.progress.toString());
            }
          }, l.prototype.getProgress = function() {
            return this.progress;
          }, l.prototype.open = function() {
            this.closed = false, this.adapter.removeClass(c.cssClasses.CLOSED_CLASS), this.adapter.removeAttribute(c.strings.ARIA_HIDDEN);
          }, l.prototype.close = function() {
            this.closed = true, this.adapter.addClass(c.cssClasses.CLOSED_CLASS), this.adapter.setAttribute(c.strings.ARIA_HIDDEN, "true");
          }, l.prototype.isClosed = function() {
            return this.closed;
          }, l);
          function l(t2) {
            return s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
          }
          e.MDCCircularProgressFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.cssClasses = void 0, e.cssClasses = { INDETERMINATE_CLASS: "mdc-circular-progress--indeterminate", CLOSED_CLASS: "mdc-circular-progress--closed" }, e.strings = { ARIA_HIDDEN: "aria-hidden", ARIA_VALUENOW: "aria-valuenow", DETERMINATE_CIRCLE_SELECTOR: ".mdc-circular-progress__determinate-circle", RADIUS: "r", STROKE_DASHOFFSET: "stroke-dashoffset" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCLinearProgress = void 0;
          var o, s = n2(1), a = n2(57), c = (o = s.MDCComponent, r2(u, o), u.attachTo = function(t2) {
            return new u(t2);
          }, Object.defineProperty(u.prototype, "determinate", { set: function(t2) {
            this.foundation.setDeterminate(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(u.prototype, "progress", { set: function(t2) {
            this.foundation.setProgress(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(u.prototype, "buffer", { set: function(t2) {
            this.foundation.setBuffer(t2);
          }, enumerable: false, configurable: true }), u.prototype.open = function() {
            this.foundation.open();
          }, u.prototype.close = function() {
            this.foundation.close();
          }, u.prototype.initialSyncWithDOM = function() {
            var t2 = this;
            this.root.addEventListener("transitionend", function() {
              t2.foundation.handleTransitionEnd();
            });
          }, u.prototype.getDefaultFoundation = function() {
            var i4 = this, t2 = { addClass: function(t3) {
              i4.root.classList.add(t3);
            }, forceLayout: function() {
              i4.root.getBoundingClientRect();
            }, setBufferBarStyle: function(t3, e2) {
              var n3 = i4.root.querySelector(a.MDCLinearProgressFoundation.strings.BUFFER_BAR_SELECTOR);
              n3 && n3.style.setProperty(t3, e2);
            }, setPrimaryBarStyle: function(t3, e2) {
              var n3 = i4.root.querySelector(a.MDCLinearProgressFoundation.strings.PRIMARY_BAR_SELECTOR);
              n3 && n3.style.setProperty(t3, e2);
            }, hasClass: function(t3) {
              return i4.root.classList.contains(t3);
            }, removeAttribute: function(t3) {
              i4.root.removeAttribute(t3);
            }, removeClass: function(t3) {
              i4.root.classList.remove(t3);
            }, setAttribute: function(t3, e2) {
              i4.root.setAttribute(t3, e2);
            }, setStyle: function(t3, e2) {
              i4.root.style.setProperty(t3, e2);
            }, attachResizeObserver: function(t3) {
              var e2 = window.ResizeObserver;
              if (e2) {
                var n3 = new e2(t3);
                return n3.observe(i4.root), n3;
              }
              return null;
            }, getWidth: function() {
              return i4.root.offsetWidth;
            } };
            return new a.MDCLinearProgressFoundation(t2);
          }, u);
          function u() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCLinearProgress = c;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, a = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCLinearProgressFoundation = void 0;
          var s, c = n2(10), u = n2(0), l = n2(58), d = (s = u.MDCFoundation, r2(p2, s), Object.defineProperty(p2, "cssClasses", { get: function() {
            return l.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "strings", { get: function() {
            return l.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, attachResizeObserver: function() {
              return null;
            }, forceLayout: function() {
            }, getWidth: function() {
              return 0;
            }, hasClass: function() {
              return false;
            }, setBufferBarStyle: function() {
              return null;
            }, setPrimaryBarStyle: function() {
              return null;
            }, setStyle: function() {
            }, removeAttribute: function() {
            }, removeClass: function() {
            }, setAttribute: function() {
            } };
          }, enumerable: false, configurable: true }), p2.prototype.init = function() {
            var s2 = this;
            this.determinate = !this.adapter.hasClass(l.cssClasses.INDETERMINATE_CLASS), this.adapter.addClass(l.cssClasses.ANIMATION_READY_CLASS), this.progress = 0, this.buffer = 1, this.observer = this.adapter.attachResizeObserver(function(t2) {
              var e2, n3;
              if (!s2.determinate) try {
                for (var i4 = a(t2), r3 = i4.next(); !r3.done; r3 = i4.next()) {
                  var o2 = r3.value;
                  o2.contentRect && s2.calculateAndSetDimensions(o2.contentRect.width);
                }
              } catch (t3) {
                e2 = { error: t3 };
              } finally {
                try {
                  r3 && !r3.done && (n3 = i4.return) && n3.call(i4);
                } finally {
                  if (e2) throw e2.error;
                }
              }
            }), !this.determinate && this.observer && this.calculateAndSetDimensions(this.adapter.getWidth());
          }, p2.prototype.setDeterminate = function(t2) {
            if (this.determinate = t2, this.determinate) return this.adapter.removeClass(l.cssClasses.INDETERMINATE_CLASS), this.adapter.setAttribute(l.strings.ARIA_VALUENOW, this.progress.toString()), this.adapter.setAttribute(l.strings.ARIA_VALUEMAX, "1"), this.adapter.setAttribute(l.strings.ARIA_VALUEMIN, "0"), this.setPrimaryBarProgress(this.progress), void this.setBufferBarProgress(this.buffer);
            this.observer && this.calculateAndSetDimensions(this.adapter.getWidth()), this.adapter.addClass(l.cssClasses.INDETERMINATE_CLASS), this.adapter.removeAttribute(l.strings.ARIA_VALUENOW), this.adapter.removeAttribute(l.strings.ARIA_VALUEMAX), this.adapter.removeAttribute(l.strings.ARIA_VALUEMIN), this.setPrimaryBarProgress(1), this.setBufferBarProgress(1);
          }, p2.prototype.isDeterminate = function() {
            return this.determinate;
          }, p2.prototype.setProgress = function(t2) {
            this.progress = t2, this.determinate && (this.setPrimaryBarProgress(t2), this.adapter.setAttribute(l.strings.ARIA_VALUENOW, t2.toString()));
          }, p2.prototype.getProgress = function() {
            return this.progress;
          }, p2.prototype.setBuffer = function(t2) {
            this.buffer = t2, this.determinate && this.setBufferBarProgress(t2);
          }, p2.prototype.getBuffer = function() {
            return this.buffer;
          }, p2.prototype.open = function() {
            this.adapter.removeClass(l.cssClasses.CLOSED_CLASS), this.adapter.removeClass(l.cssClasses.CLOSED_ANIMATION_OFF_CLASS), this.adapter.removeAttribute(l.strings.ARIA_HIDDEN);
          }, p2.prototype.close = function() {
            this.adapter.addClass(l.cssClasses.CLOSED_CLASS), this.adapter.setAttribute(l.strings.ARIA_HIDDEN, "true");
          }, p2.prototype.isClosed = function() {
            return this.adapter.hasClass(l.cssClasses.CLOSED_CLASS);
          }, p2.prototype.handleTransitionEnd = function() {
            this.adapter.hasClass(l.cssClasses.CLOSED_CLASS) && this.adapter.addClass(l.cssClasses.CLOSED_ANIMATION_OFF_CLASS);
          }, p2.prototype.destroy = function() {
            s.prototype.destroy.call(this), this.observer && this.observer.disconnect();
          }, p2.prototype.restartAnimation = function() {
            this.adapter.removeClass(l.cssClasses.ANIMATION_READY_CLASS), this.adapter.forceLayout(), this.adapter.addClass(l.cssClasses.ANIMATION_READY_CLASS);
          }, p2.prototype.setPrimaryBarProgress = function(t2) {
            var e2 = "scaleX(" + t2 + ")", n3 = "undefined" != typeof window ? c.getCorrectPropertyName(window, "transform") : "transform";
            this.adapter.setPrimaryBarStyle(n3, e2);
          }, p2.prototype.setBufferBarProgress = function(t2) {
            var e2 = 100 * t2 + "%";
            this.adapter.setBufferBarStyle(l.strings.FLEX_BASIS, e2);
          }, p2.prototype.calculateAndSetDimensions = function(t2) {
            var e2 = t2 * l.animationDimensionPercentages.PRIMARY_HALF, n3 = t2 * l.animationDimensionPercentages.PRIMARY_FULL, i4 = t2 * l.animationDimensionPercentages.SECONDARY_QUARTER, r3 = t2 * l.animationDimensionPercentages.SECONDARY_HALF, o2 = t2 * l.animationDimensionPercentages.SECONDARY_FULL;
            this.adapter.setStyle("--mdc-linear-progress-primary-half", e2 + "px"), this.adapter.setStyle("--mdc-linear-progress-primary-half-neg", -e2 + "px"), this.adapter.setStyle("--mdc-linear-progress-primary-full", n3 + "px"), this.adapter.setStyle("--mdc-linear-progress-primary-full-neg", -n3 + "px"), this.adapter.setStyle("--mdc-linear-progress-secondary-quarter", i4 + "px"), this.adapter.setStyle("--mdc-linear-progress-secondary-quarter-neg", -i4 + "px"), this.adapter.setStyle("--mdc-linear-progress-secondary-half", r3 + "px"), this.adapter.setStyle("--mdc-linear-progress-secondary-half-neg", -r3 + "px"), this.adapter.setStyle("--mdc-linear-progress-secondary-full", o2 + "px"), this.adapter.setStyle("--mdc-linear-progress-secondary-full-neg", -o2 + "px"), this.restartAnimation();
          }, p2);
          function p2(t2) {
            var e2 = s.call(this, o(o({}, p2.defaultAdapter), t2)) || this;
            return e2.observer = null, e2;
          }
          e.MDCLinearProgressFoundation = d, e.default = d;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.animationDimensionPercentages = e.strings = e.cssClasses = void 0, e.cssClasses = { CLOSED_CLASS: "mdc-linear-progress--closed", CLOSED_ANIMATION_OFF_CLASS: "mdc-linear-progress--closed-animation-off", INDETERMINATE_CLASS: "mdc-linear-progress--indeterminate", REVERSED_CLASS: "mdc-linear-progress--reversed", ANIMATION_READY_CLASS: "mdc-linear-progress--animation-ready" }, e.strings = { ARIA_HIDDEN: "aria-hidden", ARIA_VALUEMAX: "aria-valuemax", ARIA_VALUEMIN: "aria-valuemin", ARIA_VALUENOW: "aria-valuenow", BUFFER_BAR_SELECTOR: ".mdc-linear-progress__buffer-bar", FLEX_BASIS: "flex-basis", PRIMARY_BAR_SELECTOR: ".mdc-linear-progress__primary-bar" }, e.animationDimensionPercentages = { PRIMARY_HALF: 0.8367142, PRIMARY_FULL: 2.00611057, SECONDARY_QUARTER: 0.37651913, SECONDARY_HALF: 0.84386165, SECONDARY_FULL: 1.60277782 };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__awaiter || function(t2, s2, a2, c2) {
            return new (a2 = a2 || Promise)(function(e2, n3) {
              function i4(t3) {
                try {
                  o2(c2.next(t3));
                } catch (t4) {
                  n3(t4);
                }
              }
              function r3(t3) {
                try {
                  o2(c2.throw(t3));
                } catch (t4) {
                  n3(t4);
                }
              }
              function o2(t3) {
                t3.done ? e2(t3.value) : function(e3) {
                  return e3 instanceof a2 ? e3 : new a2(function(t4) {
                    t4(e3);
                  });
                }(t3.value).then(i4, r3);
              }
              o2((c2 = c2.apply(t2, s2 || [])).next());
            });
          }, a = this && this.__generator || function(n3, i4) {
            var r3, o2, s2, t2, a2 = { label: 0, sent: function() {
              if (1 & s2[0]) throw s2[1];
              return s2[1];
            }, trys: [], ops: [] };
            return t2 = { next: e2(0), throw: e2(1), return: e2(2) }, "function" == typeof Symbol && (t2[Symbol.iterator] = function() {
              return this;
            }), t2;
            function e2(e3) {
              return function(t3) {
                return function(e4) {
                  if (r3) throw new TypeError("Generator is already executing.");
                  for (; a2; ) try {
                    if (r3 = 1, o2 && (s2 = 2 & e4[0] ? o2.return : e4[0] ? o2.throw || ((s2 = o2.return) && s2.call(o2), 0) : o2.next) && !(s2 = s2.call(o2, e4[1])).done) return s2;
                    switch (o2 = 0, s2 && (e4 = [2 & e4[0], s2.value]), e4[0]) {
                      case 0:
                      case 1:
                        s2 = e4;
                        break;
                      case 4:
                        return a2.label++, { value: e4[1], done: false };
                      case 5:
                        a2.label++, o2 = e4[1], e4 = [0];
                        continue;
                      case 7:
                        e4 = a2.ops.pop(), a2.trys.pop();
                        continue;
                      default:
                        if (!(s2 = 0 < (s2 = a2.trys).length && s2[s2.length - 1]) && (6 === e4[0] || 2 === e4[0])) {
                          a2 = 0;
                          continue;
                        }
                        if (3 === e4[0] && (!s2 || e4[1] > s2[0] && e4[1] < s2[3])) {
                          a2.label = e4[1];
                          break;
                        }
                        if (6 === e4[0] && a2.label < s2[1]) {
                          a2.label = s2[1], s2 = e4;
                          break;
                        }
                        if (s2 && a2.label < s2[2]) {
                          a2.label = s2[2], a2.ops.push(e4);
                          break;
                        }
                        s2[2] && a2.ops.pop(), a2.trys.pop();
                        continue;
                    }
                    e4 = i4.call(n3, a2);
                  } catch (t4) {
                    e4 = [6, t4], o2 = 0;
                  } finally {
                    r3 = s2 = 0;
                  }
                  if (5 & e4[0]) throw e4[1];
                  return { value: e4[0] ? e4[1] : void 0, done: true };
                }([e3, t3]);
              };
            }
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCDataTableFoundation = void 0;
          var c, u = n2(0), l = n2(22), d = (c = u.MDCFoundation, r2(p2, c), Object.defineProperty(p2, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, addClassAtRowIndex: function() {
            }, getAttributeByHeaderCellIndex: function() {
              return "";
            }, getHeaderCellCount: function() {
              return 0;
            }, getHeaderCellElements: function() {
              return [];
            }, getRowCount: function() {
              return 0;
            }, getRowElements: function() {
              return [];
            }, getRowIdAtIndex: function() {
              return "";
            }, getRowIndexByChildElement: function() {
              return 0;
            }, getSelectedRowCount: function() {
              return 0;
            }, getTableContainerHeight: function() {
              return 0;
            }, getTableHeaderHeight: function() {
              return 0;
            }, isCheckboxAtRowIndexChecked: function() {
              return false;
            }, isHeaderRowCheckboxChecked: function() {
              return false;
            }, isRowsSelectable: function() {
              return false;
            }, notifyRowSelectionChanged: function() {
            }, notifySelectedAll: function() {
            }, notifySortAction: function() {
            }, notifyUnselectedAll: function() {
            }, notifyRowClick: function() {
            }, registerHeaderRowCheckbox: function() {
            }, registerRowCheckboxes: function() {
            }, removeClass: function() {
            }, removeClassAtRowIndex: function() {
            }, removeClassNameByHeaderCellIndex: function() {
            }, setAttributeAtRowIndex: function() {
            }, setAttributeByHeaderCellIndex: function() {
            }, setClassNameByHeaderCellIndex: function() {
            }, setHeaderRowCheckboxChecked: function() {
            }, setHeaderRowCheckboxIndeterminate: function() {
            }, setProgressIndicatorStyles: function() {
            }, setRowCheckboxCheckedAtIndex: function() {
            }, setSortStatusLabelByHeaderCellIndex: function() {
            } };
          }, enumerable: false, configurable: true }), p2.prototype.layout = function() {
            this.adapter.isRowsSelectable() && (this.adapter.registerHeaderRowCheckbox(), this.adapter.registerRowCheckboxes(), this.setHeaderRowCheckboxState());
          }, p2.prototype.layoutAsync = function() {
            return s(this, void 0, void 0, function() {
              return a(this, function(t2) {
                switch (t2.label) {
                  case 0:
                    return this.adapter.isRowsSelectable() ? [4, this.adapter.registerHeaderRowCheckbox()] : [3, 3];
                  case 1:
                    return t2.sent(), [4, this.adapter.registerRowCheckboxes()];
                  case 2:
                    t2.sent(), this.setHeaderRowCheckboxState(), t2.label = 3;
                  case 3:
                    return [2];
                }
              });
            });
          }, p2.prototype.getRows = function() {
            return this.adapter.getRowElements();
          }, p2.prototype.getHeaderCells = function() {
            return this.adapter.getHeaderCellElements();
          }, p2.prototype.setSelectedRowIds = function(t2) {
            for (var e2 = 0; e2 < this.adapter.getRowCount(); e2++) {
              var n3 = this.adapter.getRowIdAtIndex(e2), i4 = false;
              n3 && 0 <= t2.indexOf(n3) && (i4 = true), this.adapter.setRowCheckboxCheckedAtIndex(e2, i4), this.selectRowAtIndex(e2, i4);
            }
            this.setHeaderRowCheckboxState();
          }, p2.prototype.getRowIds = function() {
            for (var t2 = [], e2 = 0; e2 < this.adapter.getRowCount(); e2++) t2.push(this.adapter.getRowIdAtIndex(e2));
            return t2;
          }, p2.prototype.getSelectedRowIds = function() {
            for (var t2 = [], e2 = 0; e2 < this.adapter.getRowCount(); e2++) this.adapter.isCheckboxAtRowIndexChecked(e2) && t2.push(this.adapter.getRowIdAtIndex(e2));
            return t2;
          }, p2.prototype.handleHeaderRowCheckboxChange = function() {
            for (var t2 = this.adapter.isHeaderRowCheckboxChecked(), e2 = 0; e2 < this.adapter.getRowCount(); e2++) this.adapter.setRowCheckboxCheckedAtIndex(e2, t2), this.selectRowAtIndex(e2, t2);
            t2 ? this.adapter.notifySelectedAll() : this.adapter.notifyUnselectedAll();
          }, p2.prototype.handleRowCheckboxChange = function(t2) {
            var e2 = this.adapter.getRowIndexByChildElement(t2.target);
            if (-1 !== e2) {
              var n3 = this.adapter.isCheckboxAtRowIndexChecked(e2);
              this.selectRowAtIndex(e2, n3), this.setHeaderRowCheckboxState();
              var i4 = this.adapter.getRowIdAtIndex(e2);
              this.adapter.notifyRowSelectionChanged({ rowId: i4, rowIndex: e2, selected: n3 });
            }
          }, p2.prototype.handleSortAction = function(t2) {
            for (var e2 = t2.columnId, n3 = t2.columnIndex, i4 = t2.headerCell, r3 = 0; r3 < this.adapter.getHeaderCellCount(); r3++) r3 !== n3 && (this.adapter.removeClassNameByHeaderCellIndex(r3, l.cssClasses.HEADER_CELL_SORTED), this.adapter.removeClassNameByHeaderCellIndex(r3, l.cssClasses.HEADER_CELL_SORTED_DESCENDING), this.adapter.setAttributeByHeaderCellIndex(r3, l.strings.ARIA_SORT, l.SortValue.NONE), this.adapter.setSortStatusLabelByHeaderCellIndex(r3, l.SortValue.NONE));
            this.adapter.setClassNameByHeaderCellIndex(n3, l.cssClasses.HEADER_CELL_SORTED);
            var o2 = this.adapter.getAttributeByHeaderCellIndex(n3, l.strings.ARIA_SORT), s2 = l.SortValue.NONE;
            s2 = o2 === l.SortValue.ASCENDING ? (this.adapter.setClassNameByHeaderCellIndex(n3, l.cssClasses.HEADER_CELL_SORTED_DESCENDING), this.adapter.setAttributeByHeaderCellIndex(n3, l.strings.ARIA_SORT, l.SortValue.DESCENDING), l.SortValue.DESCENDING) : (o2 === l.SortValue.DESCENDING && this.adapter.removeClassNameByHeaderCellIndex(n3, l.cssClasses.HEADER_CELL_SORTED_DESCENDING), this.adapter.setAttributeByHeaderCellIndex(n3, l.strings.ARIA_SORT, l.SortValue.ASCENDING), l.SortValue.ASCENDING), this.adapter.setSortStatusLabelByHeaderCellIndex(n3, s2), this.adapter.notifySortAction({ columnId: e2, columnIndex: n3, headerCell: i4, sortValue: s2 });
          }, p2.prototype.handleRowClick = function(t2) {
            var e2 = t2.rowId, n3 = t2.row;
            this.adapter.notifyRowClick({ rowId: e2, row: n3 });
          }, p2.prototype.showProgress = function() {
            var t2 = this.adapter.getTableHeaderHeight(), e2 = this.adapter.getTableContainerHeight() - t2, n3 = t2;
            this.adapter.setProgressIndicatorStyles({ height: e2 + "px", top: n3 + "px" }), this.adapter.addClass(l.cssClasses.IN_PROGRESS);
          }, p2.prototype.hideProgress = function() {
            this.adapter.removeClass(l.cssClasses.IN_PROGRESS);
          }, p2.prototype.setHeaderRowCheckboxState = function() {
            0 === this.adapter.getSelectedRowCount() ? (this.adapter.setHeaderRowCheckboxChecked(false), this.adapter.setHeaderRowCheckboxIndeterminate(false)) : this.adapter.getSelectedRowCount() === this.adapter.getRowCount() ? (this.adapter.setHeaderRowCheckboxChecked(true), this.adapter.setHeaderRowCheckboxIndeterminate(false)) : (this.adapter.setHeaderRowCheckboxIndeterminate(true), this.adapter.setHeaderRowCheckboxChecked(false));
          }, p2.prototype.selectRowAtIndex = function(t2, e2) {
            e2 ? (this.adapter.addClassAtRowIndex(t2, l.cssClasses.ROW_SELECTED), this.adapter.setAttributeAtRowIndex(t2, l.strings.ARIA_SELECTED, "true")) : (this.adapter.removeClassAtRowIndex(t2, l.cssClasses.ROW_SELECTED), this.adapter.setAttributeAtRowIndex(t2, l.strings.ARIA_SELECTED, "false"));
          }, p2);
          function p2(t2) {
            return c.call(this, o(o({}, p2.defaultAdapter), t2)) || this;
          }
          e.MDCDataTableFoundation = d;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.areTopsMisaligned = e.isScrollAtBottom = e.isScrollAtTop = e.isScrollable = e.createFocusTrapInstance = void 0, e.createFocusTrapInstance = function(t2, e2, n3) {
            return e2(t2, { initialFocusEl: n3 });
          }, e.isScrollable = function(t2) {
            return !!t2 && t2.scrollHeight > t2.offsetHeight;
          }, e.isScrollAtTop = function(t2) {
            return !!t2 && 0 === t2.scrollTop;
          }, e.isScrollAtBottom = function(t2) {
            return !!t2 && Math.ceil(t2.scrollHeight - t2.scrollTop) === t2.clientHeight;
          }, e.areTopsMisaligned = function(t2) {
            var e2 = /* @__PURE__ */ new Set();
            return [].forEach.call(t2, function(t3) {
              return e2.add(t3.offsetTop);
            }), 1 < e2.size;
          };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCDialogFoundation = void 0;
          var s, a, c = n2(23), u = n2(0), l = n2(62);
          (a = s = s || {}).POLL_SCROLL_POS = "poll_scroll_position", a.POLL_LAYOUT_CHANGE = "poll_layout_change";
          var d, p2 = (d = u.MDCFoundation, r2(h, d), Object.defineProperty(h, "cssClasses", { get: function() {
            return l.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "strings", { get: function() {
            return l.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "numbers", { get: function() {
            return l.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "defaultAdapter", { get: function() {
            return { addBodyClass: function() {
            }, addClass: function() {
            }, areButtonsStacked: function() {
              return false;
            }, clickDefaultButton: function() {
            }, eventTargetMatches: function() {
              return false;
            }, getActionFromEvent: function() {
              return "";
            }, getInitialFocusEl: function() {
              return null;
            }, hasClass: function() {
              return false;
            }, isContentScrollable: function() {
              return false;
            }, notifyClosed: function() {
            }, notifyClosing: function() {
            }, notifyOpened: function() {
            }, notifyOpening: function() {
            }, releaseFocus: function() {
            }, removeBodyClass: function() {
            }, removeClass: function() {
            }, reverseButtons: function() {
            }, trapFocus: function() {
            }, registerContentEventHandler: function() {
            }, deregisterContentEventHandler: function() {
            }, isScrollableContentAtTop: function() {
              return false;
            }, isScrollableContentAtBottom: function() {
              return false;
            }, registerWindowEventHandler: function() {
            }, deregisterWindowEventHandler: function() {
            } };
          }, enumerable: false, configurable: true }), h.prototype.init = function() {
            this.adapter.hasClass(l.cssClasses.STACKED) && this.setAutoStackButtons(false), this.isFullscreen = this.adapter.hasClass(l.cssClasses.FULLSCREEN);
          }, h.prototype.destroy = function() {
            this.animationTimer && (clearTimeout(this.animationTimer), this.handleAnimationTimerEnd()), this.isFullscreen && this.adapter.deregisterContentEventHandler("scroll", this.contentScrollHandler), this.animFrame.cancelAll(), this.adapter.deregisterWindowEventHandler("resize", this.windowResizeHandler), this.adapter.deregisterWindowEventHandler("orientationchange", this.windowOrientationChangeHandler);
          }, h.prototype.open = function(t2) {
            var e2 = this;
            this.dialogOpen = true, this.adapter.notifyOpening(), this.adapter.addClass(l.cssClasses.OPENING), this.isFullscreen && this.adapter.registerContentEventHandler("scroll", this.contentScrollHandler), t2 && t2.isAboveFullscreenDialog && this.adapter.addClass(l.cssClasses.SCRIM_HIDDEN), this.adapter.registerWindowEventHandler("resize", this.windowResizeHandler), this.adapter.registerWindowEventHandler("orientationchange", this.windowOrientationChangeHandler), this.runNextAnimationFrame(function() {
              e2.adapter.addClass(l.cssClasses.OPEN), e2.adapter.addBodyClass(l.cssClasses.SCROLL_LOCK), e2.layout(), e2.animationTimer = setTimeout(function() {
                e2.handleAnimationTimerEnd(), e2.adapter.trapFocus(e2.adapter.getInitialFocusEl()), e2.adapter.notifyOpened();
              }, l.numbers.DIALOG_ANIMATION_OPEN_TIME_MS);
            });
          }, h.prototype.close = function(t2) {
            var e2 = this;
            void 0 === t2 && (t2 = ""), this.dialogOpen && (this.dialogOpen = false, this.adapter.notifyClosing(t2), this.adapter.addClass(l.cssClasses.CLOSING), this.adapter.removeClass(l.cssClasses.OPEN), this.adapter.removeBodyClass(l.cssClasses.SCROLL_LOCK), this.isFullscreen && this.adapter.deregisterContentEventHandler("scroll", this.contentScrollHandler), this.adapter.deregisterWindowEventHandler("resize", this.windowResizeHandler), this.adapter.deregisterWindowEventHandler("orientationchange", this.windowOrientationChangeHandler), cancelAnimationFrame(this.animationFrame), this.animationFrame = 0, clearTimeout(this.animationTimer), this.animationTimer = setTimeout(function() {
              e2.adapter.releaseFocus(), e2.handleAnimationTimerEnd(), e2.adapter.notifyClosed(t2);
            }, l.numbers.DIALOG_ANIMATION_CLOSE_TIME_MS));
          }, h.prototype.showSurfaceScrim = function() {
            var t2 = this;
            this.adapter.addClass(l.cssClasses.SURFACE_SCRIM_SHOWING), this.runNextAnimationFrame(function() {
              t2.adapter.addClass(l.cssClasses.SURFACE_SCRIM_SHOWN);
            });
          }, h.prototype.hideSurfaceScrim = function() {
            this.adapter.removeClass(l.cssClasses.SURFACE_SCRIM_SHOWN), this.adapter.addClass(l.cssClasses.SURFACE_SCRIM_HIDING);
          }, h.prototype.handleSurfaceScrimTransitionEnd = function() {
            this.adapter.removeClass(l.cssClasses.SURFACE_SCRIM_HIDING), this.adapter.removeClass(l.cssClasses.SURFACE_SCRIM_SHOWING);
          }, h.prototype.isOpen = function() {
            return this.dialogOpen;
          }, h.prototype.getEscapeKeyAction = function() {
            return this.escapeKeyAction;
          }, h.prototype.setEscapeKeyAction = function(t2) {
            this.escapeKeyAction = t2;
          }, h.prototype.getScrimClickAction = function() {
            return this.scrimClickAction;
          }, h.prototype.setScrimClickAction = function(t2) {
            this.scrimClickAction = t2;
          }, h.prototype.getAutoStackButtons = function() {
            return this.autoStackButtons;
          }, h.prototype.setAutoStackButtons = function(t2) {
            this.autoStackButtons = t2;
          }, h.prototype.getSuppressDefaultPressSelector = function() {
            return this.suppressDefaultPressSelector;
          }, h.prototype.setSuppressDefaultPressSelector = function(t2) {
            this.suppressDefaultPressSelector = t2;
          }, h.prototype.layout = function() {
            var t2 = this;
            this.animFrame.request(s.POLL_LAYOUT_CHANGE, function() {
              t2.layoutInternal();
            });
          }, h.prototype.handleClick = function(t2) {
            if (this.adapter.eventTargetMatches(t2.target, l.strings.SCRIM_SELECTOR) && "" !== this.scrimClickAction) this.close(this.scrimClickAction);
            else {
              var e2 = this.adapter.getActionFromEvent(t2);
              e2 && this.close(e2);
            }
          }, h.prototype.handleKeydown = function(t2) {
            var e2 = "Enter" === t2.key || 13 === t2.keyCode;
            if (e2 && !this.adapter.getActionFromEvent(t2)) {
              var n3 = t2.composedPath ? t2.composedPath()[0] : t2.target, i4 = !this.suppressDefaultPressSelector || !this.adapter.eventTargetMatches(n3, this.suppressDefaultPressSelector);
              e2 && i4 && this.adapter.clickDefaultButton();
            }
          }, h.prototype.handleDocumentKeydown = function(t2) {
            "Escape" !== t2.key && 27 !== t2.keyCode || "" === this.escapeKeyAction || this.close(this.escapeKeyAction);
          }, h.prototype.handleScrollEvent = function() {
            var t2 = this;
            this.animFrame.request(s.POLL_SCROLL_POS, function() {
              t2.toggleScrollDividerHeader(), t2.toggleScrollDividerFooter();
            });
          }, h.prototype.layoutInternal = function() {
            this.autoStackButtons && this.detectStackedButtons(), this.toggleScrollableClasses();
          }, h.prototype.handleAnimationTimerEnd = function() {
            this.animationTimer = 0, this.adapter.removeClass(l.cssClasses.OPENING), this.adapter.removeClass(l.cssClasses.CLOSING);
          }, h.prototype.runNextAnimationFrame = function(t2) {
            var e2 = this;
            cancelAnimationFrame(this.animationFrame), this.animationFrame = requestAnimationFrame(function() {
              e2.animationFrame = 0, clearTimeout(e2.animationTimer), e2.animationTimer = setTimeout(t2, 0);
            });
          }, h.prototype.detectStackedButtons = function() {
            this.adapter.removeClass(l.cssClasses.STACKED);
            var t2 = this.adapter.areButtonsStacked();
            t2 && this.adapter.addClass(l.cssClasses.STACKED), t2 !== this.areButtonsStacked && (this.adapter.reverseButtons(), this.areButtonsStacked = t2);
          }, h.prototype.toggleScrollableClasses = function() {
            this.adapter.removeClass(l.cssClasses.SCROLLABLE), this.adapter.isContentScrollable() && (this.adapter.addClass(l.cssClasses.SCROLLABLE), this.isFullscreen && (this.toggleScrollDividerHeader(), this.toggleScrollDividerFooter()));
          }, h.prototype.toggleScrollDividerHeader = function() {
            this.adapter.isScrollableContentAtTop() ? this.adapter.hasClass(l.cssClasses.SCROLL_DIVIDER_HEADER) && this.adapter.removeClass(l.cssClasses.SCROLL_DIVIDER_HEADER) : this.adapter.addClass(l.cssClasses.SCROLL_DIVIDER_HEADER);
          }, h.prototype.toggleScrollDividerFooter = function() {
            this.adapter.isScrollableContentAtBottom() ? this.adapter.hasClass(l.cssClasses.SCROLL_DIVIDER_FOOTER) && this.adapter.removeClass(l.cssClasses.SCROLL_DIVIDER_FOOTER) : this.adapter.addClass(l.cssClasses.SCROLL_DIVIDER_FOOTER);
          }, h);
          function h(t2) {
            var e2 = d.call(this, o(o({}, h.defaultAdapter), t2)) || this;
            return e2.dialogOpen = false, e2.isFullscreen = false, e2.animationFrame = 0, e2.animationTimer = 0, e2.escapeKeyAction = l.strings.CLOSE_ACTION, e2.scrimClickAction = l.strings.CLOSE_ACTION, e2.autoStackButtons = true, e2.areButtonsStacked = false, e2.suppressDefaultPressSelector = l.strings.SUPPRESS_DEFAULT_PRESS_SELECTOR, e2.animFrame = new c.AnimationFrame(), e2.contentScrollHandler = function() {
              e2.handleScrollEvent();
            }, e2.windowResizeHandler = function() {
              e2.layout();
            }, e2.windowOrientationChangeHandler = function() {
              e2.layout();
            }, e2;
          }
          e.MDCDialogFoundation = p2, e.default = p2;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.numbers = e.strings = e.cssClasses = void 0, e.cssClasses = { CLOSING: "mdc-dialog--closing", OPEN: "mdc-dialog--open", OPENING: "mdc-dialog--opening", SCROLLABLE: "mdc-dialog--scrollable", SCROLL_LOCK: "mdc-dialog-scroll-lock", STACKED: "mdc-dialog--stacked", FULLSCREEN: "mdc-dialog--fullscreen", SCROLL_DIVIDER_HEADER: "mdc-dialog-scroll-divider-header", SCROLL_DIVIDER_FOOTER: "mdc-dialog-scroll-divider-footer", SURFACE_SCRIM_SHOWN: "mdc-dialog__surface-scrim--shown", SURFACE_SCRIM_SHOWING: "mdc-dialog__surface-scrim--showing", SURFACE_SCRIM_HIDING: "mdc-dialog__surface-scrim--hiding", SCRIM_HIDDEN: "mdc-dialog__scrim--hidden" }, e.strings = { ACTION_ATTRIBUTE: "data-mdc-dialog-action", BUTTON_DEFAULT_ATTRIBUTE: "data-mdc-dialog-button-default", BUTTON_SELECTOR: ".mdc-dialog__button", CLOSED_EVENT: "MDCDialog:closed", CLOSE_ACTION: "close", CLOSING_EVENT: "MDCDialog:closing", CONTAINER_SELECTOR: ".mdc-dialog__container", CONTENT_SELECTOR: ".mdc-dialog__content", DESTROY_ACTION: "destroy", INITIAL_FOCUS_ATTRIBUTE: "data-mdc-dialog-initial-focus", OPENED_EVENT: "MDCDialog:opened", OPENING_EVENT: "MDCDialog:opening", SCRIM_SELECTOR: ".mdc-dialog__scrim", SUPPRESS_DEFAULT_PRESS_SELECTOR: ["textarea", ".mdc-menu .mdc-list-item", ".mdc-menu .mdc-deprecated-list-item"].join(", "), SURFACE_SELECTOR: ".mdc-dialog__surface" }, e.numbers = { DIALOG_ANIMATION_CLOSE_TIME_MS: 75, DIALOG_ANIMATION_OPEN_TIME_MS: 150 };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.createFocusTrapInstance = void 0, e.createFocusTrapInstance = function(t2, e2) {
            return e2(t2, { skipInitialFocus: true });
          };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.preventDefaultEvent = void 0;
          var i3 = ["input", "button", "textarea", "select"];
          e.preventDefaultEvent = function(t2) {
            var e2 = t2.target;
            if (e2) {
              var n3 = ("" + e2.tagName).toLowerCase();
              -1 === i3.indexOf(n3) && t2.preventDefault();
            }
          };
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.cssClasses = void 0;
          e.cssClasses = { ANIMATE: "mdc-drawer--animate", CLOSING: "mdc-drawer--closing", DISMISSIBLE: "mdc-drawer--dismissible", MODAL: "mdc-drawer--modal", OPEN: "mdc-drawer--open", OPENING: "mdc-drawer--opening", ROOT: "mdc-drawer" };
          e.strings = { APP_CONTENT_SELECTOR: ".mdc-drawer-app-content", CLOSE_EVENT: "MDCDrawer:closed", OPEN_EVENT: "MDCDrawer:opened", SCRIM_SELECTOR: ".mdc-drawer-scrim", LIST_SELECTOR: ".mdc-list,.mdc-deprecated-list", LIST_ITEM_ACTIVATED_SELECTOR: ".mdc-list-item--activated,.mdc-deprecated-list-item--activated" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCModalDrawerFoundation = void 0;
          var o, s = n2(26), a = (o = s.MDCDismissibleDrawerFoundation, r2(c, o), c.prototype.handleScrimClick = function() {
            this.close();
          }, c.prototype.opened = function() {
            this.adapter.trapFocus();
          }, c.prototype.closed = function() {
            this.adapter.releaseFocus();
          }, c);
          function c() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCModalDrawerFoundation = a, e.default = a;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = void 0, e.cssClasses = { LABEL_FLOAT_ABOVE: "mdc-floating-label--float-above", LABEL_REQUIRED: "mdc-floating-label--required", LABEL_SHAKE: "mdc-floating-label--shake", ROOT: "mdc-floating-label" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCFormFieldFoundation = void 0;
          var s, a = n2(0), c = n2(69), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { activateInputRipple: function() {
            }, deactivateInputRipple: function() {
            }, deregisterInteractionHandler: function() {
            }, registerInteractionHandler: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.init = function() {
            this.adapter.registerInteractionHandler("click", this.click);
          }, l.prototype.destroy = function() {
            this.adapter.deregisterInteractionHandler("click", this.click);
          }, l.prototype.handleClick = function() {
            var t2 = this;
            this.adapter.activateInputRipple(), requestAnimationFrame(function() {
              t2.adapter.deactivateInputRipple();
            });
          }, l);
          function l(t2) {
            var e2 = s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
            return e2.click = function() {
              e2.handleClick();
            }, e2;
          }
          e.MDCFormFieldFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.cssClasses = void 0, e.cssClasses = { ROOT: "mdc-form-field" }, e.strings = { LABEL_SELECTOR: ".mdc-form-field > label" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCIconButtonToggleFoundation = void 0;
          var s, a = n2(0), c = n2(71), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, hasClass: function() {
              return false;
            }, notifyChange: function() {
            }, removeClass: function() {
            }, getAttr: function() {
              return null;
            }, setAttr: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.init = function() {
            var t2 = this.adapter.getAttr(c.strings.DATA_ARIA_LABEL_ON), e2 = this.adapter.getAttr(c.strings.DATA_ARIA_LABEL_OFF);
            if (t2 && e2) {
              if (null !== this.adapter.getAttr(c.strings.ARIA_PRESSED)) throw new Error("MDCIconButtonToggleFoundation: Button should not set `aria-pressed` if it has a toggled aria label.");
              this.hasToggledAriaLabel = true;
            } else this.adapter.setAttr(c.strings.ARIA_PRESSED, String(this.isOn()));
          }, l.prototype.handleClick = function() {
            this.toggle(), this.adapter.notifyChange({ isOn: this.isOn() });
          }, l.prototype.isOn = function() {
            return this.adapter.hasClass(c.cssClasses.ICON_BUTTON_ON);
          }, l.prototype.toggle = function(t2) {
            if (void 0 === t2 && (t2 = !this.isOn()), t2 ? this.adapter.addClass(c.cssClasses.ICON_BUTTON_ON) : this.adapter.removeClass(c.cssClasses.ICON_BUTTON_ON), this.hasToggledAriaLabel) {
              var e2 = t2 ? this.adapter.getAttr(c.strings.DATA_ARIA_LABEL_ON) : this.adapter.getAttr(c.strings.DATA_ARIA_LABEL_OFF);
              this.adapter.setAttr(c.strings.ARIA_LABEL, e2 || "");
            } else this.adapter.setAttr(c.strings.ARIA_PRESSED, "" + t2);
          }, l);
          function l(t2) {
            var e2 = s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
            return e2.hasToggledAriaLabel = false, e2;
          }
          e.MDCIconButtonToggleFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.cssClasses = void 0, e.cssClasses = { ICON_BUTTON_ON: "mdc-icon-button--on", ROOT: "mdc-icon-button" }, e.strings = { ARIA_LABEL: "aria-label", ARIA_PRESSED: "aria-pressed", DATA_ARIA_LABEL_OFF: "data-aria-label-off", DATA_ARIA_LABEL_ON: "data-aria-label-on", CHANGE_EVENT: "MDCIconButtonToggle:change" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCLineRippleFoundation = void 0;
          var s, a = n2(0), c = n2(73), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return false;
            }, setStyle: function() {
            }, registerEventHandler: function() {
            }, deregisterEventHandler: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.init = function() {
            this.adapter.registerEventHandler("transitionend", this.transitionEndHandler);
          }, l.prototype.destroy = function() {
            this.adapter.deregisterEventHandler("transitionend", this.transitionEndHandler);
          }, l.prototype.activate = function() {
            this.adapter.removeClass(c.cssClasses.LINE_RIPPLE_DEACTIVATING), this.adapter.addClass(c.cssClasses.LINE_RIPPLE_ACTIVE);
          }, l.prototype.setRippleCenter = function(t2) {
            this.adapter.setStyle("transform-origin", t2 + "px center");
          }, l.prototype.deactivate = function() {
            this.adapter.addClass(c.cssClasses.LINE_RIPPLE_DEACTIVATING);
          }, l.prototype.handleTransitionEnd = function(t2) {
            var e2 = this.adapter.hasClass(c.cssClasses.LINE_RIPPLE_DEACTIVATING);
            "opacity" === t2.propertyName && e2 && (this.adapter.removeClass(c.cssClasses.LINE_RIPPLE_ACTIVE), this.adapter.removeClass(c.cssClasses.LINE_RIPPLE_DEACTIVATING));
          }, l);
          function l(t2) {
            var e2 = s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
            return e2.transitionEndHandler = function(t3) {
              e2.handleTransitionEnd(t3);
            }, e2;
          }
          e.MDCLineRippleFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = void 0;
          e.cssClasses = { LINE_RIPPLE_ACTIVE: "mdc-line-ripple--active", LINE_RIPPLE_DEACTIVATING: "mdc-line-ripple--deactivating" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCMenuSurface = void 0;
          var o, s = n2(1), a = n2(8), c = n2(14), u = n2(10), l = (o = s.MDCComponent, r2(d, o), d.attachTo = function(t2) {
            return new d(t2);
          }, d.prototype.initialSyncWithDOM = function() {
            var e2 = this, t2 = this.root.parentElement;
            this.anchorElement = t2 && t2.classList.contains(a.cssClasses.ANCHOR) ? t2 : null, this.root.classList.contains(a.cssClasses.FIXED) && this.setFixedPosition(true), this.handleKeydown = function(t3) {
              e2.foundation.handleKeydown(t3);
            }, this.handleBodyClick = function(t3) {
              e2.foundation.handleBodyClick(t3);
            }, this.registerBodyClickListener = function() {
              document.body.addEventListener("click", e2.handleBodyClick, { capture: true });
            }, this.deregisterBodyClickListener = function() {
              document.body.removeEventListener("click", e2.handleBodyClick, { capture: true });
            }, this.listen("keydown", this.handleKeydown), this.listen(a.strings.OPENED_EVENT, this.registerBodyClickListener), this.listen(a.strings.CLOSED_EVENT, this.deregisterBodyClickListener);
          }, d.prototype.destroy = function() {
            this.unlisten("keydown", this.handleKeydown), this.unlisten(a.strings.OPENED_EVENT, this.registerBodyClickListener), this.unlisten(a.strings.CLOSED_EVENT, this.deregisterBodyClickListener), o.prototype.destroy.call(this);
          }, d.prototype.isOpen = function() {
            return this.foundation.isOpen();
          }, d.prototype.open = function() {
            this.foundation.open();
          }, d.prototype.close = function(t2) {
            void 0 === t2 && (t2 = false), this.foundation.close(t2);
          }, Object.defineProperty(d.prototype, "quickOpen", { set: function(t2) {
            this.foundation.setQuickOpen(t2);
          }, enumerable: false, configurable: true }), d.prototype.setIsHoisted = function(t2) {
            this.foundation.setIsHoisted(t2);
          }, d.prototype.setMenuSurfaceAnchorElement = function(t2) {
            this.anchorElement = t2;
          }, d.prototype.setFixedPosition = function(t2) {
            t2 ? this.root.classList.add(a.cssClasses.FIXED) : this.root.classList.remove(a.cssClasses.FIXED), this.foundation.setFixedPosition(t2);
          }, d.prototype.setAbsolutePosition = function(t2, e2) {
            this.foundation.setAbsolutePosition(t2, e2), this.setIsHoisted(true);
          }, d.prototype.setAnchorCorner = function(t2) {
            this.foundation.setAnchorCorner(t2);
          }, d.prototype.setAnchorMargin = function(t2) {
            this.foundation.setAnchorMargin(t2);
          }, d.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, hasAnchor: function() {
              return !!n3.anchorElement;
            }, notifyClose: function() {
              return n3.emit(c.MDCMenuSurfaceFoundation.strings.CLOSED_EVENT, {});
            }, notifyClosing: function() {
              n3.emit(c.MDCMenuSurfaceFoundation.strings.CLOSING_EVENT, {});
            }, notifyOpen: function() {
              return n3.emit(c.MDCMenuSurfaceFoundation.strings.OPENED_EVENT, {});
            }, notifyOpening: function() {
              return n3.emit(c.MDCMenuSurfaceFoundation.strings.OPENING_EVENT, {});
            }, isElementInContainer: function(t3) {
              return n3.root.contains(t3);
            }, isRtl: function() {
              return "rtl" === getComputedStyle(n3.root).getPropertyValue("direction");
            }, setTransformOrigin: function(t3) {
              var e2 = u.getCorrectPropertyName(window, "transform") + "-origin";
              n3.root.style.setProperty(e2, t3);
            }, isFocused: function() {
              return document.activeElement === n3.root;
            }, saveFocus: function() {
              n3.previousFocus = document.activeElement;
            }, restoreFocus: function() {
              n3.root.contains(document.activeElement) && n3.previousFocus && n3.previousFocus.focus && n3.previousFocus.focus();
            }, getInnerDimensions: function() {
              return { width: n3.root.offsetWidth, height: n3.root.offsetHeight };
            }, getAnchorDimensions: function() {
              return n3.anchorElement ? n3.anchorElement.getBoundingClientRect() : null;
            }, getWindowDimensions: function() {
              return { width: window.innerWidth, height: window.innerHeight };
            }, getBodyDimensions: function() {
              return { width: document.body.clientWidth, height: document.body.clientHeight };
            }, getWindowScroll: function() {
              return { x: window.pageXOffset, y: window.pageYOffset };
            }, setPosition: function(t3) {
              var e2 = n3.root;
              e2.style.left = "left" in t3 ? t3.left + "px" : "", e2.style.right = "right" in t3 ? t3.right + "px" : "", e2.style.top = "top" in t3 ? t3.top + "px" : "", e2.style.bottom = "bottom" in t3 ? t3.bottom + "px" : "";
            }, setMaxHeight: function(t3) {
              n3.root.style.maxHeight = t3;
            } };
            return new c.MDCMenuSurfaceFoundation(t2);
          }, d);
          function d() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCMenuSurface = l;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCMenu = void 0;
          var o, s = n2(1), a = n2(3), c = n2(24), u = n2(7), l = n2(25), d = n2(74), p2 = n2(14), h = n2(15), f = n2(76), y = (o = s.MDCComponent, r2(C, o), C.attachTo = function(t2) {
            return new C(t2);
          }, C.prototype.initialize = function(t2, e2) {
            void 0 === t2 && (t2 = function(t3) {
              return new d.MDCMenuSurface(t3);
            }), void 0 === e2 && (e2 = function(t3) {
              return new c.MDCList(t3);
            }), this.menuSurfaceFactory = t2, this.listFactory = e2;
          }, C.prototype.initialSyncWithDOM = function() {
            var e2 = this;
            this.menuSurface = this.menuSurfaceFactory(this.root);
            var t2 = this.root.querySelector(h.strings.LIST_SELECTOR);
            t2 ? (this.list = this.listFactory(t2), this.list.wrapFocus = true) : this.list = null, this.handleKeydown = function(t3) {
              e2.foundation.handleKeydown(t3);
            }, this.handleItemAction = function(t3) {
              e2.foundation.handleItemAction(e2.items[t3.detail.index]);
            }, this.handleMenuSurfaceOpened = function() {
              e2.foundation.handleMenuSurfaceOpened();
            }, this.menuSurface.listen(p2.MDCMenuSurfaceFoundation.strings.OPENED_EVENT, this.handleMenuSurfaceOpened), this.listen("keydown", this.handleKeydown), this.listen(l.MDCListFoundation.strings.ACTION_EVENT, this.handleItemAction);
          }, C.prototype.destroy = function() {
            this.list && this.list.destroy(), this.menuSurface.destroy(), this.menuSurface.unlisten(p2.MDCMenuSurfaceFoundation.strings.OPENED_EVENT, this.handleMenuSurfaceOpened), this.unlisten("keydown", this.handleKeydown), this.unlisten(l.MDCListFoundation.strings.ACTION_EVENT, this.handleItemAction), o.prototype.destroy.call(this);
          }, Object.defineProperty(C.prototype, "open", { get: function() {
            return this.menuSurface.isOpen();
          }, set: function(t2) {
            t2 ? this.menuSurface.open() : this.menuSurface.close();
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "wrapFocus", { get: function() {
            return !!this.list && this.list.wrapFocus;
          }, set: function(t2) {
            this.list && (this.list.wrapFocus = t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "hasTypeahead", { set: function(t2) {
            this.list && (this.list.hasTypeahead = t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "typeaheadInProgress", { get: function() {
            return !!this.list && this.list.typeaheadInProgress;
          }, enumerable: false, configurable: true }), C.prototype.typeaheadMatchItem = function(t2, e2) {
            return this.list ? this.list.typeaheadMatchItem(t2, e2) : -1;
          }, C.prototype.layout = function() {
            this.list && this.list.layout();
          }, Object.defineProperty(C.prototype, "items", { get: function() {
            return this.list ? this.list.listElements : [];
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "singleSelection", { set: function(t2) {
            this.list && (this.list.singleSelection = t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "selectedIndex", { get: function() {
            return this.list ? this.list.selectedIndex : u.numbers.UNSET_INDEX;
          }, set: function(t2) {
            this.list && (this.list.selectedIndex = t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "quickOpen", { set: function(t2) {
            this.menuSurface.quickOpen = t2;
          }, enumerable: false, configurable: true }), C.prototype.setDefaultFocusState = function(t2) {
            this.foundation.setDefaultFocusState(t2);
          }, C.prototype.setAnchorCorner = function(t2) {
            this.menuSurface.setAnchorCorner(t2);
          }, C.prototype.setAnchorMargin = function(t2) {
            this.menuSurface.setAnchorMargin(t2);
          }, C.prototype.setSelectedIndex = function(t2) {
            this.foundation.setSelectedIndex(t2);
          }, C.prototype.setEnabled = function(t2, e2) {
            this.foundation.setEnabled(t2, e2);
          }, C.prototype.getOptionByIndex = function(t2) {
            return t2 < this.items.length ? this.items[t2] : null;
          }, C.prototype.getPrimaryTextAtIndex = function(t2) {
            var e2 = this.getOptionByIndex(t2);
            return e2 && this.list && this.list.getPrimaryText(e2) || "";
          }, C.prototype.setFixedPosition = function(t2) {
            this.menuSurface.setFixedPosition(t2);
          }, C.prototype.setIsHoisted = function(t2) {
            this.menuSurface.setIsHoisted(t2);
          }, C.prototype.setAbsolutePosition = function(t2, e2) {
            this.menuSurface.setAbsolutePosition(t2, e2);
          }, C.prototype.setAnchorElement = function(t2) {
            this.menuSurface.anchorElement = t2;
          }, C.prototype.getDefaultFoundation = function() {
            var i4 = this, t2 = { addClassToElementAtIndex: function(t3, e2) {
              i4.items[t3].classList.add(e2);
            }, removeClassFromElementAtIndex: function(t3, e2) {
              i4.items[t3].classList.remove(e2);
            }, addAttributeToElementAtIndex: function(t3, e2, n3) {
              i4.items[t3].setAttribute(e2, n3);
            }, removeAttributeFromElementAtIndex: function(t3, e2) {
              i4.items[t3].removeAttribute(e2);
            }, getAttributeFromElementAtIndex: function(t3, e2) {
              return i4.items[t3].getAttribute(e2);
            }, elementContainsClass: function(t3, e2) {
              return t3.classList.contains(e2);
            }, closeSurface: function(t3) {
              i4.menuSurface.close(t3);
            }, getElementIndex: function(t3) {
              return i4.items.indexOf(t3);
            }, notifySelected: function(t3) {
              i4.emit(h.strings.SELECTED_EVENT, { index: t3.index, item: i4.items[t3.index] });
            }, getMenuItemCount: function() {
              return i4.items.length;
            }, focusItemAtIndex: function(t3) {
              i4.items[t3].focus();
            }, focusListRoot: function() {
              i4.root.querySelector(h.strings.LIST_SELECTOR).focus();
            }, isSelectableItemAtIndex: function(t3) {
              return !!a.closest(i4.items[t3], "." + h.cssClasses.MENU_SELECTION_GROUP);
            }, getSelectedSiblingOfItemAtIndex: function(t3) {
              var e2 = a.closest(i4.items[t3], "." + h.cssClasses.MENU_SELECTION_GROUP).querySelector("." + h.cssClasses.MENU_SELECTED_LIST_ITEM);
              return e2 ? i4.items.indexOf(e2) : -1;
            } };
            return new f.MDCMenuFoundation(t2);
          }, C);
          function C() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCMenu = y;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCMenuFoundation = void 0;
          var s, a = n2(0), c = n2(7), u = n2(14), l = n2(15), d = (s = a.MDCFoundation, r2(p2, s), Object.defineProperty(p2, "cssClasses", { get: function() {
            return l.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "strings", { get: function() {
            return l.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "numbers", { get: function() {
            return l.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "defaultAdapter", { get: function() {
            return { addClassToElementAtIndex: function() {
            }, removeClassFromElementAtIndex: function() {
            }, addAttributeToElementAtIndex: function() {
            }, removeAttributeFromElementAtIndex: function() {
            }, getAttributeFromElementAtIndex: function() {
              return null;
            }, elementContainsClass: function() {
              return false;
            }, closeSurface: function() {
            }, getElementIndex: function() {
              return -1;
            }, notifySelected: function() {
            }, getMenuItemCount: function() {
              return 0;
            }, focusItemAtIndex: function() {
            }, focusListRoot: function() {
            }, getSelectedSiblingOfItemAtIndex: function() {
              return -1;
            }, isSelectableItemAtIndex: function() {
              return false;
            } };
          }, enumerable: false, configurable: true }), p2.prototype.destroy = function() {
            this.closeAnimationEndTimerId && clearTimeout(this.closeAnimationEndTimerId), this.adapter.closeSurface();
          }, p2.prototype.handleKeydown = function(t2) {
            var e2 = t2.key, n3 = t2.keyCode;
            "Tab" !== e2 && 9 !== n3 || this.adapter.closeSurface(true);
          }, p2.prototype.handleItemAction = function(e2) {
            var n3 = this, t2 = this.adapter.getElementIndex(e2);
            if (!(t2 < 0)) {
              this.adapter.notifySelected({ index: t2 });
              var i4 = "true" === this.adapter.getAttributeFromElementAtIndex(t2, l.strings.SKIP_RESTORE_FOCUS);
              this.adapter.closeSurface(i4), this.closeAnimationEndTimerId = setTimeout(function() {
                var t3 = n3.adapter.getElementIndex(e2);
                0 <= t3 && n3.adapter.isSelectableItemAtIndex(t3) && n3.setSelectedIndex(t3);
              }, u.MDCMenuSurfaceFoundation.numbers.TRANSITION_CLOSE_DURATION);
            }
          }, p2.prototype.handleMenuSurfaceOpened = function() {
            switch (this.defaultFocusState) {
              case l.DefaultFocusState.FIRST_ITEM:
                this.adapter.focusItemAtIndex(0);
                break;
              case l.DefaultFocusState.LAST_ITEM:
                this.adapter.focusItemAtIndex(this.adapter.getMenuItemCount() - 1);
                break;
              case l.DefaultFocusState.NONE:
                break;
              default:
                this.adapter.focusListRoot();
            }
          }, p2.prototype.setDefaultFocusState = function(t2) {
            this.defaultFocusState = t2;
          }, p2.prototype.getSelectedIndex = function() {
            return this.selectedIndex;
          }, p2.prototype.setSelectedIndex = function(t2) {
            if (this.validatedIndex(t2), !this.adapter.isSelectableItemAtIndex(t2)) throw new Error("MDCMenuFoundation: No selection group at specified index.");
            var e2 = this.adapter.getSelectedSiblingOfItemAtIndex(t2);
            0 <= e2 && (this.adapter.removeAttributeFromElementAtIndex(e2, l.strings.ARIA_CHECKED_ATTR), this.adapter.removeClassFromElementAtIndex(e2, l.cssClasses.MENU_SELECTED_LIST_ITEM)), this.adapter.addClassToElementAtIndex(t2, l.cssClasses.MENU_SELECTED_LIST_ITEM), this.adapter.addAttributeToElementAtIndex(t2, l.strings.ARIA_CHECKED_ATTR, "true"), this.selectedIndex = t2;
          }, p2.prototype.setEnabled = function(t2, e2) {
            this.validatedIndex(t2), e2 ? (this.adapter.removeClassFromElementAtIndex(t2, c.cssClasses.LIST_ITEM_DISABLED_CLASS), this.adapter.addAttributeToElementAtIndex(t2, l.strings.ARIA_DISABLED_ATTR, "false")) : (this.adapter.addClassToElementAtIndex(t2, c.cssClasses.LIST_ITEM_DISABLED_CLASS), this.adapter.addAttributeToElementAtIndex(t2, l.strings.ARIA_DISABLED_ATTR, "true"));
          }, p2.prototype.validatedIndex = function(t2) {
            var e2 = this.adapter.getMenuItemCount();
            if (!(0 <= t2 && t2 < e2)) throw new Error("MDCMenuFoundation: No list item at specified index.");
          }, p2);
          function p2(t2) {
            var e2 = s.call(this, o(o({}, p2.defaultAdapter), t2)) || this;
            return e2.closeAnimationEndTimerId = 0, e2.defaultFocusState = l.DefaultFocusState.LIST_ROOT, e2.selectedIndex = -1, e2;
          }
          e.MDCMenuFoundation = d, e.default = d;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCNotchedOutlineFoundation = void 0;
          var s, a = n2(0), c = n2(31), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "numbers", { get: function() {
            return c.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, setNotchWidthProperty: function() {
            }, removeNotchWidthProperty: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.notch = function(t2) {
            var e2 = l.cssClasses.OUTLINE_NOTCHED;
            0 < t2 && (t2 += c.numbers.NOTCH_ELEMENT_PADDING), this.adapter.setNotchWidthProperty(t2), this.adapter.addClass(e2);
          }, l.prototype.closeNotch = function() {
            var t2 = l.cssClasses.OUTLINE_NOTCHED;
            this.adapter.removeClass(t2), this.adapter.removeNotchWidthProperty();
          }, l);
          function l(t2) {
            return s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
          }
          e.MDCNotchedOutlineFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCRadioFoundation = void 0;
          var s, a = n2(0), c = n2(79), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, setNativeControlDisabled: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.setDisabled = function(t2) {
            var e2 = l.cssClasses.DISABLED;
            this.adapter.setNativeControlDisabled(t2), t2 ? this.adapter.addClass(e2) : this.adapter.removeClass(e2);
          }, l);
          function l(t2) {
            return s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
          }
          e.MDCRadioFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = e.strings = void 0;
          e.strings = { NATIVE_CONTROL_SELECTOR: ".mdc-radio__native-control" };
          e.cssClasses = { DISABLED: "mdc-radio--disabled", ROOT: "mdc-radio" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSegmentedButtonFoundation = void 0;
          var a, c = n2(0), u = n2(81), l = (a = c.MDCFoundation, r2(d, a), Object.defineProperty(d, "defaultAdapter", { get: function() {
            return { hasClass: function() {
              return false;
            }, getSegments: function() {
              return [];
            }, selectSegment: function() {
            }, unselectSegment: function() {
            }, notifySelectedChange: function() {
            } };
          }, enumerable: false, configurable: true }), d.prototype.selectSegment = function(t2) {
            this.adapter.selectSegment(t2);
          }, d.prototype.unselectSegment = function(t2) {
            this.adapter.unselectSegment(t2);
          }, d.prototype.getSelectedSegments = function() {
            return this.adapter.getSegments().filter(function(t2) {
              return t2.selected;
            });
          }, d.prototype.isSegmentSelected = function(e2) {
            return this.adapter.getSegments().some(function(t2) {
              return (t2.index === e2 || t2.segmentId === e2) && t2.selected;
            });
          }, d.prototype.isSingleSelect = function() {
            return this.adapter.hasClass(u.cssClasses.SINGLE_SELECT);
          }, d.prototype.handleSelected = function(t2) {
            this.isSingleSelect() && this.unselectPrevSelected(t2.index), this.adapter.notifySelectedChange(t2);
          }, d.prototype.unselectPrevSelected = function(t2) {
            var e2, n3;
            try {
              for (var i4 = s(this.getSelectedSegments()), r3 = i4.next(); !r3.done; r3 = i4.next()) {
                var o2 = r3.value;
                o2.index !== t2 && this.unselectSegment(o2.index);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                r3 && !r3.done && (n3 = i4.return) && n3.call(i4);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, d);
          function d(t2) {
            return a.call(this, o(o({}, d.defaultAdapter), t2)) || this;
          }
          e.MDCSegmentedButtonFoundation = l;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = e.events = e.selectors = void 0, e.selectors = { SEGMENT: ".mdc-segmented-button__segment" }, e.events = { SELECTED: "selected", CHANGE: "change" }, e.cssClasses = { SINGLE_SELECT: "mdc-segmented-button--single-select" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSegmentedButtonSegment = void 0;
          var s, a = n2(1), c = n2(2), u = n2(4), l = n2(83), d = n2(84), p2 = (s = a.MDCComponent, r2(h, s), Object.defineProperty(h.prototype, "ripple", { get: function() {
            return this.rippleComponent;
          }, enumerable: false, configurable: true }), h.attachTo = function(t2) {
            return new h(t2);
          }, h.prototype.initialize = function(t2) {
            var e2 = this;
            void 0 === t2 && (t2 = function(t3, e3) {
              return new c.MDCRipple(t3, e3);
            });
            var n3 = o(o({}, c.MDCRipple.createAdapter(this)), { computeBoundingRect: function() {
              return e2.foundation.getDimensions();
            } });
            this.rippleComponent = t2(this.root, new u.MDCRippleFoundation(n3));
          }, h.prototype.initialSyncWithDOM = function() {
            var t2 = this;
            this.handleClick = function() {
              t2.foundation.handleClick();
            }, this.listen(l.events.CLICK, this.handleClick);
          }, h.prototype.destroy = function() {
            this.ripple.destroy(), this.unlisten(l.events.CLICK, this.handleClick), s.prototype.destroy.call(this);
          }, h.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { isSingleSelect: function() {
              return n3.isSingleSelect;
            }, getAttr: function(t3) {
              return n3.root.getAttribute(t3);
            }, setAttr: function(t3, e2) {
              n3.root.setAttribute(t3, e2);
            }, addClass: function(t3) {
              n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              n3.root.classList.remove(t3);
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, notifySelectedChange: function(t3) {
              n3.emit(l.events.SELECTED, { index: n3.index, selected: t3, segmentId: n3.getSegmentId() }, true);
            }, getRootBoundingClientRect: function() {
              return n3.root.getBoundingClientRect();
            } };
            return new d.MDCSegmentedButtonSegmentFoundation(t2);
          }, h.prototype.setIndex = function(t2) {
            this.index = t2;
          }, h.prototype.setIsSingleSelect = function(t2) {
            this.isSingleSelect = t2;
          }, h.prototype.isSelected = function() {
            return this.foundation.isSelected();
          }, h.prototype.setSelected = function() {
            this.foundation.setSelected();
          }, h.prototype.setUnselected = function() {
            this.foundation.setUnselected();
          }, h.prototype.getSegmentId = function() {
            return this.foundation.getSegmentId();
          }, h);
          function h() {
            return null !== s && s.apply(this, arguments) || this;
          }
          e.MDCSegmentedButtonSegment = p2;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = e.events = e.attributes = e.booleans = void 0, e.booleans = { TRUE: "true", FALSE: "false" }, e.attributes = { ARIA_CHECKED: "aria-checked", ARIA_PRESSED: "aria-pressed", DATA_SEGMENT_ID: "data-segment-id" }, e.events = { CLICK: "click", SELECTED: "selected" }, e.cssClasses = { SELECTED: "mdc-segmented-button__segment--selected" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSegmentedButtonSegmentFoundation = void 0;
          var s, a = n2(0), c = n2(83), u = { bottom: 0, height: 0, left: 0, right: 0, top: 0, width: 0 }, l = (s = a.MDCFoundation, r2(d, s), Object.defineProperty(d, "defaultAdapter", { get: function() {
            return { isSingleSelect: function() {
              return false;
            }, getAttr: function() {
              return "";
            }, setAttr: function() {
            }, addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return false;
            }, notifySelectedChange: function() {
            }, getRootBoundingClientRect: function() {
              return u;
            } };
          }, enumerable: false, configurable: true }), d.prototype.isSelected = function() {
            return this.adapter.hasClass(c.cssClasses.SELECTED);
          }, d.prototype.setSelected = function() {
            this.adapter.addClass(c.cssClasses.SELECTED), this.setAriaAttr(c.booleans.TRUE);
          }, d.prototype.setUnselected = function() {
            this.adapter.removeClass(c.cssClasses.SELECTED), this.setAriaAttr(c.booleans.FALSE);
          }, d.prototype.getSegmentId = function() {
            var t2;
            return null !== (t2 = this.adapter.getAttr(c.attributes.DATA_SEGMENT_ID)) && void 0 !== t2 ? t2 : void 0;
          }, d.prototype.handleClick = function() {
            this.adapter.isSingleSelect() ? this.setSelected() : this.toggleSelection(), this.adapter.notifySelectedChange(this.isSelected());
          }, d.prototype.getDimensions = function() {
            return this.adapter.getRootBoundingClientRect();
          }, d.prototype.toggleSelection = function() {
            this.isSelected() ? this.setUnselected() : this.setSelected();
          }, d.prototype.setAriaAttr = function(t2) {
            this.adapter.isSingleSelect() ? this.adapter.setAttr(c.attributes.ARIA_CHECKED, t2) : this.adapter.setAttr(c.attributes.ARIA_PRESSED, t2);
          }, d);
          function d(t2) {
            return s.call(this, o(o({}, d.defaultAdapter), t2)) || this;
          }
          e.MDCSegmentedButtonSegmentFoundation = l;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSelectFoundation = void 0;
          var s, a = n2(0), c = n2(6), u = n2(8), l = n2(32), d = (s = a.MDCFoundation, r2(p2, s), Object.defineProperty(p2, "cssClasses", { get: function() {
            return l.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "numbers", { get: function() {
            return l.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "strings", { get: function() {
            return l.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return false;
            }, activateBottomLine: function() {
            }, deactivateBottomLine: function() {
            }, getSelectedIndex: function() {
              return -1;
            }, setSelectedIndex: function() {
            }, hasLabel: function() {
              return false;
            }, floatLabel: function() {
            }, getLabelWidth: function() {
              return 0;
            }, setLabelRequired: function() {
            }, hasOutline: function() {
              return false;
            }, notchOutline: function() {
            }, closeOutline: function() {
            }, setRippleCenter: function() {
            }, notifyChange: function() {
            }, setSelectedText: function() {
            }, isSelectAnchorFocused: function() {
              return false;
            }, getSelectAnchorAttr: function() {
              return "";
            }, setSelectAnchorAttr: function() {
            }, removeSelectAnchorAttr: function() {
            }, addMenuClass: function() {
            }, removeMenuClass: function() {
            }, openMenu: function() {
            }, closeMenu: function() {
            }, getAnchorElement: function() {
              return null;
            }, setMenuAnchorElement: function() {
            }, setMenuAnchorCorner: function() {
            }, setMenuWrapFocus: function() {
            }, focusMenuItemAtIndex: function() {
            }, getMenuItemCount: function() {
              return 0;
            }, getMenuItemValues: function() {
              return [];
            }, getMenuItemTextAtIndex: function() {
              return "";
            }, isTypeaheadInProgress: function() {
              return false;
            }, typeaheadMatchItem: function() {
              return -1;
            } };
          }, enumerable: false, configurable: true }), p2.prototype.getSelectedIndex = function() {
            return this.adapter.getSelectedIndex();
          }, p2.prototype.setSelectedIndex = function(t2, e2, n3) {
            void 0 === e2 && (e2 = false), void 0 === n3 && (n3 = false), t2 >= this.adapter.getMenuItemCount() || (t2 === l.numbers.UNSET_INDEX ? this.adapter.setSelectedText("") : this.adapter.setSelectedText(this.adapter.getMenuItemTextAtIndex(t2).trim()), this.adapter.setSelectedIndex(t2), e2 && this.adapter.closeMenu(), n3 || this.lastSelectedIndex === t2 || this.handleChange(), this.lastSelectedIndex = t2);
          }, p2.prototype.setValue = function(t2, e2) {
            void 0 === e2 && (e2 = false);
            var n3 = this.adapter.getMenuItemValues().indexOf(t2);
            this.setSelectedIndex(n3, false, e2);
          }, p2.prototype.getValue = function() {
            var t2 = this.adapter.getSelectedIndex(), e2 = this.adapter.getMenuItemValues();
            return t2 !== l.numbers.UNSET_INDEX ? e2[t2] : "";
          }, p2.prototype.getDisabled = function() {
            return this.disabled;
          }, p2.prototype.setDisabled = function(t2) {
            this.disabled = t2, this.disabled ? (this.adapter.addClass(l.cssClasses.DISABLED), this.adapter.closeMenu()) : this.adapter.removeClass(l.cssClasses.DISABLED), this.leadingIcon && this.leadingIcon.setDisabled(this.disabled), this.disabled ? this.adapter.removeSelectAnchorAttr("tabindex") : this.adapter.setSelectAnchorAttr("tabindex", "0"), this.adapter.setSelectAnchorAttr("aria-disabled", this.disabled.toString());
          }, p2.prototype.openMenu = function() {
            this.adapter.addClass(l.cssClasses.ACTIVATED), this.adapter.openMenu(), this.isMenuOpen = true, this.adapter.setSelectAnchorAttr("aria-expanded", "true");
          }, p2.prototype.setHelperTextContent = function(t2) {
            this.helperText && this.helperText.setContent(t2);
          }, p2.prototype.layout = function() {
            if (this.adapter.hasLabel()) {
              var t2 = 0 < this.getValue().length, e2 = this.adapter.hasClass(l.cssClasses.FOCUSED), n3 = t2 || e2, i4 = this.adapter.hasClass(l.cssClasses.REQUIRED);
              this.notchOutline(n3), this.adapter.floatLabel(n3), this.adapter.setLabelRequired(i4);
            }
          }, p2.prototype.layoutOptions = function() {
            var t2 = this.adapter.getMenuItemValues().indexOf(this.getValue());
            this.setSelectedIndex(t2, false, true);
          }, p2.prototype.handleMenuOpened = function() {
            if (0 !== this.adapter.getMenuItemValues().length) {
              var t2 = this.getSelectedIndex(), e2 = 0 <= t2 ? t2 : 0;
              this.adapter.focusMenuItemAtIndex(e2);
            }
          }, p2.prototype.handleMenuClosing = function() {
            this.adapter.setSelectAnchorAttr("aria-expanded", "false");
          }, p2.prototype.handleMenuClosed = function() {
            this.adapter.removeClass(l.cssClasses.ACTIVATED), this.isMenuOpen = false, this.adapter.isSelectAnchorFocused() || this.blur();
          }, p2.prototype.handleChange = function() {
            this.layout(), this.adapter.notifyChange(this.getValue()), this.adapter.hasClass(l.cssClasses.REQUIRED) && this.useDefaultValidation && this.setValid(this.isValid());
          }, p2.prototype.handleMenuItemAction = function(t2) {
            this.setSelectedIndex(t2, true);
          }, p2.prototype.handleFocus = function() {
            this.adapter.addClass(l.cssClasses.FOCUSED), this.layout(), this.adapter.activateBottomLine();
          }, p2.prototype.handleBlur = function() {
            this.isMenuOpen || this.blur();
          }, p2.prototype.handleClick = function(t2) {
            this.disabled || this.recentlyClicked || (this.setClickDebounceTimeout(), this.isMenuOpen ? this.adapter.closeMenu() : (this.adapter.setRippleCenter(t2), this.openMenu()));
          }, p2.prototype.handleKeydown = function(t2) {
            if (!this.isMenuOpen && this.adapter.hasClass(l.cssClasses.FOCUSED)) {
              var e2 = c.normalizeKey(t2) === c.KEY.ENTER, n3 = c.normalizeKey(t2) === c.KEY.SPACEBAR, i4 = c.normalizeKey(t2) === c.KEY.ARROW_UP, r3 = c.normalizeKey(t2) === c.KEY.ARROW_DOWN;
              if (!t2.ctrlKey && !t2.metaKey && (!n3 && t2.key && 1 === t2.key.length || n3 && this.adapter.isTypeaheadInProgress())) {
                var o2 = n3 ? " " : t2.key, s2 = this.adapter.typeaheadMatchItem(o2, this.getSelectedIndex());
                return 0 <= s2 && this.setSelectedIndex(s2), void t2.preventDefault();
              }
              (e2 || n3 || i4 || r3) && (this.openMenu(), t2.preventDefault());
            }
          }, p2.prototype.notchOutline = function(t2) {
            if (this.adapter.hasOutline()) {
              var e2 = this.adapter.hasClass(l.cssClasses.FOCUSED);
              if (t2) {
                var n3 = l.numbers.LABEL_SCALE, i4 = this.adapter.getLabelWidth() * n3;
                this.adapter.notchOutline(i4);
              } else e2 || this.adapter.closeOutline();
            }
          }, p2.prototype.setLeadingIconAriaLabel = function(t2) {
            this.leadingIcon && this.leadingIcon.setAriaLabel(t2);
          }, p2.prototype.setLeadingIconContent = function(t2) {
            this.leadingIcon && this.leadingIcon.setContent(t2);
          }, p2.prototype.getUseDefaultValidation = function() {
            return this.useDefaultValidation;
          }, p2.prototype.setUseDefaultValidation = function(t2) {
            this.useDefaultValidation = t2;
          }, p2.prototype.setValid = function(t2) {
            this.useDefaultValidation || (this.customValidity = t2), this.adapter.setSelectAnchorAttr("aria-invalid", (!t2).toString()), t2 ? (this.adapter.removeClass(l.cssClasses.INVALID), this.adapter.removeMenuClass(l.cssClasses.MENU_INVALID)) : (this.adapter.addClass(l.cssClasses.INVALID), this.adapter.addMenuClass(l.cssClasses.MENU_INVALID)), this.syncHelperTextValidity(t2);
          }, p2.prototype.isValid = function() {
            return this.useDefaultValidation && this.adapter.hasClass(l.cssClasses.REQUIRED) && !this.adapter.hasClass(l.cssClasses.DISABLED) ? this.getSelectedIndex() !== l.numbers.UNSET_INDEX && (0 !== this.getSelectedIndex() || Boolean(this.getValue())) : this.customValidity;
          }, p2.prototype.setRequired = function(t2) {
            t2 ? this.adapter.addClass(l.cssClasses.REQUIRED) : this.adapter.removeClass(l.cssClasses.REQUIRED), this.adapter.setSelectAnchorAttr("aria-required", t2.toString()), this.adapter.setLabelRequired(t2);
          }, p2.prototype.getRequired = function() {
            return "true" === this.adapter.getSelectAnchorAttr("aria-required");
          }, p2.prototype.init = function() {
            var t2 = this.adapter.getAnchorElement();
            t2 && (this.adapter.setMenuAnchorElement(t2), this.adapter.setMenuAnchorCorner(u.Corner.BOTTOM_START)), this.adapter.setMenuWrapFocus(false), this.setDisabled(this.adapter.hasClass(l.cssClasses.DISABLED)), this.syncHelperTextValidity(!this.adapter.hasClass(l.cssClasses.INVALID)), this.layout(), this.layoutOptions();
          }, p2.prototype.blur = function() {
            this.adapter.removeClass(l.cssClasses.FOCUSED), this.layout(), this.adapter.deactivateBottomLine(), this.adapter.hasClass(l.cssClasses.REQUIRED) && this.useDefaultValidation && this.setValid(this.isValid());
          }, p2.prototype.syncHelperTextValidity = function(t2) {
            if (this.helperText) {
              this.helperText.setValidity(t2);
              var e2 = this.helperText.isVisible(), n3 = this.helperText.getId();
              e2 && n3 ? this.adapter.setSelectAnchorAttr(l.strings.ARIA_DESCRIBEDBY, n3) : this.adapter.removeSelectAnchorAttr(l.strings.ARIA_DESCRIBEDBY);
            }
          }, p2.prototype.setClickDebounceTimeout = function() {
            var t2 = this;
            clearTimeout(this.clickDebounceTimeout), this.clickDebounceTimeout = setTimeout(function() {
              t2.recentlyClicked = false;
            }, l.numbers.CLICK_DEBOUNCE_TIMEOUT_MS), this.recentlyClicked = true;
          }, p2);
          function p2(t2, e2) {
            void 0 === e2 && (e2 = {});
            var n3 = s.call(this, o(o({}, p2.defaultAdapter), t2)) || this;
            return n3.disabled = false, n3.isMenuOpen = false, n3.useDefaultValidation = true, n3.customValidity = true, n3.lastSelectedIndex = l.numbers.UNSET_INDEX, n3.clickDebounceTimeout = 0, n3.recentlyClicked = false, n3.leadingIcon = e2.leadingIcon, n3.helperText = e2.helperText, n3;
          }
          e.MDCSelectFoundation = d, e.default = d;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSelectHelperText = void 0;
          var o, s = n2(1), a = n2(87), c = (o = s.MDCComponent, r2(u, o), u.attachTo = function(t2) {
            return new u(t2);
          }, Object.defineProperty(u.prototype, "foundationForSelect", { get: function() {
            return this.foundation;
          }, enumerable: false, configurable: true }), u.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, getAttr: function(t3) {
              return n3.root.getAttribute(t3);
            }, setAttr: function(t3, e2) {
              return n3.root.setAttribute(t3, e2);
            }, removeAttr: function(t3) {
              return n3.root.removeAttribute(t3);
            }, setContent: function(t3) {
              n3.root.textContent = t3;
            } };
            return new a.MDCSelectHelperTextFoundation(t2);
          }, u);
          function u() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCSelectHelperText = c;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSelectHelperTextFoundation = void 0;
          var s, a = n2(0), c = n2(88), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return false;
            }, setAttr: function() {
            }, getAttr: function() {
              return null;
            }, removeAttr: function() {
            }, setContent: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.getId = function() {
            return this.adapter.getAttr("id");
          }, l.prototype.isVisible = function() {
            return "true" !== this.adapter.getAttr(c.strings.ARIA_HIDDEN);
          }, l.prototype.setContent = function(t2) {
            this.adapter.setContent(t2);
          }, l.prototype.setValidation = function(t2) {
            t2 ? this.adapter.addClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG) : this.adapter.removeClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG);
          }, l.prototype.setValidationMsgPersistent = function(t2) {
            t2 ? this.adapter.addClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG_PERSISTENT) : this.adapter.removeClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG_PERSISTENT);
          }, l.prototype.getIsValidation = function() {
            return this.adapter.hasClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG);
          }, l.prototype.getIsValidationMsgPersistent = function() {
            return this.adapter.hasClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG_PERSISTENT);
          }, l.prototype.setValidity = function(t2) {
            if (this.adapter.hasClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG)) {
              var e2 = this.adapter.hasClass(c.cssClasses.HELPER_TEXT_VALIDATION_MSG_PERSISTENT);
              if (!t2 || e2) return this.showToScreenReader(), void (t2 ? this.adapter.removeAttr(c.strings.ROLE) : this.adapter.setAttr(c.strings.ROLE, "alert"));
              this.adapter.removeAttr(c.strings.ROLE), this.hide();
            }
          }, l.prototype.showToScreenReader = function() {
            this.adapter.removeAttr(c.strings.ARIA_HIDDEN);
          }, l.prototype.hide = function() {
            this.adapter.setAttr(c.strings.ARIA_HIDDEN, "true");
          }, l);
          function l(t2) {
            return s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
          }
          e.MDCSelectHelperTextFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = e.strings = void 0;
          e.strings = { ARIA_HIDDEN: "aria-hidden", ROLE: "role" };
          e.cssClasses = { HELPER_TEXT_VALIDATION_MSG: "mdc-select-helper-text--validation-msg", HELPER_TEXT_VALIDATION_MSG_PERSISTENT: "mdc-select-helper-text--validation-msg-persistent" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSelectIcon = void 0;
          var o, s = n2(1), a = n2(90), c = (o = s.MDCComponent, r2(u, o), u.attachTo = function(t2) {
            return new u(t2);
          }, Object.defineProperty(u.prototype, "foundationForSelect", { get: function() {
            return this.foundation;
          }, enumerable: false, configurable: true }), u.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { getAttr: function(t3) {
              return n3.root.getAttribute(t3);
            }, setAttr: function(t3, e2) {
              return n3.root.setAttribute(t3, e2);
            }, removeAttr: function(t3) {
              return n3.root.removeAttribute(t3);
            }, setContent: function(t3) {
              n3.root.textContent = t3;
            }, registerInteractionHandler: function(t3, e2) {
              return n3.listen(t3, e2);
            }, deregisterInteractionHandler: function(t3, e2) {
              return n3.unlisten(t3, e2);
            }, notifyIconAction: function() {
              return n3.emit(a.MDCSelectIconFoundation.strings.ICON_EVENT, {}, true);
            } };
            return new a.MDCSelectIconFoundation(t2);
          }, u);
          function u() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCSelectIcon = c;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSelectIconFoundation = void 0;
          var a, c = n2(0), u = n2(91), l = ["click", "keydown"], d = (a = c.MDCFoundation, r2(p2, a), Object.defineProperty(p2, "strings", { get: function() {
            return u.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "defaultAdapter", { get: function() {
            return { getAttr: function() {
              return null;
            }, setAttr: function() {
            }, removeAttr: function() {
            }, setContent: function() {
            }, registerInteractionHandler: function() {
            }, deregisterInteractionHandler: function() {
            }, notifyIconAction: function() {
            } };
          }, enumerable: false, configurable: true }), p2.prototype.init = function() {
            var e2, t2;
            this.savedTabIndex = this.adapter.getAttr("tabindex");
            try {
              for (var n3 = s(l), i4 = n3.next(); !i4.done; i4 = n3.next()) {
                var r3 = i4.value;
                this.adapter.registerInteractionHandler(r3, this.interactionHandler);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, p2.prototype.destroy = function() {
            var e2, t2;
            try {
              for (var n3 = s(l), i4 = n3.next(); !i4.done; i4 = n3.next()) {
                var r3 = i4.value;
                this.adapter.deregisterInteractionHandler(r3, this.interactionHandler);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, p2.prototype.setDisabled = function(t2) {
            this.savedTabIndex && (t2 ? (this.adapter.setAttr("tabindex", "-1"), this.adapter.removeAttr("role")) : (this.adapter.setAttr("tabindex", this.savedTabIndex), this.adapter.setAttr("role", u.strings.ICON_ROLE)));
          }, p2.prototype.setAriaLabel = function(t2) {
            this.adapter.setAttr("aria-label", t2);
          }, p2.prototype.setContent = function(t2) {
            this.adapter.setContent(t2);
          }, p2.prototype.handleInteraction = function(t2) {
            var e2 = "Enter" === t2.key || 13 === t2.keyCode;
            "click" !== t2.type && !e2 || this.adapter.notifyIconAction();
          }, p2);
          function p2(t2) {
            var e2 = a.call(this, o(o({}, p2.defaultAdapter), t2)) || this;
            return e2.savedTabIndex = null, e2.interactionHandler = function(t3) {
              e2.handleInteraction(t3);
            }, e2;
          }
          e.MDCSelectIconFoundation = d, e.default = d;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = void 0;
          e.strings = { ICON_EVENT: "MDCSelect:icon", ICON_ROLE: "button" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSliderFoundation = void 0;
          var l, s = n2(23), d = n2(10), a = n2(0), u = n2(33), p2 = n2(34);
          (l = l || {}).SLIDER_UPDATE = "slider_update";
          var c, h = "undefined" != typeof window, f = (c = a.MDCFoundation, r2(y, c), Object.defineProperty(y, "defaultAdapter", { get: function() {
            return { hasClass: function() {
              return false;
            }, addClass: function() {
            }, removeClass: function() {
            }, addThumbClass: function() {
            }, removeThumbClass: function() {
            }, getAttribute: function() {
              return null;
            }, getInputValue: function() {
              return "";
            }, setInputValue: function() {
            }, getInputAttribute: function() {
              return null;
            }, setInputAttribute: function() {
              return null;
            }, removeInputAttribute: function() {
              return null;
            }, focusInput: function() {
            }, isInputFocused: function() {
              return false;
            }, shouldHideFocusStylesForPointerEvents: function() {
              return false;
            }, getThumbKnobWidth: function() {
              return 0;
            }, getValueIndicatorContainerWidth: function() {
              return 0;
            }, getThumbBoundingClientRect: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, getBoundingClientRect: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, isRTL: function() {
              return false;
            }, setThumbStyleProperty: function() {
            }, removeThumbStyleProperty: function() {
            }, setTrackActiveStyleProperty: function() {
            }, removeTrackActiveStyleProperty: function() {
            }, setValueIndicatorText: function() {
            }, getValueToAriaValueTextFn: function() {
              return null;
            }, updateTickMarks: function() {
            }, setPointerCapture: function() {
            }, emitChangeEvent: function() {
            }, emitInputEvent: function() {
            }, emitDragStartEvent: function() {
            }, emitDragEndEvent: function() {
            }, registerEventHandler: function() {
            }, deregisterEventHandler: function() {
            }, registerThumbEventHandler: function() {
            }, deregisterThumbEventHandler: function() {
            }, registerInputEventHandler: function() {
            }, deregisterInputEventHandler: function() {
            }, registerBodyEventHandler: function() {
            }, deregisterBodyEventHandler: function() {
            }, registerWindowEventHandler: function() {
            }, deregisterWindowEventHandler: function() {
            } };
          }, enumerable: false, configurable: true }), y.prototype.init = function() {
            var t2 = this;
            this.isDisabled = this.adapter.hasClass(u.cssClasses.DISABLED), this.isDiscrete = this.adapter.hasClass(u.cssClasses.DISCRETE), this.hasTickMarks = this.adapter.hasClass(u.cssClasses.TICK_MARKS), this.isRange = this.adapter.hasClass(u.cssClasses.RANGE);
            var e2 = this.convertAttributeValueToNumber(this.adapter.getInputAttribute(u.attributes.INPUT_MIN, this.isRange ? p2.Thumb.START : p2.Thumb.END), u.attributes.INPUT_MIN), n3 = this.convertAttributeValueToNumber(this.adapter.getInputAttribute(u.attributes.INPUT_MAX, p2.Thumb.END), u.attributes.INPUT_MAX), i4 = this.convertAttributeValueToNumber(this.adapter.getInputAttribute(u.attributes.INPUT_VALUE, p2.Thumb.END), u.attributes.INPUT_VALUE), r3 = this.isRange ? this.convertAttributeValueToNumber(this.adapter.getInputAttribute(u.attributes.INPUT_VALUE, p2.Thumb.START), u.attributes.INPUT_VALUE) : e2, o2 = this.adapter.getInputAttribute(u.attributes.INPUT_STEP, p2.Thumb.END), s2 = o2 ? this.convertAttributeValueToNumber(o2, u.attributes.INPUT_STEP) : this.step, a2 = this.adapter.getAttribute(u.attributes.DATA_MIN_RANGE), c2 = a2 ? this.convertAttributeValueToNumber(a2, u.attributes.DATA_MIN_RANGE) : this.minRange;
            this.validateProperties({ min: e2, max: n3, value: i4, valueStart: r3, step: s2, minRange: c2 }), this.min = e2, this.max = n3, this.value = i4, this.valueStart = r3, this.step = s2, this.minRange = c2, this.numDecimalPlaces = C(this.step), this.valueBeforeDownEvent = i4, this.valueStartBeforeDownEvent = r3, this.mousedownOrTouchstartListener = this.handleMousedownOrTouchstart.bind(this), this.moveListener = this.handleMove.bind(this), this.pointerdownListener = this.handlePointerdown.bind(this), this.pointerupListener = this.handlePointerup.bind(this), this.thumbMouseenterListener = this.handleThumbMouseenter.bind(this), this.thumbMouseleaveListener = this.handleThumbMouseleave.bind(this), this.inputStartChangeListener = function() {
              t2.handleInputChange(p2.Thumb.START);
            }, this.inputEndChangeListener = function() {
              t2.handleInputChange(p2.Thumb.END);
            }, this.inputStartFocusListener = function() {
              t2.handleInputFocus(p2.Thumb.START);
            }, this.inputEndFocusListener = function() {
              t2.handleInputFocus(p2.Thumb.END);
            }, this.inputStartBlurListener = function() {
              t2.handleInputBlur(p2.Thumb.START);
            }, this.inputEndBlurListener = function() {
              t2.handleInputBlur(p2.Thumb.END);
            }, this.resizeListener = this.handleResize.bind(this), this.registerEventHandlers();
          }, y.prototype.destroy = function() {
            this.deregisterEventHandlers();
          }, y.prototype.setMin = function(t2) {
            this.min = t2, this.isRange || (this.valueStart = t2), this.updateUI();
          }, y.prototype.setMax = function(t2) {
            this.max = t2, this.updateUI();
          }, y.prototype.getMin = function() {
            return this.min;
          }, y.prototype.getMax = function() {
            return this.max;
          }, y.prototype.getValue = function() {
            return this.value;
          }, y.prototype.setValue = function(t2) {
            if (this.isRange && t2 < this.valueStart + this.minRange) throw new Error("end thumb value (" + t2 + ") must be >= start thumb value (" + this.valueStart + ") + min range (" + this.minRange + ")");
            this.updateValue(t2, p2.Thumb.END);
          }, y.prototype.getValueStart = function() {
            if (!this.isRange) throw new Error("`valueStart` is only applicable for range sliders.");
            return this.valueStart;
          }, y.prototype.setValueStart = function(t2) {
            if (!this.isRange) throw new Error("`valueStart` is only applicable for range sliders.");
            if (this.isRange && t2 > this.value - this.minRange) throw new Error("start thumb value (" + t2 + ") must be <= end thumb value (" + this.value + ") - min range (" + this.minRange + ")");
            this.updateValue(t2, p2.Thumb.START);
          }, y.prototype.setStep = function(t2) {
            this.step = t2, this.numDecimalPlaces = C(t2), this.updateUI();
          }, y.prototype.setMinRange = function(t2) {
            if (!this.isRange) throw new Error("`minRange` is only applicable for range sliders.");
            if (t2 < 0) throw new Error("`minRange` must be non-negative. Current value: " + t2);
            if (this.value - this.valueStart < t2) throw new Error("start thumb value (" + this.valueStart + ") and end thumb value (" + this.value + ") must differ by at least " + t2 + ".");
            this.minRange = t2;
          }, y.prototype.setIsDiscrete = function(t2) {
            this.isDiscrete = t2, this.updateValueIndicatorUI(), this.updateTickMarksUI();
          }, y.prototype.getStep = function() {
            return this.step;
          }, y.prototype.getMinRange = function() {
            if (!this.isRange) throw new Error("`minRange` is only applicable for range sliders.");
            return this.minRange;
          }, y.prototype.setHasTickMarks = function(t2) {
            this.hasTickMarks = t2, this.updateTickMarksUI();
          }, y.prototype.getDisabled = function() {
            return this.isDisabled;
          }, y.prototype.setDisabled = function(t2) {
            (this.isDisabled = t2) ? (this.adapter.addClass(u.cssClasses.DISABLED), this.isRange && this.adapter.setInputAttribute(u.attributes.INPUT_DISABLED, "", p2.Thumb.START), this.adapter.setInputAttribute(u.attributes.INPUT_DISABLED, "", p2.Thumb.END)) : (this.adapter.removeClass(u.cssClasses.DISABLED), this.isRange && this.adapter.removeInputAttribute(u.attributes.INPUT_DISABLED, p2.Thumb.START), this.adapter.removeInputAttribute(u.attributes.INPUT_DISABLED, p2.Thumb.END));
          }, y.prototype.getIsRange = function() {
            return this.isRange;
          }, y.prototype.layout = function(t2) {
            var e2 = (void 0 === t2 ? {} : t2).skipUpdateUI;
            this.rect = this.adapter.getBoundingClientRect(), this.isRange && (this.startThumbKnobWidth = this.adapter.getThumbKnobWidth(p2.Thumb.START), this.endThumbKnobWidth = this.adapter.getThumbKnobWidth(p2.Thumb.END)), e2 || this.updateUI();
          }, y.prototype.handleResize = function() {
            this.layout();
          }, y.prototype.handleDown = function(t2) {
            if (!this.isDisabled) {
              this.valueStartBeforeDownEvent = this.valueStart, this.valueBeforeDownEvent = this.value;
              var e2 = null != t2.clientX ? t2.clientX : t2.targetTouches[0].clientX;
              this.downEventClientX = e2;
              var n3 = this.mapClientXOnSliderScale(e2);
              this.thumb = this.getThumbFromDownEvent(e2, n3), null !== this.thumb && (this.handleDragStart(t2, n3, this.thumb), this.updateValue(n3, this.thumb, { emitInputEvent: true }));
            }
          }, y.prototype.handleMove = function(t2) {
            if (!this.isDisabled) {
              t2.preventDefault();
              var e2 = null != t2.clientX ? t2.clientX : t2.targetTouches[0].clientX, n3 = null != this.thumb;
              if (this.thumb = this.getThumbFromMoveEvent(e2), null !== this.thumb) {
                var i4 = this.mapClientXOnSliderScale(e2);
                n3 || (this.handleDragStart(t2, i4, this.thumb), this.adapter.emitDragStartEvent(i4, this.thumb)), this.updateValue(i4, this.thumb, { emitInputEvent: true });
              }
            }
          }, y.prototype.handleUp = function() {
            var t2, e2;
            if (!this.isDisabled && null !== this.thumb) {
              (null === (e2 = (t2 = this.adapter).shouldHideFocusStylesForPointerEvents) || void 0 === e2 ? void 0 : e2.call(t2)) && this.handleInputBlur(this.thumb);
              var n3 = this.thumb === p2.Thumb.START ? this.valueStartBeforeDownEvent : this.valueBeforeDownEvent, i4 = this.thumb === p2.Thumb.START ? this.valueStart : this.value;
              n3 !== i4 && this.adapter.emitChangeEvent(i4, this.thumb), this.adapter.emitDragEndEvent(i4, this.thumb), this.thumb = null;
            }
          }, y.prototype.handleThumbMouseenter = function() {
            this.isDiscrete && this.isRange && (this.adapter.addThumbClass(u.cssClasses.THUMB_WITH_INDICATOR, p2.Thumb.START), this.adapter.addThumbClass(u.cssClasses.THUMB_WITH_INDICATOR, p2.Thumb.END));
          }, y.prototype.handleThumbMouseleave = function() {
            var t2, e2;
            this.isDiscrete && this.isRange && ((null === (e2 = (t2 = this.adapter).shouldHideFocusStylesForPointerEvents) || void 0 === e2 || !e2.call(t2)) && (this.adapter.isInputFocused(p2.Thumb.START) || this.adapter.isInputFocused(p2.Thumb.END)) || this.thumb || (this.adapter.removeThumbClass(u.cssClasses.THUMB_WITH_INDICATOR, p2.Thumb.START), this.adapter.removeThumbClass(u.cssClasses.THUMB_WITH_INDICATOR, p2.Thumb.END)));
          }, y.prototype.handleMousedownOrTouchstart = function(t2) {
            var e2 = this, n3 = "mousedown" === t2.type ? "mousemove" : "touchmove";
            function i4() {
              e2.handleUp(), e2.adapter.deregisterBodyEventHandler(n3, e2.moveListener), e2.adapter.deregisterEventHandler("mouseup", i4), e2.adapter.deregisterEventHandler("touchend", i4);
            }
            this.adapter.registerBodyEventHandler(n3, this.moveListener), this.adapter.registerBodyEventHandler("mouseup", i4), this.adapter.registerBodyEventHandler("touchend", i4), this.handleDown(t2);
          }, y.prototype.handlePointerdown = function(t2) {
            0 === t2.button && (null != t2.pointerId && this.adapter.setPointerCapture(t2.pointerId), this.adapter.registerEventHandler("pointermove", this.moveListener), this.handleDown(t2));
          }, y.prototype.handleInputChange = function(t2) {
            var e2 = Number(this.adapter.getInputValue(t2));
            t2 === p2.Thumb.START ? this.setValueStart(e2) : this.setValue(e2), this.adapter.emitChangeEvent(t2 === p2.Thumb.START ? this.valueStart : this.value, t2), this.adapter.emitInputEvent(t2 === p2.Thumb.START ? this.valueStart : this.value, t2);
          }, y.prototype.handleInputFocus = function(t2) {
            if (this.adapter.addThumbClass(u.cssClasses.THUMB_FOCUSED, t2), this.isDiscrete && (this.adapter.addThumbClass(u.cssClasses.THUMB_WITH_INDICATOR, t2), this.isRange)) {
              var e2 = t2 === p2.Thumb.START ? p2.Thumb.END : p2.Thumb.START;
              this.adapter.addThumbClass(u.cssClasses.THUMB_WITH_INDICATOR, e2);
            }
          }, y.prototype.handleInputBlur = function(t2) {
            if (this.adapter.removeThumbClass(u.cssClasses.THUMB_FOCUSED, t2), this.isDiscrete && (this.adapter.removeThumbClass(u.cssClasses.THUMB_WITH_INDICATOR, t2), this.isRange)) {
              var e2 = t2 === p2.Thumb.START ? p2.Thumb.END : p2.Thumb.START;
              this.adapter.removeThumbClass(u.cssClasses.THUMB_WITH_INDICATOR, e2);
            }
          }, y.prototype.handleDragStart = function(t2, e2, n3) {
            var i4, r3;
            this.adapter.emitDragStartEvent(e2, n3), this.adapter.focusInput(n3), (null === (r3 = (i4 = this.adapter).shouldHideFocusStylesForPointerEvents) || void 0 === r3 ? void 0 : r3.call(i4)) && this.handleInputFocus(n3), t2.preventDefault();
          }, y.prototype.getThumbFromDownEvent = function(t2, e2) {
            if (!this.isRange) return p2.Thumb.END;
            var n3 = this.adapter.getThumbBoundingClientRect(p2.Thumb.START), i4 = this.adapter.getThumbBoundingClientRect(p2.Thumb.END), r3 = t2 >= n3.left && t2 <= n3.right, o2 = t2 >= i4.left && t2 <= i4.right;
            return r3 && o2 ? null : r3 ? p2.Thumb.START : o2 ? p2.Thumb.END : e2 < this.valueStart ? p2.Thumb.START : e2 > this.value ? p2.Thumb.END : e2 - this.valueStart <= this.value - e2 ? p2.Thumb.START : p2.Thumb.END;
          }, y.prototype.getThumbFromMoveEvent = function(t2) {
            if (null !== this.thumb) return this.thumb;
            if (null === this.downEventClientX) throw new Error("`downEventClientX` is null after move event.");
            return Math.abs(this.downEventClientX - t2) < u.numbers.THUMB_UPDATE_MIN_PX ? this.thumb : t2 < this.downEventClientX ? this.adapter.isRTL() ? p2.Thumb.END : p2.Thumb.START : this.adapter.isRTL() ? p2.Thumb.START : p2.Thumb.END;
          }, y.prototype.updateUI = function(t2) {
            t2 ? this.updateThumbAndInputAttributes(t2) : (this.updateThumbAndInputAttributes(p2.Thumb.START), this.updateThumbAndInputAttributes(p2.Thumb.END)), this.updateThumbAndTrackUI(t2), this.updateValueIndicatorUI(t2), this.updateTickMarksUI();
          }, y.prototype.updateThumbAndInputAttributes = function(t2) {
            if (t2) {
              var e2 = this.isRange && t2 === p2.Thumb.START ? this.valueStart : this.value, n3 = String(e2);
              this.adapter.setInputAttribute(u.attributes.INPUT_VALUE, n3, t2), this.isRange && t2 === p2.Thumb.START ? this.adapter.setInputAttribute(u.attributes.INPUT_MIN, String(e2 + this.minRange), p2.Thumb.END) : this.isRange && t2 === p2.Thumb.END && this.adapter.setInputAttribute(u.attributes.INPUT_MAX, String(e2 - this.minRange), p2.Thumb.START), this.adapter.getInputValue(t2) !== n3 && this.adapter.setInputValue(n3, t2);
              var i4 = this.adapter.getValueToAriaValueTextFn();
              i4 && this.adapter.setInputAttribute(u.attributes.ARIA_VALUETEXT, i4(e2, t2), t2);
            }
          }, y.prototype.updateValueIndicatorUI = function(t2) {
            if (this.isDiscrete) {
              var e2 = this.isRange && t2 === p2.Thumb.START ? this.valueStart : this.value;
              this.adapter.setValueIndicatorText(e2, t2 === p2.Thumb.START ? p2.Thumb.START : p2.Thumb.END), !t2 && this.isRange && this.adapter.setValueIndicatorText(this.valueStart, p2.Thumb.START);
            }
          }, y.prototype.updateTickMarksUI = function() {
            if (this.isDiscrete && this.hasTickMarks) {
              var t2 = (this.valueStart - this.min) / this.step, e2 = (this.value - this.valueStart) / this.step + 1, n3 = (this.max - this.value) / this.step, i4 = Array.from({ length: t2 }).fill(p2.TickMark.INACTIVE), r3 = Array.from({ length: e2 }).fill(p2.TickMark.ACTIVE), o2 = Array.from({ length: n3 }).fill(p2.TickMark.INACTIVE);
              this.adapter.updateTickMarks(i4.concat(r3).concat(o2));
            }
          }, y.prototype.mapClientXOnSliderScale = function(t2) {
            var e2 = (t2 - this.rect.left) / this.rect.width;
            this.adapter.isRTL() && (e2 = 1 - e2);
            var n3 = this.min + e2 * (this.max - this.min);
            return n3 === this.max || n3 === this.min ? n3 : Number(this.quantize(n3).toFixed(this.numDecimalPlaces));
          }, y.prototype.quantize = function(t2) {
            var e2 = Math.round((t2 - this.min) / this.step);
            return this.min + e2 * this.step;
          }, y.prototype.updateValue = function(t2, e2, n3) {
            var i4 = (void 0 === n3 ? {} : n3).emitInputEvent;
            if (t2 = this.clampValue(t2, e2), this.isRange && e2 === p2.Thumb.START) {
              if (this.valueStart === t2) return;
              this.valueStart = t2;
            } else {
              if (this.value === t2) return;
              this.value = t2;
            }
            this.updateUI(e2), i4 && this.adapter.emitInputEvent(e2 === p2.Thumb.START ? this.valueStart : this.value, e2);
          }, y.prototype.clampValue = function(t2, e2) {
            return t2 = Math.min(Math.max(t2, this.min), this.max), this.isRange && e2 === p2.Thumb.START && t2 > this.value - this.minRange ? this.value - this.minRange : this.isRange && e2 === p2.Thumb.END && t2 < this.valueStart + this.minRange ? this.valueStart + this.minRange : t2;
          }, y.prototype.updateThumbAndTrackUI = function(n3) {
            var i4 = this, t2 = this.max, e2 = this.min, r3 = (this.value - this.valueStart) / (t2 - e2), o2 = r3 * this.rect.width, s2 = this.adapter.isRTL(), a2 = h ? d.getCorrectPropertyName(window, "transform") : "transform";
            if (this.isRange) {
              var c2 = this.adapter.isRTL() ? (t2 - this.value) / (t2 - e2) * this.rect.width : (this.valueStart - e2) / (t2 - e2) * this.rect.width, u2 = c2 + o2;
              this.animFrame.request(l.SLIDER_UPDATE, function() {
                !s2 && n3 === p2.Thumb.START || s2 && n3 !== p2.Thumb.START ? (i4.adapter.setTrackActiveStyleProperty("transform-origin", "right"), i4.adapter.setTrackActiveStyleProperty("left", "auto"), i4.adapter.setTrackActiveStyleProperty("right", i4.rect.width - u2 + "px")) : (i4.adapter.setTrackActiveStyleProperty("transform-origin", "left"), i4.adapter.setTrackActiveStyleProperty("right", "auto"), i4.adapter.setTrackActiveStyleProperty("left", c2 + "px")), i4.adapter.setTrackActiveStyleProperty(a2, "scaleX(" + r3 + ")");
                var t3 = s2 ? u2 : c2, e3 = i4.adapter.isRTL() ? c2 : u2;
                n3 !== p2.Thumb.START && n3 && i4.initialStylesRemoved || (i4.adapter.setThumbStyleProperty(a2, "translateX(" + t3 + "px)", p2.Thumb.START), i4.alignValueIndicator(p2.Thumb.START, t3)), n3 !== p2.Thumb.END && n3 && i4.initialStylesRemoved || (i4.adapter.setThumbStyleProperty(a2, "translateX(" + e3 + "px)", p2.Thumb.END), i4.alignValueIndicator(p2.Thumb.END, e3)), i4.removeInitialStyles(s2), i4.updateOverlappingThumbsUI(t3, e3, n3);
              });
            } else this.animFrame.request(l.SLIDER_UPDATE, function() {
              var t3 = s2 ? i4.rect.width - o2 : o2;
              i4.adapter.setThumbStyleProperty(a2, "translateX(" + t3 + "px)", p2.Thumb.END), i4.alignValueIndicator(p2.Thumb.END, t3), i4.adapter.setTrackActiveStyleProperty(a2, "scaleX(" + r3 + ")"), i4.removeInitialStyles(s2);
            });
          }, y.prototype.alignValueIndicator = function(t2, e2) {
            if (this.isDiscrete) {
              var n3 = this.adapter.getThumbBoundingClientRect(t2).width / 2, i4 = this.adapter.getValueIndicatorContainerWidth(t2), r3 = this.adapter.getBoundingClientRect().width;
              e2 + n3 < i4 / 2 ? (this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CARET_LEFT, n3 + "px", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CARET_RIGHT, "auto", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CARET_TRANSFORM, "translateX(-50%)", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CONTAINER_LEFT, "0", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CONTAINER_RIGHT, "auto", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CONTAINER_TRANSFORM, "none", t2)) : r3 - e2 + n3 < i4 / 2 ? (this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CARET_LEFT, "auto", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CARET_RIGHT, n3 + "px", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CARET_TRANSFORM, "translateX(50%)", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CONTAINER_LEFT, "auto", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CONTAINER_RIGHT, "0", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CONTAINER_TRANSFORM, "none", t2)) : (this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CARET_LEFT, "50%", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CARET_RIGHT, "auto", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CARET_TRANSFORM, "translateX(-50%)", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CONTAINER_LEFT, "50%", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CONTAINER_RIGHT, "auto", t2), this.adapter.setThumbStyleProperty(u.strings.VAR_VALUE_INDICATOR_CONTAINER_TRANSFORM, "translateX(-50%)", t2));
            }
          }, y.prototype.removeInitialStyles = function(t2) {
            if (!this.initialStylesRemoved) {
              var e2 = t2 ? "right" : "left";
              this.adapter.removeThumbStyleProperty(e2, p2.Thumb.END), this.isRange && this.adapter.removeThumbStyleProperty(e2, p2.Thumb.START), this.initialStylesRemoved = true, this.resetTrackAndThumbAnimation();
            }
          }, y.prototype.resetTrackAndThumbAnimation = function() {
            var t2 = this;
            if (this.isDiscrete) {
              var e2 = h ? d.getCorrectPropertyName(window, "transition") : "transition", n3 = "none 0s ease 0s";
              this.adapter.setThumbStyleProperty(e2, n3, p2.Thumb.END), this.isRange && this.adapter.setThumbStyleProperty(e2, n3, p2.Thumb.START), this.adapter.setTrackActiveStyleProperty(e2, n3), requestAnimationFrame(function() {
                t2.adapter.removeThumbStyleProperty(e2, p2.Thumb.END), t2.adapter.removeTrackActiveStyleProperty(e2), t2.isRange && t2.adapter.removeThumbStyleProperty(e2, p2.Thumb.START);
              });
            }
          }, y.prototype.updateOverlappingThumbsUI = function(t2, e2, n3) {
            var i4 = false;
            if (this.adapter.isRTL()) i4 = t2 - this.startThumbKnobWidth / 2 <= e2 + this.endThumbKnobWidth / 2;
            else {
              var r3 = t2 + this.startThumbKnobWidth / 2;
              i4 = e2 - this.endThumbKnobWidth / 2 <= r3;
            }
            i4 ? (this.adapter.addThumbClass(u.cssClasses.THUMB_TOP, n3 || p2.Thumb.END), this.adapter.removeThumbClass(u.cssClasses.THUMB_TOP, n3 === p2.Thumb.START ? p2.Thumb.END : p2.Thumb.START)) : (this.adapter.removeThumbClass(u.cssClasses.THUMB_TOP, p2.Thumb.START), this.adapter.removeThumbClass(u.cssClasses.THUMB_TOP, p2.Thumb.END));
          }, y.prototype.convertAttributeValueToNumber = function(t2, e2) {
            if (null === t2) throw new Error("MDCSliderFoundation: `" + e2 + "` must be non-null.");
            var n3 = Number(t2);
            if (isNaN(n3)) throw new Error("MDCSliderFoundation: `" + e2 + "` value is `" + t2 + "`, but must be a number.");
            return n3;
          }, y.prototype.validateProperties = function(t2) {
            var e2 = t2.min, n3 = t2.max, i4 = t2.value, r3 = t2.valueStart, o2 = t2.step, s2 = t2.minRange;
            if (n3 <= e2) throw new Error("MDCSliderFoundation: min must be strictly less than max. Current: [min: " + e2 + ", max: " + n3 + "]");
            if (o2 <= 0) throw new Error("MDCSliderFoundation: step must be a positive number. Current step: " + o2);
            if (this.isRange) {
              if (i4 < e2 || n3 < i4 || r3 < e2 || n3 < r3) throw new Error("MDCSliderFoundation: values must be in [min, max] range. Current values: [start value: " + r3 + ", end value: " + i4 + ", min: " + e2 + ", max: " + n3 + "]");
              if (i4 < r3) throw new Error("MDCSliderFoundation: start value must be <= end value. Current values: [start value: " + r3 + ", end value: " + i4 + "]");
              if (s2 < 0) throw new Error("MDCSliderFoundation: minimum range must be non-negative. Current min range: " + s2);
              if (i4 - r3 < s2) throw new Error("MDCSliderFoundation: start value and end value must differ by at least " + s2 + ". Current values: [start value: " + r3 + ", end value: " + i4 + "]");
              var a2 = (r3 - e2) / o2, c2 = (i4 - e2) / o2;
              if (!Number.isInteger(parseFloat(a2.toFixed(6))) || !Number.isInteger(parseFloat(c2.toFixed(6)))) throw new Error("MDCSliderFoundation: Slider values must be valid based on the step value (" + o2 + "). Current values: [start value: " + r3 + ", end value: " + i4 + ", min: " + e2 + "]");
            } else {
              if (i4 < e2 || n3 < i4) throw new Error("MDCSliderFoundation: value must be in [min, max] range. Current values: [value: " + i4 + ", min: " + e2 + ", max: " + n3 + "]");
              if (c2 = (i4 - e2) / o2, !Number.isInteger(parseFloat(c2.toFixed(6)))) throw new Error("MDCSliderFoundation: Slider value must be valid based on the step value (" + o2 + "). Current value: " + i4);
            }
          }, y.prototype.registerEventHandlers = function() {
            this.adapter.registerWindowEventHandler("resize", this.resizeListener), y.SUPPORTS_POINTER_EVENTS ? (this.adapter.registerEventHandler("pointerdown", this.pointerdownListener), this.adapter.registerEventHandler("pointerup", this.pointerupListener)) : (this.adapter.registerEventHandler("mousedown", this.mousedownOrTouchstartListener), this.adapter.registerEventHandler("touchstart", this.mousedownOrTouchstartListener)), this.isRange && (this.adapter.registerThumbEventHandler(p2.Thumb.START, "mouseenter", this.thumbMouseenterListener), this.adapter.registerThumbEventHandler(p2.Thumb.START, "mouseleave", this.thumbMouseleaveListener), this.adapter.registerInputEventHandler(p2.Thumb.START, "change", this.inputStartChangeListener), this.adapter.registerInputEventHandler(p2.Thumb.START, "focus", this.inputStartFocusListener), this.adapter.registerInputEventHandler(p2.Thumb.START, "blur", this.inputStartBlurListener)), this.adapter.registerThumbEventHandler(p2.Thumb.END, "mouseenter", this.thumbMouseenterListener), this.adapter.registerThumbEventHandler(p2.Thumb.END, "mouseleave", this.thumbMouseleaveListener), this.adapter.registerInputEventHandler(p2.Thumb.END, "change", this.inputEndChangeListener), this.adapter.registerInputEventHandler(p2.Thumb.END, "focus", this.inputEndFocusListener), this.adapter.registerInputEventHandler(p2.Thumb.END, "blur", this.inputEndBlurListener);
          }, y.prototype.deregisterEventHandlers = function() {
            this.adapter.deregisterWindowEventHandler("resize", this.resizeListener), y.SUPPORTS_POINTER_EVENTS ? (this.adapter.deregisterEventHandler("pointerdown", this.pointerdownListener), this.adapter.deregisterEventHandler("pointerup", this.pointerupListener)) : (this.adapter.deregisterEventHandler("mousedown", this.mousedownOrTouchstartListener), this.adapter.deregisterEventHandler("touchstart", this.mousedownOrTouchstartListener)), this.isRange && (this.adapter.deregisterThumbEventHandler(p2.Thumb.START, "mouseenter", this.thumbMouseenterListener), this.adapter.deregisterThumbEventHandler(p2.Thumb.START, "mouseleave", this.thumbMouseleaveListener), this.adapter.deregisterInputEventHandler(p2.Thumb.START, "change", this.inputStartChangeListener), this.adapter.deregisterInputEventHandler(p2.Thumb.START, "focus", this.inputStartFocusListener), this.adapter.deregisterInputEventHandler(p2.Thumb.START, "blur", this.inputStartBlurListener)), this.adapter.deregisterThumbEventHandler(p2.Thumb.END, "mouseenter", this.thumbMouseenterListener), this.adapter.deregisterThumbEventHandler(p2.Thumb.END, "mouseleave", this.thumbMouseleaveListener), this.adapter.deregisterInputEventHandler(p2.Thumb.END, "change", this.inputEndChangeListener), this.adapter.deregisterInputEventHandler(p2.Thumb.END, "focus", this.inputEndFocusListener), this.adapter.deregisterInputEventHandler(p2.Thumb.END, "blur", this.inputEndBlurListener);
          }, y.prototype.handlePointerup = function() {
            this.handleUp(), this.adapter.deregisterEventHandler("pointermove", this.moveListener);
          }, y.SUPPORTS_POINTER_EVENTS = h && Boolean(window.PointerEvent) && !(["iPad Simulator", "iPhone Simulator", "iPod Simulator", "iPad", "iPhone", "iPod"].includes(navigator.platform) || navigator.userAgent.includes("Mac") && "ontouchend" in document), y);
          function y(t2) {
            var e2 = c.call(this, o(o({}, y.defaultAdapter), t2)) || this;
            return e2.initialStylesRemoved = false, e2.isDisabled = false, e2.isDiscrete = false, e2.step = u.numbers.STEP_SIZE, e2.minRange = u.numbers.MIN_RANGE, e2.hasTickMarks = false, e2.isRange = false, e2.thumb = null, e2.downEventClientX = null, e2.startThumbKnobWidth = 0, e2.endThumbKnobWidth = 0, e2.animFrame = new s.AnimationFrame(), e2;
          }
          function C(t2) {
            var e2 = /(?:\.(\d+))?(?:[eE]([+\-]?\d+))?$/.exec(String(t2));
            if (!e2) return 0;
            var n3 = e2[1] || "", i4 = e2[2] || 0;
            return Math.max(0, ("0" === n3 ? 0 : n3.length) - Number(i4));
          }
          e.MDCSliderFoundation = f;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.announce = void 0;
          var i3 = n2(16), o = i3.numbers.ARIA_LIVE_DELAY_MS, s = i3.strings.ARIA_LIVE_LABEL_TEXT_ATTR;
          e.announce = function(t2, e2) {
            void 0 === e2 && (e2 = t2);
            var n3 = t2.getAttribute("aria-live"), i4 = e2.textContent.trim();
            if (i4 && n3) {
              t2.setAttribute("aria-live", "off"), e2.textContent = "";
              var r2 = document.createElement("span");
              r2.setAttribute("style", "display: inline-block; width: 0; height: 1px;"), r2.textContent = "\xA0", e2.appendChild(r2), e2.setAttribute(s, i4), setTimeout(function() {
                t2.setAttribute("aria-live", n3), e2.removeAttribute(s), e2.textContent = i4;
              }, o);
            }
          };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSnackbarFoundation = void 0;
          var s, a = n2(0), c = n2(16), u = c.cssClasses.OPENING, l = c.cssClasses.OPEN, d = c.cssClasses.CLOSING, p2 = c.strings.REASON_ACTION, h = c.strings.REASON_DISMISS, f = (s = a.MDCFoundation, r2(y, s), Object.defineProperty(y, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(y, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(y, "numbers", { get: function() {
            return c.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(y, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, announce: function() {
            }, notifyClosed: function() {
            }, notifyClosing: function() {
            }, notifyOpened: function() {
            }, notifyOpening: function() {
            }, removeClass: function() {
            } };
          }, enumerable: false, configurable: true }), y.prototype.destroy = function() {
            this.clearAutoDismissTimer(), cancelAnimationFrame(this.animationFrame), this.animationFrame = 0, clearTimeout(this.animationTimer), this.animationTimer = 0, this.adapter.removeClass(u), this.adapter.removeClass(l), this.adapter.removeClass(d);
          }, y.prototype.open = function() {
            var e2 = this;
            this.clearAutoDismissTimer(), this.opened = true, this.adapter.notifyOpening(), this.adapter.removeClass(d), this.adapter.addClass(u), this.adapter.announce(), this.runNextAnimationFrame(function() {
              e2.adapter.addClass(l), e2.animationTimer = setTimeout(function() {
                var t2 = e2.getTimeoutMs();
                e2.handleAnimationTimerEnd(), e2.adapter.notifyOpened(), t2 !== c.numbers.INDETERMINATE && (e2.autoDismissTimer = setTimeout(function() {
                  e2.close(h);
                }, t2));
              }, c.numbers.SNACKBAR_ANIMATION_OPEN_TIME_MS);
            });
          }, y.prototype.close = function(t2) {
            var e2 = this;
            void 0 === t2 && (t2 = ""), this.opened && (cancelAnimationFrame(this.animationFrame), this.animationFrame = 0, this.clearAutoDismissTimer(), this.opened = false, this.adapter.notifyClosing(t2), this.adapter.addClass(c.cssClasses.CLOSING), this.adapter.removeClass(c.cssClasses.OPEN), this.adapter.removeClass(c.cssClasses.OPENING), clearTimeout(this.animationTimer), this.animationTimer = setTimeout(function() {
              e2.handleAnimationTimerEnd(), e2.adapter.notifyClosed(t2);
            }, c.numbers.SNACKBAR_ANIMATION_CLOSE_TIME_MS));
          }, y.prototype.isOpen = function() {
            return this.opened;
          }, y.prototype.getTimeoutMs = function() {
            return this.autoDismissTimeoutMs;
          }, y.prototype.setTimeoutMs = function(t2) {
            var e2 = c.numbers.MIN_AUTO_DISMISS_TIMEOUT_MS, n3 = c.numbers.MAX_AUTO_DISMISS_TIMEOUT_MS, i4 = c.numbers.INDETERMINATE;
            if (!(t2 === c.numbers.INDETERMINATE || t2 <= n3 && e2 <= t2)) throw new Error("\n        timeoutMs must be an integer in the range " + e2 + "\u2013" + n3 + "\n        (or " + i4 + " to disable), but got '" + t2 + "'");
            this.autoDismissTimeoutMs = t2;
          }, y.prototype.getCloseOnEscape = function() {
            return this.closeOnEscape;
          }, y.prototype.setCloseOnEscape = function(t2) {
            this.closeOnEscape = t2;
          }, y.prototype.handleKeyDown = function(t2) {
            "Escape" !== t2.key && 27 !== t2.keyCode || !this.getCloseOnEscape() || this.close(h);
          }, y.prototype.handleActionButtonClick = function(t2) {
            this.close(p2);
          }, y.prototype.handleActionIconClick = function(t2) {
            this.close(h);
          }, y.prototype.clearAutoDismissTimer = function() {
            clearTimeout(this.autoDismissTimer), this.autoDismissTimer = 0;
          }, y.prototype.handleAnimationTimerEnd = function() {
            this.animationTimer = 0, this.adapter.removeClass(c.cssClasses.OPENING), this.adapter.removeClass(c.cssClasses.CLOSING);
          }, y.prototype.runNextAnimationFrame = function(t2) {
            var e2 = this;
            cancelAnimationFrame(this.animationFrame), this.animationFrame = requestAnimationFrame(function() {
              e2.animationFrame = 0, clearTimeout(e2.animationTimer), e2.animationTimer = setTimeout(t2, 0);
            });
          }, y);
          function y(t2) {
            var e2 = s.call(this, o(o({}, y.defaultAdapter), t2)) || this;
            return e2.opened = false, e2.animationFrame = 0, e2.animationTimer = 0, e2.autoDismissTimer = 0, e2.autoDismissTimeoutMs = c.numbers.DEFAULT_AUTO_DISMISS_TIMEOUT_MS, e2.closeOnEscape = true, e2;
          }
          e.MDCSnackbarFoundation = f, e.default = f;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSwitchFoundation = void 0;
          var s, a = n2(0), c = n2(96), u = (s = a.MDCFoundation, r2(l, s), Object.defineProperty(l, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(l, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, setNativeControlChecked: function() {
            }, setNativeControlDisabled: function() {
            }, setNativeControlAttr: function() {
            } };
          }, enumerable: false, configurable: true }), l.prototype.setChecked = function(t2) {
            this.adapter.setNativeControlChecked(t2), this.updateAriaChecked(t2), this.updateCheckedStyling(t2);
          }, l.prototype.setDisabled = function(t2) {
            this.adapter.setNativeControlDisabled(t2), t2 ? this.adapter.addClass(c.cssClasses.DISABLED) : this.adapter.removeClass(c.cssClasses.DISABLED);
          }, l.prototype.handleChange = function(t2) {
            var e2 = t2.target;
            this.updateAriaChecked(e2.checked), this.updateCheckedStyling(e2.checked);
          }, l.prototype.updateCheckedStyling = function(t2) {
            t2 ? this.adapter.addClass(c.cssClasses.CHECKED) : this.adapter.removeClass(c.cssClasses.CHECKED);
          }, l.prototype.updateAriaChecked = function(t2) {
            this.adapter.setNativeControlAttr(c.strings.ARIA_CHECKED_ATTR, "" + !!t2);
          }, l);
          function l(t2) {
            return s.call(this, o(o({}, l.defaultAdapter), t2)) || this;
          }
          e.MDCSwitchFoundation = u, e.default = u;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.cssClasses = void 0;
          e.cssClasses = { CHECKED: "mdc-switch--checked", DISABLED: "mdc-switch--disabled" };
          e.strings = { ARIA_CHECKED_ATTR: "aria-checked", NATIVE_CONTROL_SELECTOR: ".mdc-switch__native-control", RIPPLE_SURFACE_SELECTOR: ".mdc-switch__thumb-underlay" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSwitchRenderFoundation = e.MDCSwitchFoundation = void 0;
          var o, s = n2(216), a = n2(35), c = (o = s.MDCObserverFoundation, r2(u, o), u.prototype.init = function() {
            this.observe(this.adapter.state, { disabled: this.stopProcessingIfDisabled, processing: this.stopProcessingIfDisabled });
          }, u.prototype.handleClick = function() {
            this.adapter.state.disabled || (this.adapter.state.selected = !this.adapter.state.selected);
          }, u.prototype.stopProcessingIfDisabled = function() {
            this.adapter.state.disabled && (this.adapter.state.processing = false);
          }, u);
          function u(t2) {
            var e2 = o.call(this, t2) || this;
            return e2.handleClick = e2.handleClick.bind(e2), e2;
          }
          e.MDCSwitchFoundation = c;
          var l, d = (r2(p2, l = c), p2.prototype.init = function() {
            l.prototype.init.call(this), this.observe(this.adapter.state, { disabled: this.onDisabledChange, processing: this.onProcessingChange, selected: this.onSelectedChange });
          }, p2.prototype.initFromDOM = function() {
            this.setObserversEnabled(this.adapter.state, false), this.adapter.state.selected = this.adapter.hasClass(a.CssClasses.SELECTED), this.onSelectedChange(), this.adapter.state.disabled = this.adapter.isDisabled(), this.adapter.state.processing = this.adapter.hasClass(a.CssClasses.PROCESSING), this.setObserversEnabled(this.adapter.state, true), this.stopProcessingIfDisabled();
          }, p2.prototype.onDisabledChange = function() {
            this.adapter.setDisabled(this.adapter.state.disabled);
          }, p2.prototype.onProcessingChange = function() {
            this.toggleClass(this.adapter.state.processing, a.CssClasses.PROCESSING);
          }, p2.prototype.onSelectedChange = function() {
            this.adapter.setAriaChecked(String(this.adapter.state.selected)), this.toggleClass(this.adapter.state.selected, a.CssClasses.SELECTED), this.toggleClass(!this.adapter.state.selected, a.CssClasses.UNSELECTED);
          }, p2.prototype.toggleClass = function(t2, e2) {
            t2 ? this.adapter.addClass(e2) : this.adapter.removeClass(e2);
          }, p2);
          function p2() {
            return null !== l && l.apply(this, arguments) || this;
          }
          e.MDCSwitchRenderFoundation = d;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), s = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), a = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && o(e2, t2, n3);
            return s(e2, t2), e2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabScroller = void 0;
          var c, u = n2(1), l = n2(5), d = n2(3), p2 = n2(99), h = a(n2(100)), f = (c = u.MDCComponent, r2(y, c), y.attachTo = function(t2) {
            return new y(t2);
          }, y.prototype.initialize = function() {
            this.area = this.root.querySelector(p2.MDCTabScrollerFoundation.strings.AREA_SELECTOR), this.content = this.root.querySelector(p2.MDCTabScrollerFoundation.strings.CONTENT_SELECTOR);
          }, y.prototype.initialSyncWithDOM = function() {
            var e2 = this;
            this.handleInteraction = function() {
              e2.foundation.handleInteraction();
            }, this.handleTransitionEnd = function(t2) {
              e2.foundation.handleTransitionEnd(t2);
            }, this.area.addEventListener("wheel", this.handleInteraction, l.applyPassive()), this.area.addEventListener("touchstart", this.handleInteraction, l.applyPassive()), this.area.addEventListener("pointerdown", this.handleInteraction, l.applyPassive()), this.area.addEventListener("mousedown", this.handleInteraction, l.applyPassive()), this.area.addEventListener("keydown", this.handleInteraction, l.applyPassive()), this.content.addEventListener("transitionend", this.handleTransitionEnd);
          }, y.prototype.destroy = function() {
            c.prototype.destroy.call(this), this.area.removeEventListener("wheel", this.handleInteraction, l.applyPassive()), this.area.removeEventListener("touchstart", this.handleInteraction, l.applyPassive()), this.area.removeEventListener("pointerdown", this.handleInteraction, l.applyPassive()), this.area.removeEventListener("mousedown", this.handleInteraction, l.applyPassive()), this.area.removeEventListener("keydown", this.handleInteraction, l.applyPassive()), this.content.removeEventListener("transitionend", this.handleTransitionEnd);
          }, y.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { eventTargetMatchesSelector: function(t3, e2) {
              return d.matches(t3, e2);
            }, addClass: function(t3) {
              n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              n3.root.classList.remove(t3);
            }, addScrollAreaClass: function(t3) {
              n3.area.classList.add(t3);
            }, setScrollAreaStyleProperty: function(t3, e2) {
              n3.area.style.setProperty(t3, e2);
            }, setScrollContentStyleProperty: function(t3, e2) {
              n3.content.style.setProperty(t3, e2);
            }, getScrollContentStyleValue: function(t3) {
              return window.getComputedStyle(n3.content).getPropertyValue(t3);
            }, setScrollAreaScrollLeft: function(t3) {
              return n3.area.scrollLeft = t3;
            }, getScrollAreaScrollLeft: function() {
              return n3.area.scrollLeft;
            }, getScrollContentOffsetWidth: function() {
              return n3.content.offsetWidth;
            }, getScrollAreaOffsetWidth: function() {
              return n3.area.offsetWidth;
            }, computeScrollAreaClientRect: function() {
              return n3.area.getBoundingClientRect();
            }, computeScrollContentClientRect: function() {
              return n3.content.getBoundingClientRect();
            }, computeHorizontalScrollbarHeight: function() {
              return h.computeHorizontalScrollbarHeight(document);
            } };
            return new p2.MDCTabScrollerFoundation(t2);
          }, y.prototype.getScrollPosition = function() {
            return this.foundation.getScrollPosition();
          }, y.prototype.getScrollContentWidth = function() {
            return this.content.offsetWidth;
          }, y.prototype.incrementScroll = function(t2) {
            this.foundation.incrementScroll(t2);
          }, y.prototype.scrollTo = function(t2) {
            this.foundation.scrollTo(t2);
          }, y);
          function y() {
            return null !== c && c.apply(this, arguments) || this;
          }
          e.MDCTabScroller = f;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__read || function(t2, e2) {
            var n3 = "function" == typeof Symbol && t2[Symbol.iterator];
            if (!n3) return t2;
            var i4, r3, o2 = n3.call(t2), s2 = [];
            try {
              for (; (void 0 === e2 || 0 < e2--) && !(i4 = o2.next()).done; ) s2.push(i4.value);
            } catch (t3) {
              r3 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (n3 = o2.return) && n3.call(o2);
              } finally {
                if (r3) throw r3.error;
              }
            }
            return s2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabScrollerFoundation = void 0;
          var a, c = n2(0), u = n2(36), l = n2(221), d = n2(222), p2 = n2(223), h = (a = c.MDCFoundation, r2(f, a), Object.defineProperty(f, "cssClasses", { get: function() {
            return u.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(f, "strings", { get: function() {
            return u.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(f, "defaultAdapter", { get: function() {
            return { eventTargetMatchesSelector: function() {
              return false;
            }, addClass: function() {
            }, removeClass: function() {
            }, addScrollAreaClass: function() {
            }, setScrollAreaStyleProperty: function() {
            }, setScrollContentStyleProperty: function() {
            }, getScrollContentStyleValue: function() {
              return "";
            }, setScrollAreaScrollLeft: function() {
            }, getScrollAreaScrollLeft: function() {
              return 0;
            }, getScrollContentOffsetWidth: function() {
              return 0;
            }, getScrollAreaOffsetWidth: function() {
              return 0;
            }, computeScrollAreaClientRect: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, computeScrollContentClientRect: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, computeHorizontalScrollbarHeight: function() {
              return 0;
            } };
          }, enumerable: false, configurable: true }), f.prototype.init = function() {
            var t2 = this.adapter.computeHorizontalScrollbarHeight();
            this.adapter.setScrollAreaStyleProperty("margin-bottom", -t2 + "px"), this.adapter.addScrollAreaClass(f.cssClasses.SCROLL_AREA_SCROLL);
          }, f.prototype.getScrollPosition = function() {
            if (this.isRTL()) return this.computeCurrentScrollPositionRTL();
            var t2 = this.calculateCurrentTranslateX();
            return this.adapter.getScrollAreaScrollLeft() - t2;
          }, f.prototype.handleInteraction = function() {
            this.isAnimating && this.stopScrollAnimation();
          }, f.prototype.handleTransitionEnd = function(t2) {
            var e2 = t2.target;
            this.isAnimating && this.adapter.eventTargetMatchesSelector(e2, f.strings.CONTENT_SELECTOR) && (this.isAnimating = false, this.adapter.removeClass(f.cssClasses.ANIMATING));
          }, f.prototype.incrementScroll = function(t2) {
            0 !== t2 && this.animate(this.getIncrementScrollOperation(t2));
          }, f.prototype.incrementScrollImmediate = function(t2) {
            if (0 !== t2) {
              var e2 = this.getIncrementScrollOperation(t2);
              0 !== e2.scrollDelta && (this.stopScrollAnimation(), this.adapter.setScrollAreaScrollLeft(e2.finalScrollPosition));
            }
          }, f.prototype.scrollTo = function(t2) {
            this.isRTL() ? this.scrollToImplRTL(t2) : this.scrollToImpl(t2);
          }, f.prototype.getRTLScroller = function() {
            return this.rtlScrollerInstance || (this.rtlScrollerInstance = this.rtlScrollerFactory()), this.rtlScrollerInstance;
          }, f.prototype.calculateCurrentTranslateX = function() {
            var t2 = this.adapter.getScrollContentStyleValue("transform");
            if ("none" === t2) return 0;
            var e2 = /\((.+?)\)/.exec(t2);
            if (!e2) return 0;
            var n3 = e2[1], i4 = s(n3.split(","), 6), r3 = (i4[0], i4[1], i4[2], i4[3], i4[4]);
            return i4[5], parseFloat(r3);
          }, f.prototype.clampScrollValue = function(t2) {
            var e2 = this.calculateScrollEdges();
            return Math.min(Math.max(e2.left, t2), e2.right);
          }, f.prototype.computeCurrentScrollPositionRTL = function() {
            var t2 = this.calculateCurrentTranslateX();
            return this.getRTLScroller().getScrollPositionRTL(t2);
          }, f.prototype.calculateScrollEdges = function() {
            return { left: 0, right: this.adapter.getScrollContentOffsetWidth() - this.adapter.getScrollAreaOffsetWidth() };
          }, f.prototype.scrollToImpl = function(t2) {
            var e2 = this.getScrollPosition(), n3 = this.clampScrollValue(t2), i4 = n3 - e2;
            this.animate({ finalScrollPosition: n3, scrollDelta: i4 });
          }, f.prototype.scrollToImplRTL = function(t2) {
            var e2 = this.getRTLScroller().scrollToRTL(t2);
            this.animate(e2);
          }, f.prototype.getIncrementScrollOperation = function(t2) {
            if (this.isRTL()) return this.getRTLScroller().incrementScrollRTL(t2);
            var e2 = this.getScrollPosition(), n3 = t2 + e2, i4 = this.clampScrollValue(n3);
            return { finalScrollPosition: i4, scrollDelta: i4 - e2 };
          }, f.prototype.animate = function(t2) {
            var e2 = this;
            0 !== t2.scrollDelta && (this.stopScrollAnimation(), this.adapter.setScrollAreaScrollLeft(t2.finalScrollPosition), this.adapter.setScrollContentStyleProperty("transform", "translateX(" + t2.scrollDelta + "px)"), this.adapter.computeScrollAreaClientRect(), requestAnimationFrame(function() {
              e2.adapter.addClass(f.cssClasses.ANIMATING), e2.adapter.setScrollContentStyleProperty("transform", "none");
            }), this.isAnimating = true);
          }, f.prototype.stopScrollAnimation = function() {
            this.isAnimating = false;
            var t2 = this.getAnimatingScrollPosition();
            this.adapter.removeClass(f.cssClasses.ANIMATING), this.adapter.setScrollContentStyleProperty("transform", "translateX(0px)"), this.adapter.setScrollAreaScrollLeft(t2);
          }, f.prototype.getAnimatingScrollPosition = function() {
            var t2 = this.calculateCurrentTranslateX(), e2 = this.adapter.getScrollAreaScrollLeft();
            return this.isRTL() ? this.getRTLScroller().getAnimatingScrollPosition(e2, t2) : e2 - t2;
          }, f.prototype.rtlScrollerFactory = function() {
            var t2 = this.adapter.getScrollAreaScrollLeft();
            this.adapter.setScrollAreaScrollLeft(t2 - 1);
            var e2 = this.adapter.getScrollAreaScrollLeft();
            if (e2 < 0) return this.adapter.setScrollAreaScrollLeft(t2), new d.MDCTabScrollerRTLNegative(this.adapter);
            var n3 = this.adapter.computeScrollAreaClientRect(), i4 = this.adapter.computeScrollContentClientRect(), r3 = Math.round(i4.right - n3.right);
            return this.adapter.setScrollAreaScrollLeft(t2), r3 === e2 ? new p2.MDCTabScrollerRTLReverse(this.adapter) : new l.MDCTabScrollerRTLDefault(this.adapter);
          }, f.prototype.isRTL = function() {
            return "rtl" === this.adapter.getScrollContentStyleValue("direction");
          }, f);
          function f(t2) {
            var e2 = a.call(this, o(o({}, f.defaultAdapter), t2)) || this;
            return e2.isAnimating = false, e2;
          }
          e.MDCTabScrollerFoundation = h, e.default = h;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.computeHorizontalScrollbarHeight = void 0;
          var r2, o = n2(36);
          e.computeHorizontalScrollbarHeight = function(t2, e2) {
            if (void 0 === e2 && (e2 = true), e2 && void 0 !== r2) return r2;
            var n3 = t2.createElement("div");
            n3.classList.add(o.cssClasses.SCROLL_TEST), t2.body.appendChild(n3);
            var i3 = n3.offsetHeight - n3.clientHeight;
            return t2.body.removeChild(n3), e2 && (r2 = i3), i3;
          };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTab = void 0;
          var o, s = n2(1), a = n2(2), c = n2(4), u = n2(102), l = n2(38), d = (o = s.MDCComponent, r2(p2, o), p2.attachTo = function(t2) {
            return new p2(t2);
          }, p2.prototype.initialize = function(t2, e2) {
            void 0 === t2 && (t2 = function(t3, e3) {
              return new a.MDCRipple(t3, e3);
            }), void 0 === e2 && (e2 = function(t3) {
              return new u.MDCTabIndicator(t3);
            }), this.id = this.root.id;
            var n3 = new c.MDCRippleFoundation(a.MDCRipple.createAdapter(this));
            this.ripple = t2(this.root, n3);
            var i4 = this.root.querySelector(l.MDCTabFoundation.strings.TAB_INDICATOR_SELECTOR);
            this.tabIndicator = e2(i4), this.content = this.root.querySelector(l.MDCTabFoundation.strings.CONTENT_SELECTOR);
          }, p2.prototype.initialSyncWithDOM = function() {
            var t2 = this;
            this.handleClick = function() {
              t2.foundation.handleClick();
            }, this.listen("click", this.handleClick);
          }, p2.prototype.destroy = function() {
            this.unlisten("click", this.handleClick), this.ripple.destroy(), o.prototype.destroy.call(this);
          }, p2.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { setAttr: function(t3, e2) {
              return n3.root.setAttribute(t3, e2);
            }, addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, activateIndicator: function(t3) {
              n3.tabIndicator.activate(t3);
            }, deactivateIndicator: function() {
              n3.tabIndicator.deactivate();
            }, notifyInteracted: function() {
              return n3.emit(l.MDCTabFoundation.strings.INTERACTED_EVENT, { tabId: n3.id }, true);
            }, getOffsetLeft: function() {
              return n3.root.offsetLeft;
            }, getOffsetWidth: function() {
              return n3.root.offsetWidth;
            }, getContentOffsetLeft: function() {
              return n3.content.offsetLeft;
            }, getContentOffsetWidth: function() {
              return n3.content.offsetWidth;
            }, focus: function() {
              return n3.root.focus();
            } };
            return new l.MDCTabFoundation(t2);
          }, Object.defineProperty(p2.prototype, "active", { get: function() {
            return this.foundation.isActive();
          }, enumerable: false, configurable: true }), Object.defineProperty(p2.prototype, "focusOnActivate", { set: function(t2) {
            this.foundation.setFocusOnActivate(t2);
          }, enumerable: false, configurable: true }), p2.prototype.activate = function(t2) {
            this.foundation.activate(t2);
          }, p2.prototype.deactivate = function() {
            this.foundation.deactivate();
          }, p2.prototype.computeIndicatorClientRect = function() {
            return this.tabIndicator.computeContentClientRect();
          }, p2.prototype.computeDimensions = function() {
            return this.foundation.computeDimensions();
          }, p2.prototype.focus = function() {
            this.root.focus();
          }, p2);
          function p2() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCTab = d;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabIndicator = void 0;
          var o, s = n2(1), a = n2(103), c = n2(17), u = n2(105), l = (o = s.MDCComponent, r2(d, o), d.attachTo = function(t2) {
            return new d(t2);
          }, d.prototype.initialize = function() {
            this.content = this.root.querySelector(c.MDCTabIndicatorFoundation.strings.CONTENT_SELECTOR);
          }, d.prototype.computeContentClientRect = function() {
            return this.foundation.computeContentClientRect();
          }, d.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, computeContentClientRect: function() {
              return n3.content.getBoundingClientRect();
            }, setContentStyleProperty: function(t3, e2) {
              n3.content.style.setProperty(t3, e2);
            } };
            return this.root.classList.contains(c.MDCTabIndicatorFoundation.cssClasses.FADE) ? new a.MDCFadingTabIndicatorFoundation(t2) : new u.MDCSlidingTabIndicatorFoundation(t2);
          }, d.prototype.activate = function(t2) {
            this.foundation.activate(t2);
          }, d.prototype.deactivate = function() {
            this.foundation.deactivate();
          }, d);
          function d() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCTabIndicator = l;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCFadingTabIndicatorFoundation = void 0;
          var o, s = n2(17), a = (o = s.MDCTabIndicatorFoundation, r2(c, o), c.prototype.activate = function() {
            this.adapter.addClass(s.MDCTabIndicatorFoundation.cssClasses.ACTIVE);
          }, c.prototype.deactivate = function() {
            this.adapter.removeClass(s.MDCTabIndicatorFoundation.cssClasses.ACTIVE);
          }, c);
          function c() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCFadingTabIndicatorFoundation = a, e.default = a;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.cssClasses = void 0;
          e.cssClasses = { ACTIVE: "mdc-tab-indicator--active", FADE: "mdc-tab-indicator--fade", NO_TRANSITION: "mdc-tab-indicator--no-transition" };
          e.strings = { CONTENT_SELECTOR: ".mdc-tab-indicator__content" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSlidingTabIndicatorFoundation = void 0;
          var o, s = n2(17), a = (o = s.MDCTabIndicatorFoundation, r2(c, o), c.prototype.activate = function(t2) {
            if (t2) {
              var e2 = this.computeContentClientRect(), n3 = t2.width / e2.width, i4 = t2.left - e2.left;
              this.adapter.addClass(s.MDCTabIndicatorFoundation.cssClasses.NO_TRANSITION), this.adapter.setContentStyleProperty("transform", "translateX(" + i4 + "px) scaleX(" + n3 + ")"), this.computeContentClientRect(), this.adapter.removeClass(s.MDCTabIndicatorFoundation.cssClasses.NO_TRANSITION), this.adapter.addClass(s.MDCTabIndicatorFoundation.cssClasses.ACTIVE), this.adapter.setContentStyleProperty("transform", "");
            } else this.adapter.addClass(s.MDCTabIndicatorFoundation.cssClasses.ACTIVE);
          }, c.prototype.deactivate = function() {
            this.adapter.removeClass(s.MDCTabIndicatorFoundation.cssClasses.ACTIVE);
          }, c);
          function c() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCSlidingTabIndicatorFoundation = a, e.default = a;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.cssClasses = void 0;
          e.cssClasses = { ACTIVE: "mdc-tab--active" };
          e.strings = { ARIA_SELECTED: "aria-selected", CONTENT_SELECTOR: ".mdc-tab__content", INTERACTED_EVENT: "MDCTab:interacted", RIPPLE_SELECTOR: ".mdc-tab__ripple", TABINDEX: "tabIndex", TAB_INDICATOR_SELECTOR: ".mdc-tab-indicator" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabBarFoundation = void 0;
          var s = n2(0), u = n2(108), a = /* @__PURE__ */ new Set();
          a.add(u.strings.ARROW_LEFT_KEY), a.add(u.strings.ARROW_RIGHT_KEY), a.add(u.strings.END_KEY), a.add(u.strings.HOME_KEY), a.add(u.strings.ENTER_KEY), a.add(u.strings.SPACE_KEY);
          var c = /* @__PURE__ */ new Map();
          c.set(u.numbers.ARROW_LEFT_KEYCODE, u.strings.ARROW_LEFT_KEY), c.set(u.numbers.ARROW_RIGHT_KEYCODE, u.strings.ARROW_RIGHT_KEY), c.set(u.numbers.END_KEYCODE, u.strings.END_KEY), c.set(u.numbers.HOME_KEYCODE, u.strings.HOME_KEY), c.set(u.numbers.ENTER_KEYCODE, u.strings.ENTER_KEY), c.set(u.numbers.SPACE_KEYCODE, u.strings.SPACE_KEY);
          var l, d = (l = s.MDCFoundation, r2(p2, l), Object.defineProperty(p2, "strings", { get: function() {
            return u.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "numbers", { get: function() {
            return u.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "defaultAdapter", { get: function() {
            return { scrollTo: function() {
            }, incrementScroll: function() {
            }, getScrollPosition: function() {
              return 0;
            }, getScrollContentWidth: function() {
              return 0;
            }, getOffsetWidth: function() {
              return 0;
            }, isRTL: function() {
              return false;
            }, setActiveTab: function() {
            }, activateTabAtIndex: function() {
            }, deactivateTabAtIndex: function() {
            }, focusTabAtIndex: function() {
            }, getTabIndicatorClientRectAtIndex: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, getTabDimensionsAtIndex: function() {
              return { rootLeft: 0, rootRight: 0, contentLeft: 0, contentRight: 0 };
            }, getPreviousActiveTabIndex: function() {
              return -1;
            }, getFocusedTabIndex: function() {
              return -1;
            }, getIndexOfTabById: function() {
              return -1;
            }, getTabListLength: function() {
              return 0;
            }, notifyTabActivated: function() {
            } };
          }, enumerable: false, configurable: true }), p2.prototype.setUseAutomaticActivation = function(t2) {
            this.useAutomaticActivation = t2;
          }, p2.prototype.activateTab = function(t2) {
            var e2, n3 = this.adapter.getPreviousActiveTabIndex();
            this.indexIsInRange(t2) && t2 !== n3 && (-1 !== n3 && (this.adapter.deactivateTabAtIndex(n3), e2 = this.adapter.getTabIndicatorClientRectAtIndex(n3)), this.adapter.activateTabAtIndex(t2, e2), this.scrollIntoView(t2), this.adapter.notifyTabActivated(t2));
          }, p2.prototype.handleKeyDown = function(t2) {
            var e2 = this.getKeyFromEvent(t2);
            if (void 0 !== e2) if (this.isActivationKey(e2) || t2.preventDefault(), this.useAutomaticActivation) {
              if (this.isActivationKey(e2)) return;
              var n3 = this.determineTargetFromKey(this.adapter.getPreviousActiveTabIndex(), e2);
              this.adapter.setActiveTab(n3), this.scrollIntoView(n3);
            } else {
              var i4 = this.adapter.getFocusedTabIndex();
              this.isActivationKey(e2) ? this.adapter.setActiveTab(i4) : (n3 = this.determineTargetFromKey(i4, e2), this.adapter.focusTabAtIndex(n3), this.scrollIntoView(n3));
            }
          }, p2.prototype.handleTabInteraction = function(t2) {
            this.adapter.setActiveTab(this.adapter.getIndexOfTabById(t2.detail.tabId));
          }, p2.prototype.scrollIntoView = function(t2) {
            this.indexIsInRange(t2) && (0 !== t2 ? t2 !== this.adapter.getTabListLength() - 1 ? this.isRTL() ? this.scrollIntoViewImplRTL(t2) : this.scrollIntoViewImpl(t2) : this.adapter.scrollTo(this.adapter.getScrollContentWidth()) : this.adapter.scrollTo(0));
          }, p2.prototype.determineTargetFromKey = function(t2, e2) {
            var n3 = this.isRTL(), i4 = this.adapter.getTabListLength() - 1, r3 = e2 === u.strings.END_KEY, o2 = e2 === u.strings.ARROW_LEFT_KEY && !n3 || e2 === u.strings.ARROW_RIGHT_KEY && n3, s2 = e2 === u.strings.ARROW_RIGHT_KEY && !n3 || e2 === u.strings.ARROW_LEFT_KEY && n3, a2 = t2;
            return r3 ? a2 = i4 : o2 ? a2 -= 1 : s2 ? a2 += 1 : a2 = 0, a2 < 0 ? a2 = i4 : i4 < a2 && (a2 = 0), a2;
          }, p2.prototype.calculateScrollIncrement = function(t2, e2, n3, i4) {
            var r3 = this.adapter.getTabDimensionsAtIndex(e2), o2 = r3.contentLeft - n3 - i4, s2 = r3.contentRight - n3 - u.numbers.EXTRA_SCROLL_AMOUNT, a2 = o2 + u.numbers.EXTRA_SCROLL_AMOUNT;
            return e2 < t2 ? Math.min(s2, 0) : Math.max(a2, 0);
          }, p2.prototype.calculateScrollIncrementRTL = function(t2, e2, n3, i4, r3) {
            var o2 = this.adapter.getTabDimensionsAtIndex(e2), s2 = r3 - o2.contentLeft - n3, a2 = r3 - o2.contentRight - n3 - i4 + u.numbers.EXTRA_SCROLL_AMOUNT, c2 = s2 - u.numbers.EXTRA_SCROLL_AMOUNT;
            return t2 < e2 ? Math.max(a2, 0) : Math.min(c2, 0);
          }, p2.prototype.findAdjacentTabIndexClosestToEdge = function(t2, e2, n3, i4) {
            var r3 = e2.rootLeft - n3, o2 = e2.rootRight - n3 - i4, s2 = r3 + o2;
            return r3 < 0 || s2 < 0 ? t2 - 1 : 0 < o2 || 0 < s2 ? t2 + 1 : -1;
          }, p2.prototype.findAdjacentTabIndexClosestToEdgeRTL = function(t2, e2, n3, i4, r3) {
            var o2 = r3 - e2.rootLeft - i4 - n3, s2 = r3 - e2.rootRight - n3, a2 = o2 + s2;
            return 0 < o2 || 0 < a2 ? t2 + 1 : s2 < 0 || a2 < 0 ? t2 - 1 : -1;
          }, p2.prototype.getKeyFromEvent = function(t2) {
            return a.has(t2.key) ? t2.key : c.get(t2.keyCode);
          }, p2.prototype.isActivationKey = function(t2) {
            return t2 === u.strings.SPACE_KEY || t2 === u.strings.ENTER_KEY;
          }, p2.prototype.indexIsInRange = function(t2) {
            return 0 <= t2 && t2 < this.adapter.getTabListLength();
          }, p2.prototype.isRTL = function() {
            return this.adapter.isRTL();
          }, p2.prototype.scrollIntoViewImpl = function(t2) {
            var e2 = this.adapter.getScrollPosition(), n3 = this.adapter.getOffsetWidth(), i4 = this.adapter.getTabDimensionsAtIndex(t2), r3 = this.findAdjacentTabIndexClosestToEdge(t2, i4, e2, n3);
            if (this.indexIsInRange(r3)) {
              var o2 = this.calculateScrollIncrement(t2, r3, e2, n3);
              this.adapter.incrementScroll(o2);
            }
          }, p2.prototype.scrollIntoViewImplRTL = function(t2) {
            var e2 = this.adapter.getScrollPosition(), n3 = this.adapter.getOffsetWidth(), i4 = this.adapter.getTabDimensionsAtIndex(t2), r3 = this.adapter.getScrollContentWidth(), o2 = this.findAdjacentTabIndexClosestToEdgeRTL(t2, i4, e2, n3, r3);
            if (this.indexIsInRange(o2)) {
              var s2 = this.calculateScrollIncrementRTL(t2, o2, e2, n3, r3);
              this.adapter.incrementScroll(s2);
            }
          }, p2);
          function p2(t2) {
            var e2 = l.call(this, o(o({}, p2.defaultAdapter), t2)) || this;
            return e2.useAutomaticActivation = false, e2;
          }
          e.MDCTabBarFoundation = d, e.default = d;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = e.numbers = void 0;
          e.strings = { ARROW_LEFT_KEY: "ArrowLeft", ARROW_RIGHT_KEY: "ArrowRight", END_KEY: "End", ENTER_KEY: "Enter", HOME_KEY: "Home", SPACE_KEY: "Space", TAB_ACTIVATED_EVENT: "MDCTabBar:activated", TAB_SCROLLER_SELECTOR: ".mdc-tab-scroller", TAB_SELECTOR: ".mdc-tab" };
          e.numbers = { ARROW_LEFT_KEYCODE: 37, ARROW_RIGHT_KEYCODE: 39, END_KEYCODE: 35, ENTER_KEYCODE: 13, EXTRA_SCROLL_AMOUNT: 20, HOME_KEYCODE: 36, SPACE_KEYCODE: 32 };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTextFieldCharacterCounter = void 0;
          var o, s = n2(1), a = n2(39), c = (o = s.MDCComponent, r2(u, o), u.attachTo = function(t2) {
            return new u(t2);
          }, Object.defineProperty(u.prototype, "foundationForTextField", { get: function() {
            return this.foundation;
          }, enumerable: false, configurable: true }), u.prototype.getDefaultFoundation = function() {
            var e2 = this, t2 = { setContent: function(t3) {
              e2.root.textContent = t3;
            } };
            return new a.MDCTextFieldCharacterCounterFoundation(t2);
          }, u);
          function u() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCTextFieldCharacterCounter = c;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = e.strings = void 0;
          var i3 = { ROOT: "mdc-text-field-character-counter" }, r2 = { ROOT_SELECTOR: "." + (e.cssClasses = i3).ROOT };
          e.strings = r2;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, u = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTextFieldFoundation = void 0;
          var s, a = n2(0), c = n2(40), l = ["mousedown", "touchstart"], d = ["click", "keydown"], p2 = (s = a.MDCFoundation, r2(h, s), Object.defineProperty(h, "cssClasses", { get: function() {
            return c.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "strings", { get: function() {
            return c.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "numbers", { get: function() {
            return c.numbers;
          }, enumerable: false, configurable: true }), Object.defineProperty(h.prototype, "shouldAlwaysFloat", { get: function() {
            var t2 = this.getNativeInput().type;
            return 0 <= c.ALWAYS_FLOAT_TYPES.indexOf(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(h.prototype, "shouldFloat", { get: function() {
            return this.shouldAlwaysFloat || this.isFocused || !!this.getValue() || this.isBadInput();
          }, enumerable: false, configurable: true }), Object.defineProperty(h.prototype, "shouldShake", { get: function() {
            return !this.isFocused && !this.isValid() && !!this.getValue();
          }, enumerable: false, configurable: true }), Object.defineProperty(h, "defaultAdapter", { get: function() {
            return { addClass: function() {
            }, removeClass: function() {
            }, hasClass: function() {
              return true;
            }, setInputAttr: function() {
            }, removeInputAttr: function() {
            }, registerTextFieldInteractionHandler: function() {
            }, deregisterTextFieldInteractionHandler: function() {
            }, registerInputInteractionHandler: function() {
            }, deregisterInputInteractionHandler: function() {
            }, registerValidationAttributeChangeHandler: function() {
              return new MutationObserver(function() {
              });
            }, deregisterValidationAttributeChangeHandler: function() {
            }, getNativeInput: function() {
              return null;
            }, isFocused: function() {
              return false;
            }, activateLineRipple: function() {
            }, deactivateLineRipple: function() {
            }, setLineRippleTransformOrigin: function() {
            }, shakeLabel: function() {
            }, floatLabel: function() {
            }, setLabelRequired: function() {
            }, hasLabel: function() {
              return false;
            }, getLabelWidth: function() {
              return 0;
            }, hasOutline: function() {
              return false;
            }, notchOutline: function() {
            }, closeOutline: function() {
            } };
          }, enumerable: false, configurable: true }), h.prototype.init = function() {
            var e2, t2, n3, i4;
            this.adapter.hasLabel() && this.getNativeInput().required && this.adapter.setLabelRequired(true), this.adapter.isFocused() ? this.inputFocusHandler() : this.adapter.hasLabel() && this.shouldFloat && (this.notchOutline(true), this.adapter.floatLabel(true), this.styleFloating(true)), this.adapter.registerInputInteractionHandler("focus", this.inputFocusHandler), this.adapter.registerInputInteractionHandler("blur", this.inputBlurHandler), this.adapter.registerInputInteractionHandler("input", this.inputInputHandler);
            try {
              for (var r3 = u(l), o2 = r3.next(); !o2.done; o2 = r3.next()) {
                var s2 = o2.value;
                this.adapter.registerInputInteractionHandler(s2, this.setPointerXOffset);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                o2 && !o2.done && (t2 = r3.return) && t2.call(r3);
              } finally {
                if (e2) throw e2.error;
              }
            }
            try {
              for (var a2 = u(d), c2 = a2.next(); !c2.done; c2 = a2.next()) s2 = c2.value, this.adapter.registerTextFieldInteractionHandler(s2, this.textFieldInteractionHandler);
            } catch (t3) {
              n3 = { error: t3 };
            } finally {
              try {
                c2 && !c2.done && (i4 = a2.return) && i4.call(a2);
              } finally {
                if (n3) throw n3.error;
              }
            }
            this.validationObserver = this.adapter.registerValidationAttributeChangeHandler(this.validationAttributeChangeHandler), this.setcharacterCounter(this.getValue().length);
          }, h.prototype.destroy = function() {
            var e2, t2, n3, i4;
            this.adapter.deregisterInputInteractionHandler("focus", this.inputFocusHandler), this.adapter.deregisterInputInteractionHandler("blur", this.inputBlurHandler), this.adapter.deregisterInputInteractionHandler("input", this.inputInputHandler);
            try {
              for (var r3 = u(l), o2 = r3.next(); !o2.done; o2 = r3.next()) {
                var s2 = o2.value;
                this.adapter.deregisterInputInteractionHandler(s2, this.setPointerXOffset);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                o2 && !o2.done && (t2 = r3.return) && t2.call(r3);
              } finally {
                if (e2) throw e2.error;
              }
            }
            try {
              for (var a2 = u(d), c2 = a2.next(); !c2.done; c2 = a2.next()) s2 = c2.value, this.adapter.deregisterTextFieldInteractionHandler(s2, this.textFieldInteractionHandler);
            } catch (t3) {
              n3 = { error: t3 };
            } finally {
              try {
                c2 && !c2.done && (i4 = a2.return) && i4.call(a2);
              } finally {
                if (n3) throw n3.error;
              }
            }
            this.adapter.deregisterValidationAttributeChangeHandler(this.validationObserver);
          }, h.prototype.handleTextFieldInteraction = function() {
            var t2 = this.adapter.getNativeInput();
            t2 && t2.disabled || (this.receivedUserInput = true);
          }, h.prototype.handleValidationAttributeChange = function(t2) {
            var e2 = this;
            t2.some(function(t3) {
              return -1 < c.VALIDATION_ATTR_WHITELIST.indexOf(t3) && (e2.styleValidity(true), e2.adapter.setLabelRequired(e2.getNativeInput().required), true);
            }), -1 < t2.indexOf("maxlength") && this.setcharacterCounter(this.getValue().length);
          }, h.prototype.notchOutline = function(t2) {
            if (this.adapter.hasOutline() && this.adapter.hasLabel()) if (t2) {
              var e2 = this.adapter.getLabelWidth() * c.numbers.LABEL_SCALE;
              this.adapter.notchOutline(e2);
            } else this.adapter.closeOutline();
          }, h.prototype.activateFocus = function() {
            this.isFocused = true, this.styleFocused(this.isFocused), this.adapter.activateLineRipple(), this.adapter.hasLabel() && (this.notchOutline(this.shouldFloat), this.adapter.floatLabel(this.shouldFloat), this.styleFloating(this.shouldFloat), this.adapter.shakeLabel(this.shouldShake)), !this.helperText || !this.helperText.isPersistent() && this.helperText.isValidation() && this.valid || this.helperText.showToScreenReader();
          }, h.prototype.setTransformOrigin = function(t2) {
            if (!this.isDisabled() && !this.adapter.hasOutline()) {
              var e2 = t2.touches, n3 = e2 ? e2[0] : t2, i4 = n3.target.getBoundingClientRect(), r3 = n3.clientX - i4.left;
              this.adapter.setLineRippleTransformOrigin(r3);
            }
          }, h.prototype.handleInput = function() {
            this.autoCompleteFocus(), this.setcharacterCounter(this.getValue().length);
          }, h.prototype.autoCompleteFocus = function() {
            this.receivedUserInput || this.activateFocus();
          }, h.prototype.deactivateFocus = function() {
            this.isFocused = false, this.adapter.deactivateLineRipple();
            var t2 = this.isValid();
            this.styleValidity(t2), this.styleFocused(this.isFocused), this.adapter.hasLabel() && (this.notchOutline(this.shouldFloat), this.adapter.floatLabel(this.shouldFloat), this.styleFloating(this.shouldFloat), this.adapter.shakeLabel(this.shouldShake)), this.shouldFloat || (this.receivedUserInput = false);
          }, h.prototype.getValue = function() {
            return this.getNativeInput().value;
          }, h.prototype.setValue = function(t2) {
            if (this.getValue() !== t2 && (this.getNativeInput().value = t2), this.setcharacterCounter(t2.length), this.validateOnValueChange) {
              var e2 = this.isValid();
              this.styleValidity(e2);
            }
            this.adapter.hasLabel() && (this.notchOutline(this.shouldFloat), this.adapter.floatLabel(this.shouldFloat), this.styleFloating(this.shouldFloat), this.validateOnValueChange && this.adapter.shakeLabel(this.shouldShake));
          }, h.prototype.isValid = function() {
            return this.useNativeValidation ? this.isNativeInputValid() : this.valid;
          }, h.prototype.setValid = function(t2) {
            this.valid = t2, this.styleValidity(t2);
            var e2 = !t2 && !this.isFocused && !!this.getValue();
            this.adapter.hasLabel() && this.adapter.shakeLabel(e2);
          }, h.prototype.setValidateOnValueChange = function(t2) {
            this.validateOnValueChange = t2;
          }, h.prototype.getValidateOnValueChange = function() {
            return this.validateOnValueChange;
          }, h.prototype.setUseNativeValidation = function(t2) {
            this.useNativeValidation = t2;
          }, h.prototype.isDisabled = function() {
            return this.getNativeInput().disabled;
          }, h.prototype.setDisabled = function(t2) {
            this.getNativeInput().disabled = t2, this.styleDisabled(t2);
          }, h.prototype.setHelperTextContent = function(t2) {
            this.helperText && this.helperText.setContent(t2);
          }, h.prototype.setLeadingIconAriaLabel = function(t2) {
            this.leadingIcon && this.leadingIcon.setAriaLabel(t2);
          }, h.prototype.setLeadingIconContent = function(t2) {
            this.leadingIcon && this.leadingIcon.setContent(t2);
          }, h.prototype.setTrailingIconAriaLabel = function(t2) {
            this.trailingIcon && this.trailingIcon.setAriaLabel(t2);
          }, h.prototype.setTrailingIconContent = function(t2) {
            this.trailingIcon && this.trailingIcon.setContent(t2);
          }, h.prototype.setcharacterCounter = function(t2) {
            if (this.characterCounter) {
              var e2 = this.getNativeInput().maxLength;
              if (-1 === e2) throw new Error("MDCTextFieldFoundation: Expected maxlength html property on text input or textarea.");
              this.characterCounter.setCounterValue(t2, e2);
            }
          }, h.prototype.isBadInput = function() {
            return this.getNativeInput().validity.badInput || false;
          }, h.prototype.isNativeInputValid = function() {
            return this.getNativeInput().validity.valid;
          }, h.prototype.styleValidity = function(t2) {
            var e2 = h.cssClasses.INVALID;
            if (t2 ? this.adapter.removeClass(e2) : this.adapter.addClass(e2), this.helperText) {
              if (this.helperText.setValidity(t2), !this.helperText.isValidation()) return;
              var n3 = this.helperText.isVisible(), i4 = this.helperText.getId();
              n3 && i4 ? this.adapter.setInputAttr(c.strings.ARIA_DESCRIBEDBY, i4) : this.adapter.removeInputAttr(c.strings.ARIA_DESCRIBEDBY);
            }
          }, h.prototype.styleFocused = function(t2) {
            var e2 = h.cssClasses.FOCUSED;
            t2 ? this.adapter.addClass(e2) : this.adapter.removeClass(e2);
          }, h.prototype.styleDisabled = function(t2) {
            var e2 = h.cssClasses, n3 = e2.DISABLED, i4 = e2.INVALID;
            t2 ? (this.adapter.addClass(n3), this.adapter.removeClass(i4)) : this.adapter.removeClass(n3), this.leadingIcon && this.leadingIcon.setDisabled(t2), this.trailingIcon && this.trailingIcon.setDisabled(t2);
          }, h.prototype.styleFloating = function(t2) {
            var e2 = h.cssClasses.LABEL_FLOATING;
            t2 ? this.adapter.addClass(e2) : this.adapter.removeClass(e2);
          }, h.prototype.getNativeInput = function() {
            return (this.adapter ? this.adapter.getNativeInput() : null) || { disabled: false, maxLength: -1, required: false, type: "input", validity: { badInput: false, valid: true }, value: "" };
          }, h);
          function h(t2, e2) {
            void 0 === e2 && (e2 = {});
            var n3 = s.call(this, o(o({}, h.defaultAdapter), t2)) || this;
            return n3.isFocused = false, n3.receivedUserInput = false, n3.valid = true, n3.useNativeValidation = true, n3.validateOnValueChange = true, n3.helperText = e2.helperText, n3.characterCounter = e2.characterCounter, n3.leadingIcon = e2.leadingIcon, n3.trailingIcon = e2.trailingIcon, n3.inputFocusHandler = function() {
              n3.activateFocus();
            }, n3.inputBlurHandler = function() {
              n3.deactivateFocus();
            }, n3.inputInputHandler = function() {
              n3.handleInput();
            }, n3.setPointerXOffset = function(t3) {
              n3.setTransformOrigin(t3);
            }, n3.textFieldInteractionHandler = function() {
              n3.handleTextFieldInteraction();
            }, n3.validationAttributeChangeHandler = function(t3) {
              n3.handleValidationAttributeChange(t3);
            }, n3;
          }
          e.MDCTextFieldFoundation = p2, e.default = p2;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTextFieldHelperText = void 0;
          var o, s = n2(1), a = n2(41), c = (o = s.MDCComponent, r2(u, o), u.attachTo = function(t2) {
            return new u(t2);
          }, Object.defineProperty(u.prototype, "foundationForTextField", { get: function() {
            return this.foundation;
          }, enumerable: false, configurable: true }), u.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, getAttr: function(t3) {
              return n3.root.getAttribute(t3);
            }, setAttr: function(t3, e2) {
              return n3.root.setAttribute(t3, e2);
            }, removeAttr: function(t3) {
              return n3.root.removeAttribute(t3);
            }, setContent: function(t3) {
              n3.root.textContent = t3;
            } };
            return new a.MDCTextFieldHelperTextFoundation(t2);
          }, u);
          function u() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCTextFieldHelperText = c;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = e.strings = void 0;
          var i3 = { HELPER_TEXT_PERSISTENT: "mdc-text-field-helper-text--persistent", HELPER_TEXT_VALIDATION_MSG: "mdc-text-field-helper-text--validation-msg", ROOT: "mdc-text-field-helper-text" }, r2 = { ARIA_HIDDEN: "aria-hidden", ROLE: "role", ROOT_SELECTOR: "." + (e.cssClasses = i3).ROOT };
          e.strings = r2;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTextFieldIcon = void 0;
          var o, s = n2(1), a = n2(115), c = (o = s.MDCComponent, r2(u, o), u.attachTo = function(t2) {
            return new u(t2);
          }, Object.defineProperty(u.prototype, "foundationForTextField", { get: function() {
            return this.foundation;
          }, enumerable: false, configurable: true }), u.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { getAttr: function(t3) {
              return n3.root.getAttribute(t3);
            }, setAttr: function(t3, e2) {
              return n3.root.setAttribute(t3, e2);
            }, removeAttr: function(t3) {
              return n3.root.removeAttribute(t3);
            }, setContent: function(t3) {
              n3.root.textContent = t3;
            }, registerInteractionHandler: function(t3, e2) {
              return n3.listen(t3, e2);
            }, deregisterInteractionHandler: function(t3, e2) {
              return n3.unlisten(t3, e2);
            }, notifyIconAction: function() {
              return n3.emit(a.MDCTextFieldIconFoundation.strings.ICON_EVENT, {}, true);
            } };
            return new a.MDCTextFieldIconFoundation(t2);
          }, u);
          function u() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCTextFieldIcon = c;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTextFieldIconFoundation = void 0;
          var a, c = n2(0), u = n2(116), l = ["click", "keydown"], d = (a = c.MDCFoundation, r2(p2, a), Object.defineProperty(p2, "strings", { get: function() {
            return u.strings;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "cssClasses", { get: function() {
            return u.cssClasses;
          }, enumerable: false, configurable: true }), Object.defineProperty(p2, "defaultAdapter", { get: function() {
            return { getAttr: function() {
              return null;
            }, setAttr: function() {
            }, removeAttr: function() {
            }, setContent: function() {
            }, registerInteractionHandler: function() {
            }, deregisterInteractionHandler: function() {
            }, notifyIconAction: function() {
            } };
          }, enumerable: false, configurable: true }), p2.prototype.init = function() {
            var e2, t2;
            this.savedTabIndex = this.adapter.getAttr("tabindex");
            try {
              for (var n3 = s(l), i4 = n3.next(); !i4.done; i4 = n3.next()) {
                var r3 = i4.value;
                this.adapter.registerInteractionHandler(r3, this.interactionHandler);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, p2.prototype.destroy = function() {
            var e2, t2;
            try {
              for (var n3 = s(l), i4 = n3.next(); !i4.done; i4 = n3.next()) {
                var r3 = i4.value;
                this.adapter.deregisterInteractionHandler(r3, this.interactionHandler);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, p2.prototype.setDisabled = function(t2) {
            this.savedTabIndex && (t2 ? (this.adapter.setAttr("tabindex", "-1"), this.adapter.removeAttr("role")) : (this.adapter.setAttr("tabindex", this.savedTabIndex), this.adapter.setAttr("role", u.strings.ICON_ROLE)));
          }, p2.prototype.setAriaLabel = function(t2) {
            this.adapter.setAttr("aria-label", t2);
          }, p2.prototype.setContent = function(t2) {
            this.adapter.setContent(t2);
          }, p2.prototype.handleInteraction = function(t2) {
            var e2 = "Enter" === t2.key || 13 === t2.keyCode;
            "click" !== t2.type && !e2 || (t2.preventDefault(), this.adapter.notifyIconAction());
          }, p2);
          function p2(t2) {
            var e2 = a.call(this, o(o({}, p2.defaultAdapter), t2)) || this;
            return e2.savedTabIndex = null, e2.interactionHandler = function(t3) {
              e2.handleInteraction(t3);
            }, e2;
          }
          e.MDCTextFieldIconFoundation = d, e.default = d;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.cssClasses = e.strings = void 0;
          e.strings = { ICON_EVENT: "MDCTextField:icon", ICON_ROLE: "button" };
          e.cssClasses = { ROOT: "mdc-text-field__icon" };
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, T = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTooltipFoundation = void 0;
          var s, a = n2(23), h = n2(10), c = n2(0), u = n2(6), b = n2(42), l = b.CssClasses.RICH, d = b.CssClasses.SHOWN, p2 = b.CssClasses.SHOWING, f = b.CssClasses.SHOWING_TRANSITION, y = b.CssClasses.HIDE, C = b.CssClasses.HIDE_TRANSITION, E = b.CssClasses.MULTILINE_TOOLTIP;
          (s = s || {}).POLL_ANCHOR = "poll_anchor";
          var g, _ = "undefined" != typeof window, m = (g = c.MDCFoundation, r2(v, g), Object.defineProperty(v, "defaultAdapter", { get: function() {
            return { getAttribute: function() {
              return null;
            }, setAttribute: function() {
            }, removeAttribute: function() {
            }, addClass: function() {
            }, hasClass: function() {
              return false;
            }, removeClass: function() {
            }, getComputedStyleProperty: function() {
              return "";
            }, setStyleProperty: function() {
            }, setSurfaceAnimationStyleProperty: function() {
            }, getViewportWidth: function() {
              return 0;
            }, getViewportHeight: function() {
              return 0;
            }, getTooltipSize: function() {
              return { width: 0, height: 0 };
            }, getAnchorBoundingRect: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, getParentBoundingRect: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, getAnchorAttribute: function() {
              return null;
            }, setAnchorAttribute: function() {
              return null;
            }, isRTL: function() {
              return false;
            }, anchorContainsElement: function() {
              return false;
            }, tooltipContainsElement: function() {
              return false;
            }, focusAnchorElement: function() {
            }, registerEventHandler: function() {
            }, deregisterEventHandler: function() {
            }, registerAnchorEventHandler: function() {
            }, deregisterAnchorEventHandler: function() {
            }, registerDocumentEventHandler: function() {
            }, deregisterDocumentEventHandler: function() {
            }, registerWindowEventHandler: function() {
            }, deregisterWindowEventHandler: function() {
            }, notifyHidden: function() {
            }, getTooltipCaretBoundingRect: function() {
              return { top: 0, right: 0, bottom: 0, left: 0, width: 0, height: 0 };
            }, setTooltipCaretStyle: function() {
            }, clearTooltipCaretStyles: function() {
            }, getActiveElement: function() {
              return null;
            } };
          }, enumerable: false, configurable: true }), v.prototype.init = function() {
            this.richTooltip = this.adapter.hasClass(l), this.persistentTooltip = "true" === this.adapter.getAttribute(b.attributes.PERSISTENT), this.interactiveTooltip = !!this.adapter.getAnchorAttribute(b.attributes.ARIA_EXPANDED) && "dialog" === this.adapter.getAnchorAttribute(b.attributes.ARIA_HASPOPUP), this.hasCaret = this.richTooltip && "true" === this.adapter.getAttribute(b.attributes.HAS_CARET);
          }, v.prototype.isShown = function() {
            return this.tooltipShown;
          }, v.prototype.isRich = function() {
            return this.richTooltip;
          }, v.prototype.isPersistent = function() {
            return this.persistentTooltip;
          }, v.prototype.handleAnchorMouseEnter = function() {
            var t2 = this;
            this.tooltipShown ? this.show() : (this.clearHideTimeout(), this.showTimeout = setTimeout(function() {
              t2.show();
            }, this.showDelayMs));
          }, v.prototype.handleAnchorTouchstart = function() {
            var t2 = this;
            this.showTimeout = setTimeout(function() {
              t2.show();
            }, this.showDelayMs), this.adapter.registerWindowEventHandler("contextmenu", this.preventContextMenuOnLongTouch);
          }, v.prototype.preventContextMenuOnLongTouch = function(t2) {
            t2.preventDefault();
          }, v.prototype.handleAnchorTouchend = function() {
            this.clearShowTimeout(), this.isShown() || this.adapter.deregisterWindowEventHandler("contextmenu", this.preventContextMenuOnLongTouch);
          }, v.prototype.handleAnchorFocus = function(t2) {
            var e2 = this, n3 = t2.relatedTarget;
            n3 instanceof HTMLElement && this.adapter.tooltipContainsElement(n3) || (this.showTimeout = setTimeout(function() {
              e2.show();
            }, this.showDelayMs));
          }, v.prototype.handleAnchorMouseLeave = function() {
            var t2 = this;
            this.clearShowTimeout(), this.hideTimeout = setTimeout(function() {
              t2.hide();
            }, this.hideDelayMs);
          }, v.prototype.handleAnchorClick = function() {
            this.tooltipShown ? this.hide() : this.show();
          }, v.prototype.handleDocumentClick = function(t2) {
            var e2 = t2.target instanceof HTMLElement && (this.adapter.anchorContainsElement(t2.target) || this.adapter.tooltipContainsElement(t2.target));
            this.richTooltip && this.persistentTooltip && e2 || this.hide();
          }, v.prototype.handleKeydown = function(t2) {
            if (u.normalizeKey(t2) === u.KEY.ESCAPE) {
              var e2 = this.adapter.getActiveElement();
              e2 instanceof HTMLElement && this.adapter.tooltipContainsElement(e2) && this.adapter.focusAnchorElement(), this.hide();
            }
          }, v.prototype.handleAnchorBlur = function(t2) {
            if (this.richTooltip) {
              if (t2.relatedTarget instanceof HTMLElement && this.adapter.tooltipContainsElement(t2.relatedTarget)) return;
              if (null === t2.relatedTarget && this.interactiveTooltip) return;
            }
            this.hide();
          }, v.prototype.handleTooltipMouseEnter = function() {
            this.show();
          }, v.prototype.handleTooltipMouseLeave = function() {
            var t2 = this;
            this.clearShowTimeout(), this.hideTimeout = setTimeout(function() {
              t2.hide();
            }, this.hideDelayMs);
          }, v.prototype.handleRichTooltipFocusOut = function(t2) {
            t2.relatedTarget instanceof HTMLElement && (this.adapter.anchorContainsElement(t2.relatedTarget) || this.adapter.tooltipContainsElement(t2.relatedTarget)) || null === t2.relatedTarget && this.interactiveTooltip || this.hide();
          }, v.prototype.handleWindowScrollEvent = function() {
            this.persistentTooltip ? this.handleWindowChangeEvent() : this.hide();
          }, v.prototype.handleWindowChangeEvent = function() {
            var t2 = this;
            this.animFrame.request(s.POLL_ANCHOR, function() {
              t2.repositionTooltipOnAnchorMove();
            });
          }, v.prototype.show = function() {
            var e2, t2, n3 = this;
            if (this.clearHideTimeout(), this.clearShowTimeout(), !this.tooltipShown) {
              this.tooltipShown = true, this.adapter.removeAttribute("aria-hidden"), this.richTooltip && (this.interactiveTooltip && this.adapter.setAnchorAttribute("aria-expanded", "true"), this.adapter.registerEventHandler("focusout", this.richTooltipFocusOutHandler)), this.persistentTooltip || (this.adapter.registerEventHandler("mouseenter", this.tooltipMouseEnterHandler), this.adapter.registerEventHandler("mouseleave", this.tooltipMouseLeaveHandler)), this.adapter.removeClass(y), this.adapter.addClass(p2), this.isTooltipMultiline() && !this.richTooltip && this.adapter.addClass(E), this.anchorRect = this.adapter.getAnchorBoundingRect(), this.parentRect = this.adapter.getParentBoundingRect(), this.richTooltip ? this.positionRichTooltip() : this.positionPlainTooltip(), this.adapter.registerAnchorEventHandler("blur", this.anchorBlurHandler), this.adapter.registerDocumentEventHandler("click", this.documentClickHandler), this.adapter.registerDocumentEventHandler("keydown", this.documentKeydownHandler), this.adapter.registerWindowEventHandler("scroll", this.windowScrollHandler), this.adapter.registerWindowEventHandler("resize", this.windowResizeHandler);
              try {
                for (var i4 = T(this.addAncestorScrollEventListeners), r3 = i4.next(); !r3.done; r3 = i4.next()) (0, r3.value)();
              } catch (t3) {
                e2 = { error: t3 };
              } finally {
                try {
                  r3 && !r3.done && (t2 = i4.return) && t2.call(i4);
                } finally {
                  if (e2) throw e2.error;
                }
              }
              this.frameId = requestAnimationFrame(function() {
                n3.clearAllAnimationClasses(), n3.adapter.addClass(d), n3.adapter.addClass(f);
              });
            }
          }, v.prototype.hide = function() {
            var e2, t2;
            if (this.clearHideTimeout(), this.clearShowTimeout(), this.tooltipShown) {
              this.frameId && cancelAnimationFrame(this.frameId), this.tooltipShown = false, this.adapter.setAttribute("aria-hidden", "true"), this.adapter.deregisterEventHandler("focusout", this.richTooltipFocusOutHandler), this.richTooltip && this.interactiveTooltip && this.adapter.setAnchorAttribute("aria-expanded", "false"), this.persistentTooltip || (this.adapter.deregisterEventHandler("mouseenter", this.tooltipMouseEnterHandler), this.adapter.deregisterEventHandler("mouseleave", this.tooltipMouseLeaveHandler)), this.clearAllAnimationClasses(), this.adapter.addClass(y), this.adapter.addClass(C), this.adapter.removeClass(d), this.adapter.deregisterAnchorEventHandler("blur", this.anchorBlurHandler), this.adapter.deregisterDocumentEventHandler("click", this.documentClickHandler), this.adapter.deregisterDocumentEventHandler("keydown", this.documentKeydownHandler), this.adapter.deregisterWindowEventHandler("scroll", this.windowScrollHandler), this.adapter.deregisterWindowEventHandler("resize", this.windowResizeHandler), this.adapter.deregisterWindowEventHandler("contextmenu", this.preventContextMenuOnLongTouch);
              try {
                for (var n3 = T(this.removeAncestorScrollEventListeners), i4 = n3.next(); !i4.done; i4 = n3.next()) (0, i4.value)();
              } catch (t3) {
                e2 = { error: t3 };
              } finally {
                try {
                  i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
                } finally {
                  if (e2) throw e2.error;
                }
              }
            }
          }, v.prototype.handleTransitionEnd = function() {
            var t2 = this.adapter.hasClass(y);
            this.adapter.removeClass(p2), this.adapter.removeClass(f), this.adapter.removeClass(y), this.adapter.removeClass(C), t2 && null === this.showTimeout && this.adapter.notifyHidden();
          }, v.prototype.clearAllAnimationClasses = function() {
            this.adapter.removeClass(f), this.adapter.removeClass(C);
          }, v.prototype.setTooltipPosition = function(t2) {
            var e2 = t2.xPos, n3 = t2.yPos, i4 = t2.withCaretPos;
            this.hasCaret && i4 ? this.tooltipPositionWithCaret = i4 : (e2 && (this.xTooltipPos = e2), n3 && (this.yTooltipPos = n3));
          }, v.prototype.setAnchorBoundaryType = function(t2) {
            t2 === b.AnchorBoundaryType.UNBOUNDED ? this.anchorGap = b.numbers.UNBOUNDED_ANCHOR_GAP : this.anchorGap = b.numbers.BOUNDED_ANCHOR_GAP;
          }, v.prototype.setShowDelay = function(t2) {
            this.showDelayMs = t2;
          }, v.prototype.setHideDelay = function(t2) {
            this.hideDelayMs = t2;
          }, v.prototype.isTooltipMultiline = function() {
            var t2 = this.adapter.getTooltipSize();
            return t2.height > b.numbers.MIN_HEIGHT && t2.width >= b.numbers.MAX_WIDTH;
          }, v.prototype.positionPlainTooltip = function() {
            var t2 = this.calculateTooltipStyles(this.anchorRect), e2 = t2.top, n3 = t2.yTransformOrigin, i4 = t2.left, r3 = t2.xTransformOrigin, o2 = _ ? h.getCorrectPropertyName(window, "transform") : "transform";
            this.adapter.setSurfaceAnimationStyleProperty(o2 + "-origin", r3 + " " + n3), this.adapter.setStyleProperty("top", e2 + "px"), this.adapter.setStyleProperty("left", i4 + "px");
          }, v.prototype.positionRichTooltip = function() {
            var t2, e2, n3, i4, r3 = this.adapter.getComputedStyleProperty("width");
            this.adapter.setStyleProperty("width", r3);
            var o2 = this.hasCaret ? this.calculateTooltipWithCaretStyles(this.anchorRect) : this.calculateTooltipStyles(this.anchorRect), s2 = o2.top, a2 = o2.yTransformOrigin, c2 = o2.left, u2 = o2.xTransformOrigin, l2 = _ ? h.getCorrectPropertyName(window, "transform") : "transform";
            this.adapter.setSurfaceAnimationStyleProperty(l2 + "-origin", u2 + " " + a2);
            var d2 = c2 - (null !== (e2 = null === (t2 = this.parentRect) || void 0 === t2 ? void 0 : t2.left) && void 0 !== e2 ? e2 : 0), p3 = s2 - (null !== (i4 = null === (n3 = this.parentRect) || void 0 === n3 ? void 0 : n3.top) && void 0 !== i4 ? i4 : 0);
            this.adapter.setStyleProperty("top", p3 + "px"), this.adapter.setStyleProperty("left", d2 + "px");
          }, v.prototype.calculateTooltipStyles = function(t2) {
            if (!t2) return { top: 0, left: 0 };
            var e2 = this.adapter.getTooltipSize(), n3 = this.calculateYTooltipDistance(t2, e2.height), i4 = this.calculateXTooltipDistance(t2, e2.width);
            return { top: n3.distance, yTransformOrigin: n3.yTransformOrigin, left: i4.distance, xTransformOrigin: i4.xTransformOrigin };
          }, v.prototype.calculateXTooltipDistance = function(t2, e2) {
            var n3, i4, r3, o2, s2, a2 = !this.adapter.isRTL();
            s2 = this.richTooltip ? (n3 = a2 ? t2.left - e2 : t2.right, i4 = a2 ? t2.right : t2.left - e2, o2 = a2 ? b.strings.RIGHT : b.strings.LEFT, a2 ? b.strings.LEFT : b.strings.RIGHT) : (n3 = a2 ? t2.left : t2.right - e2, i4 = a2 ? t2.right - e2 : t2.left, r3 = t2.left + (t2.width - e2) / 2, o2 = a2 ? b.strings.LEFT : b.strings.RIGHT, a2 ? b.strings.RIGHT : b.strings.LEFT);
            var c2 = this.richTooltip ? this.determineValidPositionOptions(n3, i4) : this.determineValidPositionOptions(r3, n3, i4);
            if (this.xTooltipPos === b.XPosition.START && c2.has(n3)) return { distance: n3, xTransformOrigin: o2 };
            if (this.xTooltipPos === b.XPosition.END && c2.has(i4)) return { distance: i4, xTransformOrigin: s2 };
            if (this.xTooltipPos === b.XPosition.CENTER && c2.has(r3)) return { distance: r3, xTransformOrigin: b.strings.CENTER };
            var u2 = (this.richTooltip ? [{ distance: i4, xTransformOrigin: s2 }, { distance: n3, xTransformOrigin: o2 }] : [{ distance: r3, xTransformOrigin: b.strings.CENTER }, { distance: n3, xTransformOrigin: o2 }, { distance: i4, xTransformOrigin: s2 }]).find(function(t3) {
              var e3 = t3.distance;
              return c2.has(e3);
            });
            return u2 || (t2.left < 0 ? { distance: this.minViewportTooltipThreshold, xTransformOrigin: b.strings.LEFT } : { distance: this.adapter.getViewportWidth() - (e2 + this.minViewportTooltipThreshold), xTransformOrigin: b.strings.RIGHT });
          }, v.prototype.determineValidPositionOptions = function() {
            for (var e2, t2, n3 = [], i4 = 0; i4 < arguments.length; i4++) n3[i4] = arguments[i4];
            var r3 = /* @__PURE__ */ new Set(), o2 = /* @__PURE__ */ new Set();
            try {
              for (var s2 = T(n3), a2 = s2.next(); !a2.done; a2 = s2.next()) {
                var c2 = a2.value;
                this.positionHonorsViewportThreshold(c2) ? r3.add(c2) : this.positionDoesntCollideWithViewport(c2) && o2.add(c2);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                a2 && !a2.done && (t2 = s2.return) && t2.call(s2);
              } finally {
                if (e2) throw e2.error;
              }
            }
            return r3.size ? r3 : o2;
          }, v.prototype.positionHonorsViewportThreshold = function(t2) {
            var e2 = this.adapter.getViewportWidth();
            return t2 + this.adapter.getTooltipSize().width <= e2 - this.minViewportTooltipThreshold && t2 >= this.minViewportTooltipThreshold;
          }, v.prototype.positionDoesntCollideWithViewport = function(t2) {
            var e2 = this.adapter.getViewportWidth();
            return t2 + this.adapter.getTooltipSize().width <= e2 && 0 <= t2;
          }, v.prototype.calculateYTooltipDistance = function(t2, e2) {
            var n3 = t2.bottom + this.anchorGap, i4 = t2.top - (this.anchorGap + e2), r3 = this.determineValidYPositionOptions(i4, n3);
            return this.yTooltipPos === b.YPosition.ABOVE && r3.has(i4) ? { distance: i4, yTransformOrigin: b.strings.BOTTOM } : this.yTooltipPos === b.YPosition.BELOW && r3.has(n3) ? { distance: n3, yTransformOrigin: b.strings.TOP } : r3.has(n3) ? { distance: n3, yTransformOrigin: b.strings.TOP } : r3.has(i4) ? { distance: i4, yTransformOrigin: b.strings.BOTTOM } : { distance: n3, yTransformOrigin: b.strings.TOP };
          }, v.prototype.determineValidYPositionOptions = function(t2, e2) {
            var n3 = /* @__PURE__ */ new Set(), i4 = /* @__PURE__ */ new Set();
            return this.yPositionHonorsViewportThreshold(t2) ? n3.add(t2) : this.yPositionDoesntCollideWithViewport(t2) && i4.add(t2), this.yPositionHonorsViewportThreshold(e2) ? n3.add(e2) : this.yPositionDoesntCollideWithViewport(e2) && i4.add(e2), n3.size ? n3 : i4;
          }, v.prototype.yPositionHonorsViewportThreshold = function(t2) {
            var e2 = this.adapter.getViewportHeight();
            return t2 + this.adapter.getTooltipSize().height + this.minViewportTooltipThreshold <= e2 && t2 >= this.minViewportTooltipThreshold;
          }, v.prototype.yPositionDoesntCollideWithViewport = function(t2) {
            var e2 = this.adapter.getViewportHeight();
            return t2 + this.adapter.getTooltipSize().height <= e2 && 0 <= t2;
          }, v.prototype.calculateTooltipWithCaretStyles = function(t2) {
            this.adapter.clearTooltipCaretStyles();
            var e2 = this.adapter.getTooltipCaretBoundingRect();
            if (!t2 || !e2) return { position: b.PositionWithCaret.DETECTED, top: 0, left: 0 };
            var n3 = e2.width / b.numbers.ANIMATION_SCALE, i4 = e2.height / b.numbers.ANIMATION_SCALE / 2, r3 = this.adapter.getTooltipSize(), o2 = this.calculateYWithCaretDistanceOptions(t2, r3.height, { caretWidth: n3, caretHeight: i4 }), s2 = this.calculateXWithCaretDistanceOptions(t2, r3.width, { caretWidth: n3, caretHeight: i4 }), a2 = this.validateTooltipWithCaretDistances(o2, s2);
            a2.size < 1 && (a2 = this.generateBackupPositionOption(t2, r3, { caretWidth: n3, caretHeight: i4 }));
            var c2 = this.determineTooltipWithCaretDistance(a2), u2 = c2.position, l2 = c2.xDistance, d2 = c2.yDistance, p3 = this.setCaretPositionStyles(u2, { caretWidth: n3, caretHeight: i4 });
            return { yTransformOrigin: p3.yTransformOrigin, xTransformOrigin: p3.xTransformOrigin, top: d2, left: l2 };
          }, v.prototype.calculateXWithCaretDistanceOptions = function(t2, e2, n3) {
            var i4 = n3.caretWidth, r3 = n3.caretHeight, o2 = !this.adapter.isRTL(), s2 = t2.left + t2.width / 2, a2 = t2.left - (e2 + this.anchorGap + r3), c2 = t2.right + this.anchorGap + r3, u2 = o2 ? a2 : c2, l2 = o2 ? c2 : a2, d2 = s2 - (b.numbers.CARET_INDENTATION + i4 / 2), p3 = s2 - (e2 - b.numbers.CARET_INDENTATION - i4 / 2), h7 = o2 ? d2 : p3, f2 = o2 ? p3 : d2, y2 = s2 - e2 / 2;
            return /* @__PURE__ */ new Map([[b.XPositionWithCaret.START, h7], [b.XPositionWithCaret.CENTER, y2], [b.XPositionWithCaret.END, f2], [b.XPositionWithCaret.SIDE_END, l2], [b.XPositionWithCaret.SIDE_START, u2]]);
          }, v.prototype.calculateYWithCaretDistanceOptions = function(t2, e2, n3) {
            var i4 = n3.caretWidth, r3 = n3.caretHeight, o2 = t2.top + t2.height / 2, s2 = t2.bottom + this.anchorGap + r3, a2 = t2.top - (this.anchorGap + e2 + r3), c2 = o2 - (b.numbers.CARET_INDENTATION + i4 / 2), u2 = o2 - e2 / 2, l2 = o2 - (e2 - b.numbers.CARET_INDENTATION - i4 / 2);
            return /* @__PURE__ */ new Map([[b.YPositionWithCaret.ABOVE, a2], [b.YPositionWithCaret.BELOW, s2], [b.YPositionWithCaret.SIDE_TOP, c2], [b.YPositionWithCaret.SIDE_CENTER, u2], [b.YPositionWithCaret.SIDE_BOTTOM, l2]]);
          }, v.prototype.repositionTooltipOnAnchorMove = function() {
            var t2 = this.adapter.getAnchorBoundingRect();
            t2 && this.anchorRect && (t2.top === this.anchorRect.top && t2.left === this.anchorRect.left && t2.height === this.anchorRect.height && t2.width === this.anchorRect.width || (this.anchorRect = t2, this.parentRect = this.adapter.getParentBoundingRect(), this.richTooltip ? this.positionRichTooltip() : this.positionPlainTooltip()));
          }, v.prototype.validateTooltipWithCaretDistances = function(t2, e2) {
            var n3, i4, r3, o2, s2, a2, c2 = /* @__PURE__ */ new Map(), u2 = /* @__PURE__ */ new Map(), l2 = /* @__PURE__ */ new Map([[b.YPositionWithCaret.ABOVE, [b.XPositionWithCaret.START, b.XPositionWithCaret.CENTER, b.XPositionWithCaret.END]], [b.YPositionWithCaret.BELOW, [b.XPositionWithCaret.START, b.XPositionWithCaret.CENTER, b.XPositionWithCaret.END]], [b.YPositionWithCaret.SIDE_TOP, [b.XPositionWithCaret.SIDE_START, b.XPositionWithCaret.SIDE_END]], [b.YPositionWithCaret.SIDE_CENTER, [b.XPositionWithCaret.SIDE_START, b.XPositionWithCaret.SIDE_END]], [b.YPositionWithCaret.SIDE_BOTTOM, [b.XPositionWithCaret.SIDE_START, b.XPositionWithCaret.SIDE_END]]]);
            try {
              for (var d2 = T(l2.keys()), p3 = d2.next(); !p3.done; p3 = d2.next()) {
                var h7 = p3.value, f2 = t2.get(h7);
                if (this.yPositionHonorsViewportThreshold(f2)) try {
                  for (var y2 = (r3 = void 0, T(l2.get(h7))), C2 = y2.next(); !C2.done; C2 = y2.next()) {
                    var E2 = C2.value, g2 = e2.get(E2);
                    if (this.positionHonorsViewportThreshold(g2)) {
                      var _2 = this.caretPositionOptionsMapping(E2, h7);
                      c2.set(_2, { xDistance: g2, yDistance: f2 });
                    }
                  }
                } catch (t3) {
                  r3 = { error: t3 };
                } finally {
                  try {
                    C2 && !C2.done && (o2 = y2.return) && o2.call(y2);
                  } finally {
                    if (r3) throw r3.error;
                  }
                }
                if (this.yPositionDoesntCollideWithViewport(f2)) try {
                  for (var m2 = (s2 = void 0, T(l2.get(h7))), v2 = m2.next(); !v2.done; v2 = m2.next()) E2 = v2.value, g2 = e2.get(E2), this.positionDoesntCollideWithViewport(g2) && (_2 = this.caretPositionOptionsMapping(E2, h7), u2.set(_2, { xDistance: g2, yDistance: f2 }));
                } catch (t3) {
                  s2 = { error: t3 };
                } finally {
                  try {
                    v2 && !v2.done && (a2 = m2.return) && a2.call(m2);
                  } finally {
                    if (s2) throw s2.error;
                  }
                }
              }
            } catch (t3) {
              n3 = { error: t3 };
            } finally {
              try {
                p3 && !p3.done && (i4 = d2.return) && i4.call(d2);
              } finally {
                if (n3) throw n3.error;
              }
            }
            return c2.size ? c2 : u2;
          }, v.prototype.generateBackupPositionOption = function(t2, e2, n3) {
            var i4, r3, o2, s2, a2 = !this.adapter.isRTL();
            r3 = t2.left < 0 ? (i4 = this.minViewportTooltipThreshold + n3.caretHeight, a2 ? b.XPositionWithCaret.END : b.XPositionWithCaret.START) : (i4 = this.adapter.getViewportWidth() - (e2.width + this.minViewportTooltipThreshold + n3.caretHeight), a2 ? b.XPositionWithCaret.START : b.XPositionWithCaret.END), s2 = t2.top < 0 ? (o2 = this.minViewportTooltipThreshold + n3.caretHeight, b.YPositionWithCaret.BELOW) : (o2 = this.adapter.getViewportHeight() - (e2.height + this.minViewportTooltipThreshold + n3.caretHeight), b.YPositionWithCaret.ABOVE);
            var c2 = this.caretPositionOptionsMapping(r3, s2);
            return /* @__PURE__ */ new Map([[c2, { xDistance: i4, yDistance: o2 }]]);
          }, v.prototype.determineTooltipWithCaretDistance = function(e2) {
            if (e2.has(this.tooltipPositionWithCaret)) {
              var t2 = e2.get(this.tooltipPositionWithCaret);
              return { position: this.tooltipPositionWithCaret, xDistance: t2.xDistance, yDistance: t2.yDistance };
            }
            var n3 = [b.PositionWithCaret.ABOVE_START, b.PositionWithCaret.ABOVE_CENTER, b.PositionWithCaret.ABOVE_END, b.PositionWithCaret.TOP_SIDE_START, b.PositionWithCaret.CENTER_SIDE_START, b.PositionWithCaret.BOTTOM_SIDE_START, b.PositionWithCaret.TOP_SIDE_END, b.PositionWithCaret.CENTER_SIDE_END, b.PositionWithCaret.BOTTOM_SIDE_END, b.PositionWithCaret.BELOW_START, b.PositionWithCaret.BELOW_CENTER, b.PositionWithCaret.BELOW_END].find(function(t3) {
              return e2.has(t3);
            }), i4 = e2.get(n3);
            return { position: n3, xDistance: i4.xDistance, yDistance: i4.yDistance };
          }, v.prototype.caretPositionOptionsMapping = function(t2, e2) {
            switch (e2) {
              case b.YPositionWithCaret.ABOVE:
                if (t2 === b.XPositionWithCaret.START) return b.PositionWithCaret.ABOVE_START;
                if (t2 === b.XPositionWithCaret.CENTER) return b.PositionWithCaret.ABOVE_CENTER;
                if (t2 === b.XPositionWithCaret.END) return b.PositionWithCaret.ABOVE_END;
                break;
              case b.YPositionWithCaret.BELOW:
                if (t2 === b.XPositionWithCaret.START) return b.PositionWithCaret.BELOW_START;
                if (t2 === b.XPositionWithCaret.CENTER) return b.PositionWithCaret.BELOW_CENTER;
                if (t2 === b.XPositionWithCaret.END) return b.PositionWithCaret.BELOW_END;
                break;
              case b.YPositionWithCaret.SIDE_TOP:
                if (t2 === b.XPositionWithCaret.SIDE_START) return b.PositionWithCaret.TOP_SIDE_START;
                if (t2 === b.XPositionWithCaret.SIDE_END) return b.PositionWithCaret.TOP_SIDE_END;
                break;
              case b.YPositionWithCaret.SIDE_CENTER:
                if (t2 === b.XPositionWithCaret.SIDE_START) return b.PositionWithCaret.CENTER_SIDE_START;
                if (t2 === b.XPositionWithCaret.SIDE_END) return b.PositionWithCaret.CENTER_SIDE_END;
                break;
              case b.YPositionWithCaret.SIDE_BOTTOM:
                if (t2 === b.XPositionWithCaret.SIDE_START) return b.PositionWithCaret.BOTTOM_SIDE_START;
                if (t2 === b.XPositionWithCaret.SIDE_END) return b.PositionWithCaret.BOTTOM_SIDE_END;
            }
            throw new Error("MDCTooltipFoundation: Invalid caret position of " + t2 + ", " + e2);
          }, v.prototype.setCaretPositionStyles = function(t2, e2) {
            var n3, i4, r3 = this.calculateCaretPositionOnTooltip(t2, e2);
            if (!r3) return { yTransformOrigin: 0, xTransformOrigin: 0 };
            this.adapter.clearTooltipCaretStyles(), this.adapter.setTooltipCaretStyle(r3.yAlignment, r3.yAxisPx), this.adapter.setTooltipCaretStyle(r3.xAlignment, r3.xAxisPx);
            var o2 = r3.skew * (Math.PI / 180), s2 = Math.cos(o2);
            this.adapter.setTooltipCaretStyle("transform", "rotate(" + r3.rotation + "deg) skewY(" + r3.skew + "deg) scaleX(" + s2 + ")"), this.adapter.setTooltipCaretStyle("transform-origin", r3.xAlignment + " " + r3.yAlignment);
            try {
              for (var a2 = T(r3.caretCorners), c2 = a2.next(); !c2.done; c2 = a2.next()) {
                var u2 = c2.value;
                this.adapter.setTooltipCaretStyle(u2, "0");
              }
            } catch (t3) {
              n3 = { error: t3 };
            } finally {
              try {
                c2 && !c2.done && (i4 = a2.return) && i4.call(a2);
              } finally {
                if (n3) throw n3.error;
              }
            }
            return { yTransformOrigin: r3.yTransformOrigin, xTransformOrigin: r3.xTransformOrigin };
          }, v.prototype.calculateCaretPositionOnTooltip = function(t2, e2) {
            var n3 = !this.adapter.isRTL(), i4 = this.adapter.getComputedStyleProperty("width"), r3 = this.adapter.getComputedStyleProperty("height");
            if (i4 && r3 && e2) {
              var o2 = "calc((" + i4 + " - " + e2.caretWidth + "px) / 2)", s2 = "calc((" + r3 + " - " + e2.caretWidth + "px) / 2)", a2 = b.numbers.CARET_INDENTATION + "px", c2 = "calc(" + i4 + " - " + a2 + ")", u2 = "calc(" + r3 + " - " + a2 + ")", l2 = Math.abs(55), d2 = ["border-bottom-right-radius", "border-top-left-radius"], p3 = ["border-bottom-left-radius", "border-top-right-radius"];
              switch (t2) {
                case b.PositionWithCaret.BELOW_CENTER:
                  return { yAlignment: b.strings.TOP, xAlignment: b.strings.LEFT, yAxisPx: "0", xAxisPx: o2, rotation: -35, skew: -20, xTransformOrigin: o2, yTransformOrigin: "0", caretCorners: d2 };
                case b.PositionWithCaret.BELOW_END:
                  return { yAlignment: b.strings.TOP, xAlignment: n3 ? b.strings.RIGHT : b.strings.LEFT, yAxisPx: "0", xAxisPx: a2, rotation: n3 ? 35 : -35, skew: n3 ? 20 : -20, xTransformOrigin: n3 ? c2 : a2, yTransformOrigin: "0", caretCorners: n3 ? p3 : d2 };
                case b.PositionWithCaret.BELOW_START:
                  return { yAlignment: b.strings.TOP, xAlignment: n3 ? b.strings.LEFT : b.strings.RIGHT, yAxisPx: "0", xAxisPx: a2, rotation: n3 ? -35 : 35, skew: n3 ? -20 : 20, xTransformOrigin: n3 ? a2 : c2, yTransformOrigin: "0", caretCorners: n3 ? d2 : p3 };
                case b.PositionWithCaret.TOP_SIDE_END:
                  return { yAlignment: b.strings.TOP, xAlignment: n3 ? b.strings.LEFT : b.strings.RIGHT, yAxisPx: a2, xAxisPx: "0", rotation: n3 ? l2 : -1 * l2, skew: n3 ? -20 : 20, xTransformOrigin: n3 ? "0" : i4, yTransformOrigin: a2, caretCorners: n3 ? d2 : p3 };
                case b.PositionWithCaret.CENTER_SIDE_END:
                  return { yAlignment: b.strings.TOP, xAlignment: n3 ? b.strings.LEFT : b.strings.RIGHT, yAxisPx: s2, xAxisPx: "0", rotation: n3 ? l2 : -1 * l2, skew: n3 ? -20 : 20, xTransformOrigin: n3 ? "0" : i4, yTransformOrigin: s2, caretCorners: n3 ? d2 : p3 };
                case b.PositionWithCaret.BOTTOM_SIDE_END:
                  return { yAlignment: b.strings.BOTTOM, xAlignment: n3 ? b.strings.LEFT : b.strings.RIGHT, yAxisPx: a2, xAxisPx: "0", rotation: n3 ? -1 * l2 : l2, skew: n3 ? 20 : -20, xTransformOrigin: n3 ? "0" : i4, yTransformOrigin: u2, caretCorners: n3 ? p3 : d2 };
                case b.PositionWithCaret.TOP_SIDE_START:
                  return { yAlignment: b.strings.TOP, xAlignment: n3 ? b.strings.RIGHT : b.strings.LEFT, yAxisPx: a2, xAxisPx: "0", rotation: n3 ? -1 * l2 : l2, skew: n3 ? 20 : -20, xTransformOrigin: n3 ? i4 : "0", yTransformOrigin: a2, caretCorners: n3 ? p3 : d2 };
                case b.PositionWithCaret.CENTER_SIDE_START:
                  return { yAlignment: b.strings.TOP, xAlignment: n3 ? b.strings.RIGHT : b.strings.LEFT, yAxisPx: s2, xAxisPx: "0", rotation: n3 ? -1 * l2 : l2, skew: n3 ? 20 : -20, xTransformOrigin: n3 ? i4 : "0", yTransformOrigin: s2, caretCorners: n3 ? p3 : d2 };
                case b.PositionWithCaret.BOTTOM_SIDE_START:
                  return { yAlignment: b.strings.BOTTOM, xAlignment: n3 ? b.strings.RIGHT : b.strings.LEFT, yAxisPx: a2, xAxisPx: "0", rotation: n3 ? l2 : -1 * l2, skew: n3 ? -20 : 20, xTransformOrigin: n3 ? i4 : "0", yTransformOrigin: u2, caretCorners: n3 ? d2 : p3 };
                case b.PositionWithCaret.ABOVE_CENTER:
                  return { yAlignment: b.strings.BOTTOM, xAlignment: b.strings.LEFT, yAxisPx: "0", xAxisPx: o2, rotation: 35, skew: 20, xTransformOrigin: o2, yTransformOrigin: r3, caretCorners: p3 };
                case b.PositionWithCaret.ABOVE_END:
                  return { yAlignment: b.strings.BOTTOM, xAlignment: n3 ? b.strings.RIGHT : b.strings.LEFT, yAxisPx: "0", xAxisPx: a2, rotation: n3 ? -35 : 35, skew: n3 ? -20 : 20, xTransformOrigin: n3 ? c2 : a2, yTransformOrigin: r3, caretCorners: n3 ? d2 : p3 };
                default:
                case b.PositionWithCaret.ABOVE_START:
                  return { yAlignment: b.strings.BOTTOM, xAlignment: n3 ? b.strings.LEFT : b.strings.RIGHT, yAxisPx: "0", xAxisPx: a2, rotation: n3 ? 35 : -35, skew: n3 ? 20 : -20, xTransformOrigin: n3 ? a2 : c2, yTransformOrigin: r3, caretCorners: n3 ? p3 : d2 };
              }
            }
          }, v.prototype.clearShowTimeout = function() {
            this.showTimeout && (clearTimeout(this.showTimeout), this.showTimeout = null);
          }, v.prototype.clearHideTimeout = function() {
            this.hideTimeout && (clearTimeout(this.hideTimeout), this.hideTimeout = null);
          }, v.prototype.attachScrollHandler = function(t2) {
            var e2 = this;
            this.addAncestorScrollEventListeners.push(function() {
              t2("scroll", e2.windowScrollHandler);
            });
          }, v.prototype.removeScrollHandler = function(t2) {
            var e2 = this;
            this.removeAncestorScrollEventListeners.push(function() {
              t2("scroll", e2.windowScrollHandler);
            });
          }, v.prototype.destroy = function() {
            var e2, t2;
            this.frameId && (cancelAnimationFrame(this.frameId), this.frameId = null), this.clearHideTimeout(), this.clearShowTimeout(), this.adapter.removeClass(d), this.adapter.removeClass(f), this.adapter.removeClass(p2), this.adapter.removeClass(y), this.adapter.removeClass(C), this.richTooltip && this.adapter.deregisterEventHandler("focusout", this.richTooltipFocusOutHandler), this.persistentTooltip || (this.adapter.deregisterEventHandler("mouseenter", this.tooltipMouseEnterHandler), this.adapter.deregisterEventHandler("mouseleave", this.tooltipMouseLeaveHandler)), this.adapter.deregisterAnchorEventHandler("blur", this.anchorBlurHandler), this.adapter.deregisterDocumentEventHandler("click", this.documentClickHandler), this.adapter.deregisterDocumentEventHandler("keydown", this.documentKeydownHandler), this.adapter.deregisterWindowEventHandler("scroll", this.windowScrollHandler), this.adapter.deregisterWindowEventHandler("resize", this.windowResizeHandler);
            try {
              for (var n3 = T(this.removeAncestorScrollEventListeners), i4 = n3.next(); !i4.done; i4 = n3.next()) (0, i4.value)();
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
            this.animFrame.cancelAll();
          }, v);
          function v(t2) {
            var e2 = g.call(this, o(o({}, v.defaultAdapter), t2)) || this;
            return e2.tooltipShown = false, e2.anchorGap = b.numbers.BOUNDED_ANCHOR_GAP, e2.xTooltipPos = b.XPosition.DETECTED, e2.yTooltipPos = b.YPosition.DETECTED, e2.tooltipPositionWithCaret = b.PositionWithCaret.DETECTED, e2.minViewportTooltipThreshold = b.numbers.MIN_VIEWPORT_TOOLTIP_THRESHOLD, e2.hideDelayMs = b.numbers.HIDE_DELAY_MS, e2.showDelayMs = b.numbers.SHOW_DELAY_MS, e2.anchorRect = null, e2.parentRect = null, e2.frameId = null, e2.hideTimeout = null, e2.showTimeout = null, e2.addAncestorScrollEventListeners = new Array(), e2.removeAncestorScrollEventListeners = new Array(), e2.animFrame = new a.AnimationFrame(), e2.anchorBlurHandler = function(t3) {
              e2.handleAnchorBlur(t3);
            }, e2.documentClickHandler = function(t3) {
              e2.handleDocumentClick(t3);
            }, e2.documentKeydownHandler = function(t3) {
              e2.handleKeydown(t3);
            }, e2.tooltipMouseEnterHandler = function() {
              e2.handleTooltipMouseEnter();
            }, e2.tooltipMouseLeaveHandler = function() {
              e2.handleTooltipMouseLeave();
            }, e2.richTooltipFocusOutHandler = function(t3) {
              e2.handleRichTooltipFocusOut(t3);
            }, e2.windowScrollHandler = function() {
              e2.handleWindowScrollEvent();
            }, e2.windowResizeHandler = function() {
              e2.handleWindowChangeEvent();
            }, e2;
          }
          e.MDCTooltipFoundation = m, e.default = m;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCFixedTopAppBarFoundation = void 0;
          var o, s = n2(9), a = n2(43), c = (o = a.MDCTopAppBarFoundation, r2(u, o), u.prototype.handleTargetScroll = function() {
            this.adapter.getViewportScrollY() <= 0 ? this.wasScrolled && (this.adapter.removeClass(s.cssClasses.FIXED_SCROLLED_CLASS), this.wasScrolled = false) : this.wasScrolled || (this.adapter.addClass(s.cssClasses.FIXED_SCROLLED_CLASS), this.wasScrolled = true);
          }, u);
          function u() {
            var t2 = null !== o && o.apply(this, arguments) || this;
            return t2.wasScrolled = false, t2;
          }
          e.MDCFixedTopAppBarFoundation = c, e.default = c;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCShortTopAppBarFoundation = void 0;
          var o, s = n2(9), a = n2(44), c = (o = a.MDCTopAppBarBaseFoundation, r2(u, o), Object.defineProperty(u.prototype, "isCollapsed", { get: function() {
            return this.collapsed;
          }, enumerable: false, configurable: true }), u.prototype.init = function() {
            o.prototype.init.call(this), 0 < this.adapter.getTotalActionItems() && this.adapter.addClass(s.cssClasses.SHORT_HAS_ACTION_ITEM_CLASS), this.setAlwaysCollapsed(this.adapter.hasClass(s.cssClasses.SHORT_COLLAPSED_CLASS));
          }, u.prototype.setAlwaysCollapsed = function(t2) {
            this.isAlwaysCollapsed = !!t2, this.isAlwaysCollapsed ? this.collapse() : this.maybeCollapseBar();
          }, u.prototype.getAlwaysCollapsed = function() {
            return this.isAlwaysCollapsed;
          }, u.prototype.handleTargetScroll = function() {
            this.maybeCollapseBar();
          }, u.prototype.maybeCollapseBar = function() {
            this.isAlwaysCollapsed || (this.adapter.getViewportScrollY() <= 0 ? this.collapsed && this.uncollapse() : this.collapsed || this.collapse());
          }, u.prototype.uncollapse = function() {
            this.adapter.removeClass(s.cssClasses.SHORT_COLLAPSED_CLASS), this.collapsed = false;
          }, u.prototype.collapse = function() {
            this.adapter.addClass(s.cssClasses.SHORT_COLLAPSED_CLASS), this.collapsed = true;
          }, u);
          function u(t2) {
            var e2 = o.call(this, t2) || this;
            return e2.collapsed = false, e2.isAlwaysCollapsed = false, e2;
          }
          e.MDCShortTopAppBarFoundation = c, e.default = c;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), o = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && i3(e2, t2, n3);
            return r2(e2, t2), e2;
          }, s = this && this.__importDefault || function(t2) {
            return t2 && t2.__esModule ? t2 : { default: t2 };
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.topAppBar = e.tooltip = e.textField = e.tabScroller = e.tabIndicator = e.tabBar = e.tab = e.switchControl = e.snackbar = e.slider = e.select = e.segmentedButton = e.ripple = e.radio = e.notchedOutline = e.menuSurface = e.menu = e.list = e.linearProgress = e.lineRipple = e.iconButton = e.formField = e.floatingLabel = e.drawer = e.dom = e.dialog = e.dataTable = e.circularProgress = e.chips = e.checkbox = e.base = e.banner = e.autoInit = void 0;
          var a = s(n2(121));
          e.autoInit = a.default;
          var c = o(n2(123));
          e.banner = c;
          var u = o(n2(126));
          e.base = u;
          var l = o(n2(128));
          e.checkbox = l;
          var d = o(n2(130));
          e.chips = d;
          var p2 = o(n2(141));
          e.circularProgress = p2;
          var h = o(n2(144));
          e.dataTable = h;
          var f = o(n2(148));
          e.dialog = f;
          var y = o(n2(152));
          e.dom = y;
          var C = o(n2(153));
          e.drawer = C;
          var E = o(n2(157));
          e.floatingLabel = E;
          var g = o(n2(159));
          e.formField = g;
          var _ = o(n2(162));
          e.iconButton = _;
          var m = o(n2(166));
          e.lineRipple = m;
          var v = o(n2(168));
          e.linearProgress = v;
          var T = o(n2(171));
          e.list = T;
          var b = o(n2(174));
          e.menuSurface = b;
          var A = o(n2(177));
          e.menu = A;
          var O = o(n2(180));
          e.notchedOutline = O;
          var I = o(n2(182));
          e.radio = I;
          var S = o(n2(185));
          e.ripple = S;
          var R = o(n2(188));
          e.segmentedButton = R;
          var D = o(n2(195));
          e.select = D;
          var L = o(n2(203));
          e.slider = L;
          var P = o(n2(206));
          e.snackbar = P;
          var M = o(n2(210));
          e.switchControl = M;
          var N = o(n2(218));
          e.tabBar = N;
          var w = o(n2(225));
          e.tabIndicator = w;
          var x = o(n2(227));
          e.tabScroller = x;
          var F = o(n2(230));
          e.tab = F;
          var j = o(n2(233));
          e.textField = j;
          var H = o(n2(243));
          e.tooltip = H;
          var B = o(n2(246));
          e.topAppBar = B, a.default.register("MDCBanner", c.MDCBanner), a.default.register("MDCCheckbox", l.MDCCheckbox), a.default.register("MDCChip", d.MDCChip), a.default.register("MDCChipSet", d.MDCChipSet), a.default.register("MDCCircularProgress", p2.MDCCircularProgress), a.default.register("MDCDataTable", h.MDCDataTable), a.default.register("MDCDialog", f.MDCDialog), a.default.register("MDCDrawer", C.MDCDrawer), a.default.register("MDCFloatingLabel", E.MDCFloatingLabel), a.default.register("MDCFormField", g.MDCFormField), a.default.register("MDCIconButtonToggle", _.MDCIconButtonToggle), a.default.register("MDCLineRipple", m.MDCLineRipple), a.default.register("MDCLinearProgress", v.MDCLinearProgress), a.default.register("MDCList", T.MDCList), a.default.register("MDCMenu", A.MDCMenu), a.default.register("MDCMenuSurface", b.MDCMenuSurface), a.default.register("MDCNotchedOutline", O.MDCNotchedOutline), a.default.register("MDCRadio", I.MDCRadio), a.default.register("MDCRipple", S.MDCRipple), a.default.register("MDCSegmentedButton", R.MDCSegmentedButton), a.default.register("MDCSelect", D.MDCSelect), a.default.register("MDCSlider", L.MDCSlider), a.default.register("MDCSnackbar", P.MDCSnackbar), a.default.register("MDCSwitch", M.MDCSwitch), a.default.register("MDCTabBar", N.MDCTabBar), a.default.register("MDCTextField", j.MDCTextField), a.default.register("MDCTooltip", H.MDCTooltip), a.default.register("MDCTopAppBar", B.MDCTopAppBar);
        }, function(t, e, n2) {
          "use strict";
          var d = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.mdcAutoInit = void 0;
          var i3 = n2(122), p2 = i3.strings.AUTO_INIT_ATTR, h = i3.strings.AUTO_INIT_STATE_ATTR, f = i3.strings.INITIALIZED_STATE, y = {}, r2 = console.warn.bind(console);
          function o(t2) {
            var e2, n3;
            void 0 === t2 && (t2 = document);
            var i4 = [], r3 = [].slice.call(t2.querySelectorAll("[" + p2 + "]"));
            r3 = r3.filter(function(t3) {
              return t3.getAttribute(h) !== f;
            });
            try {
              for (var o2 = d(r3), s = o2.next(); !s.done; s = o2.next()) {
                var a = s.value, c = a.getAttribute(p2);
                if (!c) throw new Error("(mdc-auto-init) Constructor name must be given.");
                var u = y[c];
                if ("function" != typeof u) throw new Error("(mdc-auto-init) Could not find constructor in registry for " + c);
                var l = u.attachTo(a);
                Object.defineProperty(a, c, { configurable: true, enumerable: false, value: l, writable: false }), i4.push(l), a.setAttribute(h, f);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                s && !s.done && (n3 = o2.return) && n3.call(o2);
              } finally {
                if (e2) throw e2.error;
              }
            }
            return function(t3, e3, n4) {
              var i5;
              void 0 === n4 && (n4 = false), "function" == typeof CustomEvent ? i5 = new CustomEvent(t3, { bubbles: n4, detail: e3 }) : (i5 = document.createEvent("CustomEvent")).initCustomEvent(t3, n4, false, e3), document.dispatchEvent(i5);
            }("MDCAutoInit:End", {}), i4;
          }
          (e.mdcAutoInit = o).register = function(t2, e2, n3) {
            if (void 0 === n3 && (n3 = r2), "function" != typeof e2) throw new Error("(mdc-auto-init) Invalid Constructor value: " + e2 + ". Expected function.");
            var i4 = y[t2];
            i4 && n3("(mdc-auto-init) Overriding registration for " + t2 + " with " + e2 + ". Was: " + i4), y[t2] = e2;
          }, o.deregister = function(t2) {
            delete y[t2];
          }, o.deregisterAll = function() {
            var e2, t2;
            try {
              for (var n3 = d(Object.keys(y)), i4 = n3.next(); !i4.done; i4 = n3.next()) {
                var r3 = i4.value;
                o.deregister(r3);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, e.default = o;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.strings = void 0, e.strings = { AUTO_INIT_ATTR: "data-mdc-auto-init", AUTO_INIT_STATE_ATTR: "data-mdc-auto-init-state", INITIALIZED_STATE: "initialized" };
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(124), e), r2(n2(125), e), r2(n2(18), e), r2(n2(45), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCBanner = void 0;
          var o, s = n2(1), a = n2(11), c = n2(3), u = n2(18), l = n2(45), d = (o = s.MDCComponent, r2(p2, o), p2.attachTo = function(t2) {
            return new p2(t2);
          }, p2.prototype.initialize = function(t2) {
            var n3 = this;
            void 0 === t2 && (t2 = function(t3, e2) {
              return new a.FocusTrap(t3, e2);
            }), this.contentEl = this.root.querySelector(u.selectors.CONTENT), this.textEl = this.root.querySelector(u.selectors.TEXT), this.primaryActionEl = this.root.querySelector(u.selectors.PRIMARY_ACTION), this.secondaryActionEl = this.root.querySelector(u.selectors.SECONDARY_ACTION), this.focusTrapFactory = t2, this.handleContentClick = function(t3) {
              var e2 = t3.target;
              c.closest(e2, u.selectors.PRIMARY_ACTION) ? n3.foundation.handlePrimaryActionClick() : c.closest(e2, u.selectors.SECONDARY_ACTION) && n3.foundation.handleSecondaryActionClick();
            };
          }, p2.prototype.initialSyncWithDOM = function() {
            this.registerContentClickHandler(this.handleContentClick), this.focusTrap = this.focusTrapFactory(this.root, { initialFocusEl: this.primaryActionEl });
          }, p2.prototype.destroy = function() {
            o.prototype.destroy.call(this), this.deregisterContentClickHandler(this.handleContentClick);
          }, p2.prototype.layout = function() {
            this.foundation.layout();
          }, p2.prototype.open = function() {
            this.foundation.open();
          }, p2.prototype.close = function(t2) {
            this.foundation.close(t2);
          }, p2.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              n3.root.classList.add(t3);
            }, getContentHeight: function() {
              return n3.contentEl.offsetHeight;
            }, notifyClosed: function(t3) {
              n3.emit(u.events.CLOSED, { reason: t3 });
            }, notifyClosing: function(t3) {
              n3.emit(u.events.CLOSING, { reason: t3 });
            }, notifyOpened: function() {
              n3.emit(u.events.OPENED, {});
            }, notifyOpening: function() {
              n3.emit(u.events.OPENING, {});
            }, notifyActionClicked: function(t3) {
              n3.emit(u.events.ACTION_CLICKED, { action: t3 });
            }, releaseFocus: function() {
              n3.focusTrap.releaseFocus();
            }, removeClass: function(t3) {
              n3.root.classList.remove(t3);
            }, setStyleProperty: function(t3, e2) {
              n3.root.style.setProperty(t3, e2);
            }, trapFocus: function() {
              n3.focusTrap.trapFocus();
            } };
            return new l.MDCBannerFoundation(t2);
          }, Object.defineProperty(p2.prototype, "isOpen", { get: function() {
            return this.foundation.isOpen();
          }, enumerable: false, configurable: true }), p2.prototype.getText = function() {
            return this.textEl.textContent || "";
          }, p2.prototype.setText = function(t2) {
            this.textEl.textContent = t2;
          }, p2.prototype.getPrimaryActionText = function() {
            return this.primaryActionEl.textContent || "";
          }, p2.prototype.setPrimaryActionText = function(t2) {
            this.primaryActionEl.textContent = t2;
          }, p2.prototype.getSecondaryActionText = function() {
            return this.secondaryActionEl ? this.secondaryActionEl.textContent || "" : null;
          }, p2.prototype.setSecondaryActionText = function(t2) {
            this.secondaryActionEl && (this.secondaryActionEl.textContent = t2);
          }, p2.prototype.registerContentClickHandler = function(t2) {
            this.contentEl.addEventListener("click", t2);
          }, p2.prototype.deregisterContentClickHandler = function(t2) {
            this.contentEl.removeEventListener("click", t2);
          }, p2);
          function p2() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCBanner = d;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(1), e), r2(n2(0), e), r2(n2(127), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(129), e), r2(n2(46), e), r2(n2(20), e), r2(n2(48), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(131), e), r2(n2(134), e), r2(n2(137), e);
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.trailingActionStrings = void 0, r2(n2(132), e), r2(n2(49), e), r2(n2(50), e), r2(n2(133), e);
          var o = n2(12);
          Object.defineProperty(e, "trailingActionStrings", { enumerable: true, get: function() {
            return o.strings;
          } });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.chipStrings = e.chipCssClasses = void 0, r2(n2(135), e), r2(n2(51), e), r2(n2(21), e), r2(n2(136), e);
          var o = n2(13);
          Object.defineProperty(e, "chipCssClasses", { enumerable: true, get: function() {
            return o.cssClasses;
          } }), Object.defineProperty(e, "chipStrings", { enumerable: true, get: function() {
            return o.strings;
          } });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.chipSetStrings = e.chipSetCssClasses = void 0, r2(n2(138), e), r2(n2(139), e), r2(n2(52), e);
          var o = n2(53);
          Object.defineProperty(e, "chipSetCssClasses", { enumerable: true, get: function() {
            return o.cssClasses;
          } }), Object.defineProperty(e, "chipSetStrings", { enumerable: true, get: function() {
            return o.strings;
          } });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), s = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCChipSet = void 0;
          var o, a = n2(1), c = n2(140), u = n2(51), l = n2(21), d = n2(52), p2 = l.MDCChipFoundation.strings, h = p2.INTERACTION_EVENT, f = p2.SELECTION_EVENT, y = p2.REMOVAL_EVENT, C = p2.NAVIGATION_EVENT, E = d.MDCChipSetFoundation.strings.CHIP_SELECTOR, g = 0, _ = (o = a.MDCComponent, r2(m, o), m.attachTo = function(t2) {
            return new m(t2);
          }, Object.defineProperty(m.prototype, "chips", { get: function() {
            return this.chipsList.slice();
          }, enumerable: false, configurable: true }), Object.defineProperty(m.prototype, "selectedChipIds", { get: function() {
            return this.foundation.getSelectedChipIds();
          }, enumerable: false, configurable: true }), m.prototype.initialize = function(t2) {
            void 0 === t2 && (t2 = function(t3) {
              return new u.MDCChip(t3);
            }), this.chipFactory = t2, this.chipsList = this.instantiateChips(this.chipFactory);
          }, m.prototype.initialSyncWithDOM = function() {
            var e2, t2, n3 = this;
            try {
              for (var i4 = s(this.chipsList), r3 = i4.next(); !r3.done; r3 = i4.next()) {
                var o2 = r3.value;
                o2.id && o2.selected && this.foundation.select(o2.id);
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                r3 && !r3.done && (t2 = i4.return) && t2.call(i4);
              } finally {
                if (e2) throw e2.error;
              }
            }
            this.handleChipInteraction = function(t3) {
              return n3.foundation.handleChipInteraction(t3.detail);
            }, this.handleChipSelection = function(t3) {
              return n3.foundation.handleChipSelection(t3.detail);
            }, this.handleChipRemoval = function(t3) {
              return n3.foundation.handleChipRemoval(t3.detail);
            }, this.handleChipNavigation = function(t3) {
              return n3.foundation.handleChipNavigation(t3.detail);
            }, this.listen(h, this.handleChipInteraction), this.listen(f, this.handleChipSelection), this.listen(y, this.handleChipRemoval), this.listen(C, this.handleChipNavigation);
          }, m.prototype.destroy = function() {
            var e2, t2;
            try {
              for (var n3 = s(this.chipsList), i4 = n3.next(); !i4.done; i4 = n3.next()) i4.value.destroy();
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
            this.unlisten(h, this.handleChipInteraction), this.unlisten(f, this.handleChipSelection), this.unlisten(y, this.handleChipRemoval), this.unlisten(C, this.handleChipNavigation), o.prototype.destroy.call(this);
          }, m.prototype.addChip = function(t2) {
            t2.id = t2.id || "mdc-chip-" + ++g, this.chipsList.push(this.chipFactory(t2));
          }, m.prototype.getDefaultFoundation = function() {
            var i4 = this, t2 = { announceMessage: function(t3) {
              c.announce(t3);
            }, focusChipPrimaryActionAtIndex: function(t3) {
              i4.chipsList[t3].focusPrimaryAction();
            }, focusChipTrailingActionAtIndex: function(t3) {
              i4.chipsList[t3].focusTrailingAction();
            }, getChipListCount: function() {
              return i4.chips.length;
            }, getIndexOfChipById: function(t3) {
              return i4.findChipIndex(t3);
            }, hasClass: function(t3) {
              return i4.root.classList.contains(t3);
            }, isRTL: function() {
              return "rtl" === window.getComputedStyle(i4.root).getPropertyValue("direction");
            }, removeChipAtIndex: function(t3) {
              0 <= t3 && t3 < i4.chips.length && (i4.chipsList[t3].destroy(), i4.chipsList[t3].remove(), i4.chipsList.splice(t3, 1));
            }, removeFocusFromChipAtIndex: function(t3) {
              i4.chipsList[t3].removeFocus();
            }, selectChipAtIndex: function(t3, e2, n3) {
              0 <= t3 && t3 < i4.chips.length && i4.chipsList[t3].setSelectedFromChipSet(e2, n3);
            } };
            return new d.MDCChipSetFoundation(t2);
          }, m.prototype.instantiateChips = function(e2) {
            return [].slice.call(this.root.querySelectorAll(E)).map(function(t2) {
              return t2.id = t2.id || "mdc-chip-" + ++g, e2(t2);
            });
          }, m.prototype.findChipIndex = function(t2) {
            for (var e2 = 0; e2 < this.chips.length; e2++) if (this.chipsList[e2].id === t2) return e2;
            return -1;
          }, m);
          function m() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCChipSet = _;
        }, function(t, i3, e) {
          "use strict";
          var c, n2;
          Object.defineProperty(i3, "__esModule", { value: true }), i3.announce = i3.DATA_MDC_DOM_ANNOUNCE = i3.AnnouncerPriority = void 0, (n2 = c = i3.AnnouncerPriority || (i3.AnnouncerPriority = {})).POLITE = "polite", n2.ASSERTIVE = "assertive", i3.DATA_MDC_DOM_ANNOUNCE = "data-mdc-dom-announce", i3.announce = function(t2, e2) {
            r2.getInstance().say(t2, e2);
          };
          var r2 = (o.getInstance = function() {
            return o.instance || (o.instance = new o()), o.instance;
          }, o.prototype.say = function(t2, e2) {
            var n3, i4, r3 = null !== (n3 = null == e2 ? void 0 : e2.priority) && void 0 !== n3 ? n3 : c.POLITE, o2 = null !== (i4 = null == e2 ? void 0 : e2.ownerDocument) && void 0 !== i4 ? i4 : document, s = this.getLiveRegion(r3, o2);
            function a() {
              s.textContent = "", o2.removeEventListener("click", a);
            }
            s.textContent = "", setTimeout(function() {
              s.textContent = t2, o2.addEventListener("click", a);
            }, 1);
          }, o.prototype.getLiveRegion = function(t2, e2) {
            var n3 = this.liveRegions.get(e2);
            n3 || (n3 = /* @__PURE__ */ new Map(), this.liveRegions.set(e2, n3));
            var i4 = n3.get(t2);
            if (i4 && e2.body.contains(i4)) return i4;
            var r3 = this.createLiveRegion(t2, e2);
            return n3.set(t2, r3), r3;
          }, o.prototype.createLiveRegion = function(t2, e2) {
            var n3 = e2.createElement("div");
            return n3.style.position = "absolute", n3.style.top = "-9999px", n3.style.left = "-9999px", n3.style.height = "1px", n3.style.overflow = "hidden", n3.setAttribute("aria-atomic", "true"), n3.setAttribute("aria-live", t2), n3.setAttribute(i3.DATA_MDC_DOM_ANNOUNCE, "true"), e2.body.appendChild(n3), n3;
          }, o);
          function o() {
            this.liveRegions = /* @__PURE__ */ new Map();
          }
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(142), e), r2(n2(143), e), r2(n2(55), e), r2(n2(54), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCCircularProgress = void 0;
          var o, s = n2(1), a = n2(54), c = (o = s.MDCComponent, r2(u, o), u.prototype.initialize = function() {
            this.determinateCircle = this.root.querySelector(a.MDCCircularProgressFoundation.strings.DETERMINATE_CIRCLE_SELECTOR);
          }, u.attachTo = function(t2) {
            return new u(t2);
          }, Object.defineProperty(u.prototype, "determinate", { set: function(t2) {
            this.foundation.setDeterminate(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(u.prototype, "progress", { set: function(t2) {
            this.foundation.setProgress(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(u.prototype, "isClosed", { get: function() {
            return this.foundation.isClosed();
          }, enumerable: false, configurable: true }), u.prototype.open = function() {
            this.foundation.open();
          }, u.prototype.close = function() {
            this.foundation.close();
          }, u.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              n3.root.classList.add(t3);
            }, getDeterminateCircleAttribute: function(t3) {
              return n3.determinateCircle.getAttribute(t3);
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, removeClass: function(t3) {
              n3.root.classList.remove(t3);
            }, removeAttribute: function(t3) {
              n3.root.removeAttribute(t3);
            }, setAttribute: function(t3, e2) {
              n3.root.setAttribute(t3, e2);
            }, setDeterminateCircleAttribute: function(t3, e2) {
              n3.determinateCircle.setAttribute(t3, e2);
            } };
            return new a.MDCCircularProgressFoundation(t2);
          }, u);
          function u() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCCircularProgress = c;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(145), e), r2(n2(146), e), r2(n2(59), e), r2(n2(22), e), r2(n2(147), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCDataTable = void 0;
          var s, a = n2(1), c = n2(46), u = n2(3), l = n2(56), d = n2(22), p2 = n2(59), h = (s = a.MDCComponent, r2(f, s), f.attachTo = function(t2) {
            return new f(t2);
          }, f.prototype.initialize = function(t2) {
            void 0 === t2 && (t2 = function(t3) {
              return new c.MDCCheckbox(t3);
            }), this.checkboxFactory = t2;
          }, f.prototype.initialSyncWithDOM = function() {
            var n3 = this;
            this.headerRow = this.root.querySelector("." + d.cssClasses.HEADER_ROW), this.handleHeaderRowCheckboxChange = function() {
              n3.foundation.handleHeaderRowCheckboxChange();
            }, this.headerRow.addEventListener("change", this.handleHeaderRowCheckboxChange), this.headerRowClickListener = function(t2) {
              n3.handleHeaderRowClick(t2);
            }, this.headerRow.addEventListener("click", this.headerRowClickListener), this.content = this.root.querySelector("." + d.cssClasses.CONTENT), this.handleContentClick = function(t2) {
              var e2 = u.closest(t2.target, d.selectors.ROW);
              e2 && n3.foundation.handleRowClick({ rowId: n3.getRowIdByRowElement(e2), row: e2 });
            }, this.content.addEventListener("click", this.handleContentClick), this.handleRowCheckboxChange = function(t2) {
              n3.foundation.handleRowCheckboxChange(t2);
            }, this.content.addEventListener("change", this.handleRowCheckboxChange), this.layout();
          }, f.prototype.layout = function() {
            this.foundation.layout();
          }, f.prototype.getHeaderCells = function() {
            return [].slice.call(this.root.querySelectorAll(d.selectors.HEADER_CELL));
          }, f.prototype.getRows = function() {
            return this.foundation.getRows();
          }, f.prototype.getSelectedRowIds = function() {
            return this.foundation.getSelectedRowIds();
          }, f.prototype.setSelectedRowIds = function(t2) {
            this.foundation.setSelectedRowIds(t2);
          }, f.prototype.showProgress = function() {
            this.getLinearProgress().open(), this.foundation.showProgress();
          }, f.prototype.hideProgress = function() {
            this.foundation.hideProgress(), this.getLinearProgress().close();
          }, f.prototype.destroy = function() {
            var e2, t2;
            if (this.handleHeaderRowCheckboxChange && this.headerRow.removeEventListener("change", this.handleHeaderRowCheckboxChange), this.headerRowClickListener && this.headerRow.removeEventListener("click", this.headerRowClickListener), this.handleRowCheckboxChange && this.content.removeEventListener("change", this.handleRowCheckboxChange), this.headerRowCheckbox && this.headerRowCheckbox.destroy(), this.rowCheckboxList) try {
              for (var n3 = o(this.rowCheckboxList), i4 = n3.next(); !i4.done; i4 = n3.next()) i4.value.destroy();
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
            this.handleContentClick && this.content.removeEventListener("click", this.handleContentClick);
          }, f.prototype.getDefaultFoundation = function() {
            var i4 = this, t2 = { addClass: function(t3) {
              i4.root.classList.add(t3);
            }, removeClass: function(t3) {
              i4.root.classList.remove(t3);
            }, getHeaderCellElements: function() {
              return i4.getHeaderCells();
            }, getHeaderCellCount: function() {
              return i4.getHeaderCells().length;
            }, getAttributeByHeaderCellIndex: function(t3, e2) {
              return i4.getHeaderCells()[t3].getAttribute(e2);
            }, setAttributeByHeaderCellIndex: function(t3, e2, n3) {
              i4.getHeaderCells()[t3].setAttribute(e2, n3);
            }, setClassNameByHeaderCellIndex: function(t3, e2) {
              i4.getHeaderCells()[t3].classList.add(e2);
            }, removeClassNameByHeaderCellIndex: function(t3, e2) {
              i4.getHeaderCells()[t3].classList.remove(e2);
            }, notifySortAction: function(t3) {
              i4.emit(d.events.SORTED, t3, true);
            }, getTableContainerHeight: function() {
              var t3 = i4.root.querySelector("." + d.cssClasses.TABLE_CONTAINER);
              if (!t3) throw new Error("MDCDataTable: Table container element not found.");
              return t3.getBoundingClientRect().height;
            }, getTableHeaderHeight: function() {
              var t3 = i4.root.querySelector(d.selectors.HEADER_ROW);
              if (!t3) throw new Error("MDCDataTable: Table header element not found.");
              return t3.getBoundingClientRect().height;
            }, setProgressIndicatorStyles: function(t3) {
              var e2 = i4.root.querySelector(d.selectors.PROGRESS_INDICATOR);
              if (!e2) throw new Error("MDCDataTable: Progress indicator element not found.");
              e2.style.setProperty("height", t3.height), e2.style.setProperty("top", t3.top);
            }, addClassAtRowIndex: function(t3, e2) {
              i4.getRows()[t3].classList.add(e2);
            }, getRowCount: function() {
              return i4.getRows().length;
            }, getRowElements: function() {
              return [].slice.call(i4.root.querySelectorAll(d.selectors.ROW));
            }, getRowIdAtIndex: function(t3) {
              return i4.getRows()[t3].getAttribute(d.dataAttributes.ROW_ID);
            }, getRowIndexByChildElement: function(t3) {
              return i4.getRows().indexOf(u.closest(t3, d.selectors.ROW));
            }, getSelectedRowCount: function() {
              return i4.root.querySelectorAll(d.selectors.ROW_SELECTED).length;
            }, isCheckboxAtRowIndexChecked: function(t3) {
              return i4.rowCheckboxList[t3].checked;
            }, isHeaderRowCheckboxChecked: function() {
              return i4.headerRowCheckbox.checked;
            }, isRowsSelectable: function() {
              return !!i4.root.querySelector(d.selectors.ROW_CHECKBOX) || !!i4.root.querySelector(d.selectors.HEADER_ROW_CHECKBOX);
            }, notifyRowSelectionChanged: function(t3) {
              i4.emit(d.events.ROW_SELECTION_CHANGED, { row: i4.getRowByIndex(t3.rowIndex), rowId: i4.getRowIdByIndex(t3.rowIndex), rowIndex: t3.rowIndex, selected: t3.selected }, true);
            }, notifySelectedAll: function() {
              i4.emit(d.events.SELECTED_ALL, {}, true);
            }, notifyUnselectedAll: function() {
              i4.emit(d.events.UNSELECTED_ALL, {}, true);
            }, notifyRowClick: function(t3) {
              i4.emit(d.events.ROW_CLICK, t3, true);
            }, registerHeaderRowCheckbox: function() {
              i4.headerRowCheckbox && i4.headerRowCheckbox.destroy();
              var t3 = i4.root.querySelector(d.selectors.HEADER_ROW_CHECKBOX);
              i4.headerRowCheckbox = i4.checkboxFactory(t3);
            }, registerRowCheckboxes: function() {
              i4.rowCheckboxList && i4.rowCheckboxList.forEach(function(t3) {
                t3.destroy();
              }), i4.rowCheckboxList = [], i4.getRows().forEach(function(t3) {
                var e2 = i4.checkboxFactory(t3.querySelector(d.selectors.ROW_CHECKBOX));
                i4.rowCheckboxList.push(e2);
              });
            }, removeClassAtRowIndex: function(t3, e2) {
              i4.getRows()[t3].classList.remove(e2);
            }, setAttributeAtRowIndex: function(t3, e2, n3) {
              i4.getRows()[t3].setAttribute(e2, n3);
            }, setHeaderRowCheckboxChecked: function(t3) {
              i4.headerRowCheckbox.checked = t3;
            }, setHeaderRowCheckboxIndeterminate: function(t3) {
              i4.headerRowCheckbox.indeterminate = t3;
            }, setRowCheckboxCheckedAtIndex: function(t3, e2) {
              i4.rowCheckboxList[t3].checked = e2;
            }, setSortStatusLabelByHeaderCellIndex: function(t3, e2) {
              var n3 = i4.getHeaderCells()[t3].querySelector(d.selectors.SORT_STATUS_LABEL);
              n3 && (n3.textContent = i4.getSortStatusMessageBySortValue(e2));
            } };
            return new p2.MDCDataTableFoundation(t2);
          }, f.prototype.getRowByIndex = function(t2) {
            return this.getRows()[t2];
          }, f.prototype.getRowIdByIndex = function(t2) {
            return this.getRowByIndex(t2).getAttribute(d.dataAttributes.ROW_ID);
          }, f.prototype.handleHeaderRowClick = function(t2) {
            var e2 = u.closest(t2.target, d.selectors.HEADER_CELL_WITH_SORT);
            if (e2) {
              var n3 = e2.getAttribute(d.dataAttributes.COLUMN_ID), i4 = this.getHeaderCells().indexOf(e2);
              -1 !== i4 && this.foundation.handleSortAction({ columnId: n3, columnIndex: i4, headerCell: e2 });
            }
          }, f.prototype.getSortStatusMessageBySortValue = function(t2) {
            switch (t2) {
              case d.SortValue.ASCENDING:
                return d.messages.SORTED_IN_ASCENDING;
              case d.SortValue.DESCENDING:
                return d.messages.SORTED_IN_DESCENDING;
              default:
                return "";
            }
          }, f.prototype.getLinearProgressElement = function() {
            var t2 = this.root.querySelector("." + d.cssClasses.LINEAR_PROGRESS);
            if (!t2) throw new Error("MDCDataTable: linear progress element is not found.");
            return t2;
          }, f.prototype.getLinearProgress = function() {
            if (!this.linearProgress) {
              var t2 = this.getLinearProgressElement();
              this.linearProgress = new l.MDCLinearProgress(t2);
            }
            return this.linearProgress;
          }, f.prototype.getRowIdByRowElement = function(t2) {
            return t2.getAttribute(d.dataAttributes.ROW_ID);
          }, f);
          function f() {
            return null !== s && s.apply(this, arguments) || this;
          }
          e.MDCDataTable = h;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), o = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && i3(e2, t2, n3);
            return r2(e2, t2), e2;
          }, s = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.util = void 0;
          var a = o(n2(60));
          e.util = a, s(n2(149), e), s(n2(150), e), s(n2(62), e), s(n2(61), e), s(n2(151), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), s = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), a = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && o(e2, t2, n3);
            return s(e2, t2), e2;
          }, c = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCDialog = void 0;
          var u, l = n2(1), d = n2(11), p2 = n2(3), h = n2(2), f = n2(61), y = a(n2(60)), C = f.MDCDialogFoundation.strings, E = (u = l.MDCComponent, r2(g, u), Object.defineProperty(g.prototype, "isOpen", { get: function() {
            return this.foundation.isOpen();
          }, enumerable: false, configurable: true }), Object.defineProperty(g.prototype, "escapeKeyAction", { get: function() {
            return this.foundation.getEscapeKeyAction();
          }, set: function(t2) {
            this.foundation.setEscapeKeyAction(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(g.prototype, "scrimClickAction", { get: function() {
            return this.foundation.getScrimClickAction();
          }, set: function(t2) {
            this.foundation.setScrimClickAction(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(g.prototype, "autoStackButtons", { get: function() {
            return this.foundation.getAutoStackButtons();
          }, set: function(t2) {
            this.foundation.setAutoStackButtons(t2);
          }, enumerable: false, configurable: true }), g.attachTo = function(t2) {
            return new g(t2);
          }, g.prototype.initialize = function(t2) {
            var e2, n3;
            void 0 === t2 && (t2 = function(t3, e3) {
              return new d.FocusTrap(t3, e3);
            });
            var i4 = this.root.querySelector(C.CONTAINER_SELECTOR);
            if (!i4) throw new Error("Dialog component requires a " + C.CONTAINER_SELECTOR + " container element");
            this.container = i4, this.content = this.root.querySelector(C.CONTENT_SELECTOR), this.buttons = [].slice.call(this.root.querySelectorAll(C.BUTTON_SELECTOR)), this.defaultButton = this.root.querySelector("[" + C.BUTTON_DEFAULT_ATTRIBUTE + "]"), this.focusTrapFactory = t2, this.buttonRipples = [];
            try {
              for (var r3 = c(this.buttons), o2 = r3.next(); !o2.done; o2 = r3.next()) {
                var s2 = o2.value;
                this.buttonRipples.push(new h.MDCRipple(s2));
              }
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                o2 && !o2.done && (n3 = r3.return) && n3.call(r3);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, g.prototype.initialSyncWithDOM = function() {
            var t2 = this;
            this.focusTrap = y.createFocusTrapInstance(this.container, this.focusTrapFactory, this.getInitialFocusEl() || void 0), this.handleClick = this.foundation.handleClick.bind(this.foundation), this.handleKeydown = this.foundation.handleKeydown.bind(this.foundation), this.handleDocumentKeydown = this.foundation.handleDocumentKeydown.bind(this.foundation), this.handleOpening = function() {
              document.addEventListener("keydown", t2.handleDocumentKeydown);
            }, this.handleClosing = function() {
              document.removeEventListener("keydown", t2.handleDocumentKeydown);
            }, this.listen("click", this.handleClick), this.listen("keydown", this.handleKeydown), this.listen(C.OPENING_EVENT, this.handleOpening), this.listen(C.CLOSING_EVENT, this.handleClosing);
          }, g.prototype.destroy = function() {
            this.unlisten("click", this.handleClick), this.unlisten("keydown", this.handleKeydown), this.unlisten(C.OPENING_EVENT, this.handleOpening), this.unlisten(C.CLOSING_EVENT, this.handleClosing), this.handleClosing(), this.buttonRipples.forEach(function(t2) {
              t2.destroy();
            }), u.prototype.destroy.call(this);
          }, g.prototype.layout = function() {
            this.foundation.layout();
          }, g.prototype.open = function() {
            this.foundation.open();
          }, g.prototype.close = function(t2) {
            void 0 === t2 && (t2 = ""), this.foundation.close(t2);
          }, g.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addBodyClass: function(t3) {
              return document.body.classList.add(t3);
            }, addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, areButtonsStacked: function() {
              return y.areTopsMisaligned(n3.buttons);
            }, clickDefaultButton: function() {
              n3.defaultButton && !n3.defaultButton.disabled && n3.defaultButton.click();
            }, eventTargetMatches: function(t3, e2) {
              return !!t3 && p2.matches(t3, e2);
            }, getActionFromEvent: function(t3) {
              if (!t3.target) return "";
              var e2 = p2.closest(t3.target, "[" + C.ACTION_ATTRIBUTE + "]");
              return e2 && e2.getAttribute(C.ACTION_ATTRIBUTE);
            }, getInitialFocusEl: function() {
              return n3.getInitialFocusEl();
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, isContentScrollable: function() {
              return y.isScrollable(n3.content);
            }, notifyClosed: function(t3) {
              return n3.emit(C.CLOSED_EVENT, t3 ? { action: t3 } : {});
            }, notifyClosing: function(t3) {
              return n3.emit(C.CLOSING_EVENT, t3 ? { action: t3 } : {});
            }, notifyOpened: function() {
              return n3.emit(C.OPENED_EVENT, {});
            }, notifyOpening: function() {
              return n3.emit(C.OPENING_EVENT, {});
            }, releaseFocus: function() {
              n3.focusTrap.releaseFocus();
            }, removeBodyClass: function(t3) {
              return document.body.classList.remove(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, reverseButtons: function() {
              n3.buttons.reverse(), n3.buttons.forEach(function(t3) {
                t3.parentElement.appendChild(t3);
              });
            }, trapFocus: function() {
              n3.focusTrap.trapFocus();
            }, registerContentEventHandler: function(t3, e2) {
              n3.content instanceof HTMLElement && n3.content.addEventListener(t3, e2);
            }, deregisterContentEventHandler: function(t3, e2) {
              n3.content instanceof HTMLElement && n3.content.removeEventListener(t3, e2);
            }, isScrollableContentAtTop: function() {
              return y.isScrollAtTop(n3.content);
            }, isScrollableContentAtBottom: function() {
              return y.isScrollAtBottom(n3.content);
            }, registerWindowEventHandler: function(t3, e2) {
              window.addEventListener(t3, e2);
            }, deregisterWindowEventHandler: function(t3, e2) {
              window.removeEventListener(t3, e2);
            } };
            return new f.MDCDialogFoundation(t2);
          }, g.prototype.getInitialFocusEl = function() {
            return this.root.querySelector("[" + C.INITIAL_FOCUS_ATTRIBUTE + "]");
          }, g);
          function g() {
            return null !== u && u.apply(this, arguments) || this;
          }
          e.MDCDialog = E;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), o = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && i3(e2, t2, n3);
            return r2(e2, t2), e2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.ponyfill = e.keyboard = e.focusTrap = e.events = void 0;
          var s = o(n2(5));
          e.events = s;
          var a = o(n2(11));
          e.focusTrap = a;
          var c = o(n2(6));
          e.keyboard = c;
          var u = o(n2(3));
          e.ponyfill = u;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), o = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && i3(e2, t2, n3);
            return r2(e2, t2), e2;
          }, s = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.util = void 0;
          var a = o(n2(63));
          e.util = a, s(n2(154), e), s(n2(155), e), s(n2(65), e), s(n2(26), e), s(n2(66), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), s = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), a = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && o(e2, t2, n3);
            return s(e2, t2), e2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCDrawer = void 0;
          var c, u = n2(1), l = n2(11), d = n2(24), p2 = n2(26), h = n2(66), f = a(n2(63)), y = p2.MDCDismissibleDrawerFoundation.cssClasses, C = p2.MDCDismissibleDrawerFoundation.strings, E = (c = u.MDCComponent, r2(g, c), g.attachTo = function(t2) {
            return new g(t2);
          }, Object.defineProperty(g.prototype, "open", { get: function() {
            return this.foundation.isOpen();
          }, set: function(t2) {
            t2 ? this.foundation.open() : this.foundation.close();
          }, enumerable: false, configurable: true }), Object.defineProperty(g.prototype, "list", { get: function() {
            return this.innerList;
          }, enumerable: false, configurable: true }), g.prototype.initialize = function(t2, e2) {
            void 0 === t2 && (t2 = function(t3) {
              return new l.FocusTrap(t3);
            }), void 0 === e2 && (e2 = function(t3) {
              return new d.MDCList(t3);
            });
            var n3 = this.root.querySelector(C.LIST_SELECTOR);
            n3 && (this.innerList = e2(n3), this.innerList.wrapFocus = true), this.focusTrapFactory = t2;
          }, g.prototype.initialSyncWithDOM = function() {
            var e2 = this, t2 = y.MODAL, n3 = C.SCRIM_SELECTOR;
            this.scrim = this.root.parentNode.querySelector(n3), this.scrim && this.root.classList.contains(t2) && (this.handleScrimClick = function() {
              return e2.foundation.handleScrimClick();
            }, this.scrim.addEventListener("click", this.handleScrimClick), this.focusTrap = f.createFocusTrapInstance(this.root, this.focusTrapFactory)), this.handleKeydown = function(t3) {
              e2.foundation.handleKeydown(t3);
            }, this.handleTransitionEnd = function(t3) {
              e2.foundation.handleTransitionEnd(t3);
            }, this.listen("keydown", this.handleKeydown), this.listen("transitionend", this.handleTransitionEnd);
          }, g.prototype.destroy = function() {
            this.unlisten("keydown", this.handleKeydown), this.unlisten("transitionend", this.handleTransitionEnd), this.innerList && this.innerList.destroy();
            var t2 = y.MODAL;
            this.scrim && this.handleScrimClick && this.root.classList.contains(t2) && (this.scrim.removeEventListener("click", this.handleScrimClick), this.open = false);
          }, g.prototype.getDefaultFoundation = function() {
            var e2 = this, t2 = { addClass: function(t3) {
              e2.root.classList.add(t3);
            }, removeClass: function(t3) {
              e2.root.classList.remove(t3);
            }, hasClass: function(t3) {
              return e2.root.classList.contains(t3);
            }, elementHasClass: function(t3, e3) {
              return t3.classList.contains(e3);
            }, saveFocus: function() {
              e2.previousFocus = document.activeElement;
            }, restoreFocus: function() {
              var t3 = e2.previousFocus;
              t3 && t3.focus && e2.root.contains(document.activeElement) && t3.focus();
            }, focusActiveNavigationItem: function() {
              var t3 = e2.root.querySelector(C.LIST_ITEM_ACTIVATED_SELECTOR);
              t3 && t3.focus();
            }, notifyClose: function() {
              e2.emit(C.CLOSE_EVENT, {}, true);
            }, notifyOpen: function() {
              e2.emit(C.OPEN_EVENT, {}, true);
            }, trapFocus: function() {
              e2.focusTrap.trapFocus();
            }, releaseFocus: function() {
              e2.focusTrap.releaseFocus();
            } }, n3 = y.DISMISSIBLE, i4 = y.MODAL;
            if (this.root.classList.contains(n3)) return new p2.MDCDismissibleDrawerFoundation(t2);
            if (this.root.classList.contains(i4)) return new h.MDCModalDrawerFoundation(t2);
            throw new Error("MDCDrawer: Failed to instantiate component. Supported variants are " + n3 + " and " + i4 + ".");
          }, g);
          function g() {
            return null !== c && c.apply(this, arguments) || this;
          }
          e.MDCDrawer = E;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true }), e.handleKeydown = e.clearBuffer = e.isTypingInProgress = e.matchItem = e.initSortedIndex = e.initState = void 0;
          var C = n2(6), u = n2(7), E = n2(64);
          function g(t2, e2) {
            var n3, i3 = t2.nextChar, r2 = t2.focusItemAtIndex, o = t2.sortedIndexByFirstChar, s = t2.focusedItemIndex, a = t2.skipFocus, c = t2.isItemAtIndexDisabled;
            return clearTimeout(e2.bufferClearTimeout), e2.bufferClearTimeout = setTimeout(function() {
              l(e2);
            }, u.numbers.TYPEAHEAD_BUFFER_CLEAR_TIMEOUT_MS), e2.typeaheadBuffer = e2.typeaheadBuffer + i3, -1 === (n3 = 1 === e2.typeaheadBuffer.length ? function(t3, e3, n4, i4) {
              var r3 = i4.typeaheadBuffer[0], o2 = t3.get(r3);
              if (!o2) return -1;
              if (r3 === i4.currentFirstChar && o2[i4.sortedIndexCursor].index === e3) {
                i4.sortedIndexCursor = (i4.sortedIndexCursor + 1) % o2.length;
                var s2 = o2[i4.sortedIndexCursor].index;
                if (!n4(s2)) return s2;
              }
              i4.currentFirstChar = r3;
              var a2, c2 = -1;
              for (a2 = 0; a2 < o2.length; a2++) if (!n4(o2[a2].index)) {
                c2 = a2;
                break;
              }
              for (; a2 < o2.length; a2++) if (o2[a2].index > e3 && !n4(o2[a2].index)) {
                c2 = a2;
                break;
              }
              return -1 === c2 ? -1 : (i4.sortedIndexCursor = c2, o2[i4.sortedIndexCursor].index);
            }(o, s, c, e2) : function(t3, e3, n4) {
              var i4 = n4.typeaheadBuffer[0], r3 = t3.get(i4);
              if (!r3) return -1;
              var o2 = r3[n4.sortedIndexCursor];
              if (0 === o2.text.lastIndexOf(n4.typeaheadBuffer, 0) && !e3(o2.index)) return o2.index;
              var s2 = (n4.sortedIndexCursor + 1) % r3.length, a2 = -1;
              for (; s2 !== n4.sortedIndexCursor; ) {
                var c2 = r3[s2], u2 = 0 === c2.text.lastIndexOf(n4.typeaheadBuffer, 0), l2 = !e3(c2.index);
                if (u2 && l2) {
                  a2 = s2;
                  break;
                }
                s2 = (s2 + 1) % r3.length;
              }
              return -1 === a2 ? -1 : (n4.sortedIndexCursor = a2, r3[n4.sortedIndexCursor].index);
            }(o, c, e2)) || a || r2(n3), n3;
          }
          function _(t2) {
            return 0 < t2.typeaheadBuffer.length;
          }
          function l(t2) {
            t2.typeaheadBuffer = "";
          }
          e.initState = function() {
            return { bufferClearTimeout: 0, currentFirstChar: "", sortedIndexCursor: 0, typeaheadBuffer: "" };
          }, e.initSortedIndex = function(t2, e2) {
            for (var n3 = /* @__PURE__ */ new Map(), i3 = 0; i3 < t2; i3++) {
              var r2 = e2(i3).trim();
              if (r2) {
                var o = r2[0].toLowerCase();
                n3.has(o) || n3.set(o, []), n3.get(o).push({ text: r2.toLowerCase(), index: i3 });
              }
            }
            return n3.forEach(function(t3) {
              t3.sort(function(t4, e3) {
                return t4.index - e3.index;
              });
            }), n3;
          }, e.matchItem = g, e.isTypingInProgress = _, e.clearBuffer = l, e.handleKeydown = function(t2, e2) {
            var n3 = t2.event, i3 = t2.isTargetListItem, r2 = t2.focusedItemIndex, o = t2.focusItemAtIndex, s = t2.sortedIndexByFirstChar, a = t2.isItemAtIndexDisabled, c = "ArrowLeft" === C.normalizeKey(n3), u2 = "ArrowUp" === C.normalizeKey(n3), l2 = "ArrowRight" === C.normalizeKey(n3), d = "ArrowDown" === C.normalizeKey(n3), p2 = "Home" === C.normalizeKey(n3), h = "End" === C.normalizeKey(n3), f = "Enter" === C.normalizeKey(n3), y = "Spacebar" === C.normalizeKey(n3);
            return n3.altKey || n3.ctrlKey || n3.metaKey || c || u2 || l2 || d || p2 || h || f ? -1 : y || 1 !== n3.key.length ? y ? (i3 && E.preventDefaultEvent(n3), i3 && _(e2) ? g({ focusItemAtIndex: o, focusedItemIndex: r2, nextChar: " ", sortedIndexByFirstChar: s, skipFocus: false, isItemAtIndexDisabled: a }, e2) : -1) : -1 : (E.preventDefaultEvent(n3), g({ focusItemAtIndex: o, focusedItemIndex: r2, nextChar: n3.key.toLowerCase(), sortedIndexByFirstChar: s, skipFocus: false, isItemAtIndexDisabled: a }, e2));
          };
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(158), e), r2(n2(27), e), r2(n2(67), e), r2(n2(28), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(160), e), r2(n2(161), e), r2(n2(69), e), r2(n2(68), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCFormField = void 0;
          var o, s = n2(1), a = n2(68), c = (o = s.MDCComponent, r2(u, o), u.attachTo = function(t2) {
            return new u(t2);
          }, u.prototype.labelEl = function() {
            var t2 = a.MDCFormFieldFoundation.strings.LABEL_SELECTOR;
            return this.root.querySelector(t2);
          }, u.prototype.getDefaultFoundation = function() {
            var i4 = this, t2 = { activateInputRipple: function() {
              i4.input && i4.input.ripple && i4.input.ripple.activate();
            }, deactivateInputRipple: function() {
              i4.input && i4.input.ripple && i4.input.ripple.deactivate();
            }, deregisterInteractionHandler: function(t3, e2) {
              var n3 = i4.labelEl();
              n3 && n3.removeEventListener(t3, e2);
            }, registerInteractionHandler: function(t3, e2) {
              var n3 = i4.labelEl();
              n3 && n3.addEventListener(t3, e2);
            } };
            return new a.MDCFormFieldFoundation(t2);
          }, u);
          function u() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCFormField = c;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(163), e), r2(n2(164), e), r2(n2(71), e), r2(n2(70), e), r2(n2(165), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCIconButtonToggle = void 0;
          var o, s = n2(1), a = n2(2), c = n2(70), u = c.MDCIconButtonToggleFoundation.strings, l = (o = s.MDCComponent, r2(d, o), d.attachTo = function(t2) {
            return new d(t2);
          }, d.prototype.initialSyncWithDOM = function() {
            var t2 = this;
            this.handleClick = function() {
              t2.foundation.handleClick();
            }, this.listen("click", this.handleClick);
          }, d.prototype.destroy = function() {
            this.unlisten("click", this.handleClick), this.ripple.destroy(), o.prototype.destroy.call(this);
          }, d.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, notifyChange: function(t3) {
              n3.emit(u.CHANGE_EVENT, t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, getAttr: function(t3) {
              return n3.root.getAttribute(t3);
            }, setAttr: function(t3, e2) {
              return n3.root.setAttribute(t3, e2);
            } };
            return new c.MDCIconButtonToggleFoundation(t2);
          }, Object.defineProperty(d.prototype, "ripple", { get: function() {
            return this.rippleComponent;
          }, enumerable: false, configurable: true }), Object.defineProperty(d.prototype, "on", { get: function() {
            return this.foundation.isOn();
          }, set: function(t2) {
            this.foundation.toggle(t2);
          }, enumerable: false, configurable: true }), d.prototype.createRipple = function() {
            var t2 = new a.MDCRipple(this.root);
            return t2.unbounded = true, t2;
          }, d);
          function d() {
            var t2 = null !== o && o.apply(this, arguments) || this;
            return t2.rippleComponent = t2.createRipple(), t2;
          }
          e.MDCIconButtonToggle = l;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(167), e), r2(n2(29), e), r2(n2(73), e), r2(n2(72), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(169), e), r2(n2(56), e), r2(n2(58), e), r2(n2(57), e), r2(n2(170), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(172), e), r2(n2(24), e), r2(n2(7), e), r2(n2(25), e), r2(n2(173), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(175), e), r2(n2(74), e), r2(n2(8), e), r2(n2(14), e), r2(n2(176), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.Corner = void 0;
          var o = n2(8);
          Object.defineProperty(e, "Corner", { enumerable: true, get: function() {
            return o.Corner;
          } }), r2(n2(178), e), r2(n2(75), e), r2(n2(15), e), r2(n2(76), e), r2(n2(179), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(181), e), r2(n2(30), e), r2(n2(31), e), r2(n2(77), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(183), e), r2(n2(184), e), r2(n2(79), e), r2(n2(78), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCRadio = void 0;
          var s, a = n2(1), c = n2(5), u = n2(2), l = n2(4), d = n2(78), p2 = (s = a.MDCComponent, r2(h, s), h.attachTo = function(t2) {
            return new h(t2);
          }, Object.defineProperty(h.prototype, "checked", { get: function() {
            return this.nativeControl.checked;
          }, set: function(t2) {
            this.nativeControl.checked = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(h.prototype, "disabled", { get: function() {
            return this.nativeControl.disabled;
          }, set: function(t2) {
            this.foundation.setDisabled(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(h.prototype, "value", { get: function() {
            return this.nativeControl.value;
          }, set: function(t2) {
            this.nativeControl.value = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(h.prototype, "ripple", { get: function() {
            return this.rippleSurface;
          }, enumerable: false, configurable: true }), h.prototype.destroy = function() {
            this.rippleSurface.destroy(), s.prototype.destroy.call(this);
          }, h.prototype.getDefaultFoundation = function() {
            var e2 = this, t2 = { addClass: function(t3) {
              return e2.root.classList.add(t3);
            }, removeClass: function(t3) {
              return e2.root.classList.remove(t3);
            }, setNativeControlDisabled: function(t3) {
              return e2.nativeControl.disabled = t3;
            } };
            return new d.MDCRadioFoundation(t2);
          }, h.prototype.createRipple = function() {
            var n3 = this, t2 = o(o({}, u.MDCRipple.createAdapter(this)), { registerInteractionHandler: function(t3, e2) {
              n3.nativeControl.addEventListener(t3, e2, c.applyPassive());
            }, deregisterInteractionHandler: function(t3, e2) {
              n3.nativeControl.removeEventListener(t3, e2, c.applyPassive());
            }, isSurfaceActive: function() {
              return false;
            }, isUnbounded: function() {
              return true;
            } });
            return new u.MDCRipple(this.root, new l.MDCRippleFoundation(t2));
          }, Object.defineProperty(h.prototype, "nativeControl", { get: function() {
            var t2 = d.MDCRadioFoundation.strings.NATIVE_CONTROL_SELECTOR, e2 = this.root.querySelector(t2);
            if (!e2) throw new Error("Radio component requires a " + t2 + " element");
            return e2;
          }, enumerable: false, configurable: true }), h);
          function h() {
            var t2 = null !== s && s.apply(this, arguments) || this;
            return t2.rippleSurface = t2.createRipple(), t2;
          }
          e.MDCRadio = p2;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), o = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && i3(e2, t2, n3);
            return r2(e2, t2), e2;
          }, s = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.util = void 0;
          var a = o(n2(19));
          e.util = a, s(n2(186), e), s(n2(2), e), s(n2(47), e), s(n2(4), e), s(n2(187), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(189), e), r2(n2(192), e), r2(n2(194), e);
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(190), e), r2(n2(80), e), r2(n2(191), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSegmentedButton = void 0;
          var s, a = n2(1), c = n2(82), u = n2(81), l = n2(80), d = (s = a.MDCComponent, r2(p2, s), p2.attachTo = function(t2) {
            return new p2(t2);
          }, Object.defineProperty(p2.prototype, "segments", { get: function() {
            return this.segmentsList.slice();
          }, enumerable: false, configurable: true }), p2.prototype.initialize = function(t2) {
            void 0 === t2 && (t2 = function(t3) {
              return new c.MDCSegmentedButtonSegment(t3);
            }), this.segmentFactory = t2, this.segmentsList = this.instantiateSegments(this.segmentFactory);
          }, p2.prototype.instantiateSegments = function(e2) {
            return [].slice.call(this.root.querySelectorAll(u.selectors.SEGMENT)).map(function(t2) {
              return e2(t2);
            });
          }, p2.prototype.initialSyncWithDOM = function() {
            var e2 = this;
            this.handleSelected = function(t3) {
              e2.foundation.handleSelected(t3.detail);
            }, this.listen(u.events.SELECTED, this.handleSelected);
            for (var t2 = this.foundation.isSingleSelect(), n3 = 0; n3 < this.segmentsList.length; n3++) {
              var i4 = this.segmentsList[n3];
              i4.setIndex(n3), i4.setIsSingleSelect(t2);
            }
            var r3 = this.segmentsList.filter(function(t3) {
              return t3.isSelected();
            });
            if (t2 && 0 === r3.length && 0 < this.segmentsList.length) throw new Error("No segment selected in singleSelect mdc-segmented-button");
            if (t2 && 1 < r3.length) throw new Error("Multiple segments selected in singleSelect mdc-segmented-button");
          }, p2.prototype.destroy = function() {
            var e2, t2;
            try {
              for (var n3 = o(this.segmentsList), i4 = n3.next(); !i4.done; i4 = n3.next()) i4.value.destroy();
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
            this.unlisten(u.events.SELECTED, this.handleSelected), s.prototype.destroy.call(this);
          }, p2.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, getSegments: function() {
              return n3.mappedSegments();
            }, selectSegment: function(e2) {
              var t3 = n3.mappedSegments().find(function(t4) {
                return t4.index === e2 || t4.segmentId === e2;
              });
              t3 && n3.segmentsList[t3.index].setSelected();
            }, unselectSegment: function(e2) {
              var t3 = n3.mappedSegments().find(function(t4) {
                return t4.index === e2 || t4.segmentId === e2;
              });
              t3 && n3.segmentsList[t3.index].setUnselected();
            }, notifySelectedChange: function(t3) {
              n3.emit(u.events.CHANGE, t3, true);
            } };
            return new l.MDCSegmentedButtonFoundation(t2);
          }, p2.prototype.getSelectedSegments = function() {
            return this.foundation.getSelectedSegments();
          }, p2.prototype.selectSegment = function(t2) {
            this.foundation.selectSegment(t2);
          }, p2.prototype.unselectSegment = function(t2) {
            this.foundation.unselectSegment(t2);
          }, p2.prototype.isSegmentSelected = function(t2) {
            return this.foundation.isSegmentSelected(t2);
          }, p2.prototype.mappedSegments = function() {
            return this.segmentsList.map(function(t2, e2) {
              return { index: e2, selected: t2.isSelected(), segmentId: t2.getSegmentId() };
            });
          }, p2);
          function p2() {
            return null !== s && s.apply(this, arguments) || this;
          }
          e.MDCSegmentedButton = d;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(193), e), r2(n2(84), e), r2(n2(82), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(196), e), r2(n2(197), e), r2(n2(32), e), r2(n2(85), e), r2(n2(198), e), r2(n2(199), e), r2(n2(201), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), a = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), c = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && s(e2, t2, n3);
            return a(e2, t2), e2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSelect = void 0;
          var u, l = n2(1), d = n2(27), p2 = n2(29), h = c(n2(8)), f = n2(75), y = c(n2(15)), C = n2(30), E = n2(2), g = n2(4), _ = n2(32), m = n2(85), v = n2(86), T = n2(89), b = (u = l.MDCComponent, r2(A, u), A.attachTo = function(t2) {
            return new A(t2);
          }, A.prototype.initialize = function(t2, e2, n3, i4, r3, o2) {
            if (void 0 === t2 && (t2 = function(t3) {
              return new d.MDCFloatingLabel(t3);
            }), void 0 === e2 && (e2 = function(t3) {
              return new p2.MDCLineRipple(t3);
            }), void 0 === n3 && (n3 = function(t3) {
              return new C.MDCNotchedOutline(t3);
            }), void 0 === i4 && (i4 = function(t3) {
              return new f.MDCMenu(t3);
            }), void 0 === r3 && (r3 = function(t3) {
              return new T.MDCSelectIcon(t3);
            }), void 0 === o2 && (o2 = function(t3) {
              return new v.MDCSelectHelperText(t3);
            }), this.selectAnchor = this.root.querySelector(_.strings.SELECT_ANCHOR_SELECTOR), this.selectedText = this.root.querySelector(_.strings.SELECTED_TEXT_SELECTOR), this.hiddenInput = this.root.querySelector(_.strings.HIDDEN_INPUT_SELECTOR), !this.selectedText) throw new Error("MDCSelect: Missing required element: The following selector must be present: '" + _.strings.SELECTED_TEXT_SELECTOR + "'");
            if (this.selectAnchor.hasAttribute(_.strings.ARIA_CONTROLS)) {
              var s2 = document.getElementById(this.selectAnchor.getAttribute(_.strings.ARIA_CONTROLS));
              s2 && (this.helperText = o2(s2));
            }
            this.menuSetup(i4);
            var a2 = this.root.querySelector(_.strings.LABEL_SELECTOR);
            this.label = a2 ? t2(a2) : null;
            var c2 = this.root.querySelector(_.strings.LINE_RIPPLE_SELECTOR);
            this.lineRipple = c2 ? e2(c2) : null;
            var u2 = this.root.querySelector(_.strings.OUTLINE_SELECTOR);
            this.outline = u2 ? n3(u2) : null;
            var l2 = this.root.querySelector(_.strings.LEADING_ICON_SELECTOR);
            l2 && (this.leadingIcon = r3(l2)), this.root.classList.contains(_.cssClasses.OUTLINED) || (this.ripple = this.createRipple());
          }, A.prototype.initialSyncWithDOM = function() {
            var e2 = this;
            if (this.handleFocus = function() {
              e2.foundation.handleFocus();
            }, this.handleBlur = function() {
              e2.foundation.handleBlur();
            }, this.handleClick = function(t2) {
              e2.selectAnchor.focus(), e2.foundation.handleClick(e2.getNormalizedXCoordinate(t2));
            }, this.handleKeydown = function(t2) {
              e2.foundation.handleKeydown(t2);
            }, this.handleMenuItemAction = function(t2) {
              e2.foundation.handleMenuItemAction(t2.detail.index);
            }, this.handleMenuOpened = function() {
              e2.foundation.handleMenuOpened();
            }, this.handleMenuClosed = function() {
              e2.foundation.handleMenuClosed();
            }, this.handleMenuClosing = function() {
              e2.foundation.handleMenuClosing();
            }, this.selectAnchor.addEventListener("focus", this.handleFocus), this.selectAnchor.addEventListener("blur", this.handleBlur), this.selectAnchor.addEventListener("click", this.handleClick), this.selectAnchor.addEventListener("keydown", this.handleKeydown), this.menu.listen(h.strings.CLOSED_EVENT, this.handleMenuClosed), this.menu.listen(h.strings.CLOSING_EVENT, this.handleMenuClosing), this.menu.listen(h.strings.OPENED_EVENT, this.handleMenuOpened), this.menu.listen(y.strings.SELECTED_EVENT, this.handleMenuItemAction), this.hiddenInput) {
              if (this.hiddenInput.value) return this.foundation.setValue(this.hiddenInput.value, true), void this.foundation.layout();
              this.hiddenInput.value = this.value;
            }
          }, A.prototype.destroy = function() {
            this.selectAnchor.removeEventListener("focus", this.handleFocus), this.selectAnchor.removeEventListener("blur", this.handleBlur), this.selectAnchor.removeEventListener("keydown", this.handleKeydown), this.selectAnchor.removeEventListener("click", this.handleClick), this.menu.unlisten(h.strings.CLOSED_EVENT, this.handleMenuClosed), this.menu.unlisten(h.strings.OPENED_EVENT, this.handleMenuOpened), this.menu.unlisten(y.strings.SELECTED_EVENT, this.handleMenuItemAction), this.menu.destroy(), this.ripple && this.ripple.destroy(), this.outline && this.outline.destroy(), this.leadingIcon && this.leadingIcon.destroy(), this.helperText && this.helperText.destroy(), u.prototype.destroy.call(this);
          }, Object.defineProperty(A.prototype, "value", { get: function() {
            return this.foundation.getValue();
          }, set: function(t2) {
            this.foundation.setValue(t2);
          }, enumerable: false, configurable: true }), A.prototype.setValue = function(t2, e2) {
            void 0 === e2 && (e2 = false), this.foundation.setValue(t2, e2);
          }, Object.defineProperty(A.prototype, "selectedIndex", { get: function() {
            return this.foundation.getSelectedIndex();
          }, set: function(t2) {
            this.foundation.setSelectedIndex(t2, true);
          }, enumerable: false, configurable: true }), A.prototype.setSelectedIndex = function(t2, e2) {
            void 0 === e2 && (e2 = false), this.foundation.setSelectedIndex(t2, true, e2);
          }, Object.defineProperty(A.prototype, "disabled", { get: function() {
            return this.foundation.getDisabled();
          }, set: function(t2) {
            this.foundation.setDisabled(t2), this.hiddenInput && (this.hiddenInput.disabled = t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(A.prototype, "leadingIconAriaLabel", { set: function(t2) {
            this.foundation.setLeadingIconAriaLabel(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(A.prototype, "leadingIconContent", { set: function(t2) {
            this.foundation.setLeadingIconContent(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(A.prototype, "helperTextContent", { set: function(t2) {
            this.foundation.setHelperTextContent(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(A.prototype, "useDefaultValidation", { set: function(t2) {
            this.foundation.setUseDefaultValidation(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(A.prototype, "valid", { get: function() {
            return this.foundation.isValid();
          }, set: function(t2) {
            this.foundation.setValid(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(A.prototype, "required", { get: function() {
            return this.foundation.getRequired();
          }, set: function(t2) {
            this.foundation.setRequired(t2);
          }, enumerable: false, configurable: true }), A.prototype.layout = function() {
            this.foundation.layout();
          }, A.prototype.layoutOptions = function() {
            this.foundation.layoutOptions(), this.menu.layout(), this.menuItemValues = this.menu.items.map(function(t2) {
              return t2.getAttribute(_.strings.VALUE_ATTR) || "";
            }), this.hiddenInput && (this.hiddenInput.value = this.value);
          }, A.prototype.getDefaultFoundation = function() {
            var t2 = o(o(o(o({}, this.getSelectAdapterMethods()), this.getCommonAdapterMethods()), this.getOutlineAdapterMethods()), this.getLabelAdapterMethods());
            return new m.MDCSelectFoundation(t2, this.getFoundationMap());
          }, A.prototype.menuSetup = function(t2) {
            this.menuElement = this.root.querySelector(_.strings.MENU_SELECTOR), this.menu = t2(this.menuElement), this.menu.hasTypeahead = true, this.menu.singleSelection = true, this.menuItemValues = this.menu.items.map(function(t3) {
              return t3.getAttribute(_.strings.VALUE_ATTR) || "";
            });
          }, A.prototype.createRipple = function() {
            var n3 = this, t2 = o(o({}, E.MDCRipple.createAdapter({ root: this.selectAnchor })), { registerInteractionHandler: function(t3, e2) {
              n3.selectAnchor.addEventListener(t3, e2);
            }, deregisterInteractionHandler: function(t3, e2) {
              n3.selectAnchor.removeEventListener(t3, e2);
            } });
            return new E.MDCRipple(this.selectAnchor, new g.MDCRippleFoundation(t2));
          }, A.prototype.getSelectAdapterMethods = function() {
            var n3 = this;
            return { getMenuItemAttr: function(t2, e2) {
              return t2.getAttribute(e2);
            }, setSelectedText: function(t2) {
              n3.selectedText.textContent = t2;
            }, isSelectAnchorFocused: function() {
              return document.activeElement === n3.selectAnchor;
            }, getSelectAnchorAttr: function(t2) {
              return n3.selectAnchor.getAttribute(t2);
            }, setSelectAnchorAttr: function(t2, e2) {
              n3.selectAnchor.setAttribute(t2, e2);
            }, removeSelectAnchorAttr: function(t2) {
              n3.selectAnchor.removeAttribute(t2);
            }, addMenuClass: function(t2) {
              n3.menuElement.classList.add(t2);
            }, removeMenuClass: function(t2) {
              n3.menuElement.classList.remove(t2);
            }, openMenu: function() {
              n3.menu.open = true;
            }, closeMenu: function() {
              n3.menu.open = false;
            }, getAnchorElement: function() {
              return n3.root.querySelector(_.strings.SELECT_ANCHOR_SELECTOR);
            }, setMenuAnchorElement: function(t2) {
              n3.menu.setAnchorElement(t2);
            }, setMenuAnchorCorner: function(t2) {
              n3.menu.setAnchorCorner(t2);
            }, setMenuWrapFocus: function(t2) {
              n3.menu.wrapFocus = t2;
            }, getSelectedIndex: function() {
              var t2 = n3.menu.selectedIndex;
              return t2 instanceof Array ? t2[0] : t2;
            }, setSelectedIndex: function(t2) {
              n3.menu.selectedIndex = t2;
            }, focusMenuItemAtIndex: function(t2) {
              n3.menu.items[t2].focus();
            }, getMenuItemCount: function() {
              return n3.menu.items.length;
            }, getMenuItemValues: function() {
              return n3.menuItemValues;
            }, getMenuItemTextAtIndex: function(t2) {
              return n3.menu.getPrimaryTextAtIndex(t2);
            }, isTypeaheadInProgress: function() {
              return n3.menu.typeaheadInProgress;
            }, typeaheadMatchItem: function(t2, e2) {
              return n3.menu.typeaheadMatchItem(t2, e2);
            } };
          }, A.prototype.getCommonAdapterMethods = function() {
            var n3 = this;
            return { addClass: function(t2) {
              n3.root.classList.add(t2);
            }, removeClass: function(t2) {
              n3.root.classList.remove(t2);
            }, hasClass: function(t2) {
              return n3.root.classList.contains(t2);
            }, setRippleCenter: function(t2) {
              n3.lineRipple && n3.lineRipple.setRippleCenter(t2);
            }, activateBottomLine: function() {
              n3.lineRipple && n3.lineRipple.activate();
            }, deactivateBottomLine: function() {
              n3.lineRipple && n3.lineRipple.deactivate();
            }, notifyChange: function(t2) {
              n3.hiddenInput && (n3.hiddenInput.value = t2);
              var e2 = n3.selectedIndex;
              n3.emit(_.strings.CHANGE_EVENT, { value: t2, index: e2 }, true);
            } };
          }, A.prototype.getOutlineAdapterMethods = function() {
            var e2 = this;
            return { hasOutline: function() {
              return Boolean(e2.outline);
            }, notchOutline: function(t2) {
              e2.outline && e2.outline.notch(t2);
            }, closeOutline: function() {
              e2.outline && e2.outline.closeNotch();
            } };
          }, A.prototype.getLabelAdapterMethods = function() {
            var e2 = this;
            return { hasLabel: function() {
              return !!e2.label;
            }, floatLabel: function(t2) {
              e2.label && e2.label.float(t2);
            }, getLabelWidth: function() {
              return e2.label ? e2.label.getWidth() : 0;
            }, setLabelRequired: function(t2) {
              e2.label && e2.label.setRequired(t2);
            } };
          }, A.prototype.getNormalizedXCoordinate = function(t2) {
            var e2 = t2.target.getBoundingClientRect();
            return (this.isTouchEvent(t2) ? t2.touches[0].clientX : t2.clientX) - e2.left;
          }, A.prototype.isTouchEvent = function(t2) {
            return Boolean(t2.touches);
          }, A.prototype.getFoundationMap = function() {
            return { helperText: this.helperText ? this.helperText.foundationForSelect : void 0, leadingIcon: this.leadingIcon ? this.leadingIcon.foundationForSelect : void 0 };
          }, A);
          function A() {
            return null !== u && u.apply(this, arguments) || this;
          }
          e.MDCSelect = b;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.helperTextStrings = e.helperTextCssClasses = void 0, r2(n2(200), e), r2(n2(86), e), r2(n2(87), e);
          var o = n2(88);
          Object.defineProperty(e, "helperTextCssClasses", { enumerable: true, get: function() {
            return o.cssClasses;
          } }), Object.defineProperty(e, "helperTextStrings", { enumerable: true, get: function() {
            return o.strings;
          } });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.iconStrings = void 0, r2(n2(202), e), r2(n2(89), e), r2(n2(90), e);
          var o = n2(91);
          Object.defineProperty(e, "iconStrings", { enumerable: true, get: function() {
            return o.strings;
          } });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(204), e), r2(n2(205), e), r2(n2(33), e), r2(n2(92), e), r2(n2(34), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), c = this && this.__assign || function() {
            return (c = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSlider = void 0;
          var o, s = n2(1), u = n2(5), l = n2(3), d = n2(2), p2 = n2(4), h = n2(33), a = n2(92), f = n2(34), y = (o = s.MDCComponent, r2(C, o), C.attachTo = function(t2, e2) {
            return void 0 === e2 && (e2 = {}), new C(t2, void 0, e2);
          }, C.prototype.getDefaultFoundation = function() {
            var i4 = this, t2 = { hasClass: function(t3) {
              return i4.root.classList.contains(t3);
            }, addClass: function(t3) {
              i4.root.classList.add(t3);
            }, removeClass: function(t3) {
              i4.root.classList.remove(t3);
            }, addThumbClass: function(t3, e2) {
              i4.getThumbEl(e2).classList.add(t3);
            }, removeThumbClass: function(t3, e2) {
              i4.getThumbEl(e2).classList.remove(t3);
            }, getAttribute: function(t3) {
              return i4.root.getAttribute(t3);
            }, getInputValue: function(t3) {
              return i4.getInput(t3).value;
            }, setInputValue: function(t3, e2) {
              i4.getInput(e2).value = t3;
            }, getInputAttribute: function(t3, e2) {
              return i4.getInput(e2).getAttribute(t3);
            }, setInputAttribute: function(t3, e2, n3) {
              i4.getInput(n3).setAttribute(t3, e2);
            }, removeInputAttribute: function(t3, e2) {
              i4.getInput(e2).removeAttribute(t3);
            }, focusInput: function(t3) {
              i4.getInput(t3).focus();
            }, isInputFocused: function(t3) {
              return i4.getInput(t3) === document.activeElement;
            }, shouldHideFocusStylesForPointerEvents: function() {
              return false;
            }, getThumbKnobWidth: function(t3) {
              return i4.getThumbEl(t3).querySelector("." + h.cssClasses.THUMB_KNOB).getBoundingClientRect().width;
            }, getThumbBoundingClientRect: function(t3) {
              return i4.getThumbEl(t3).getBoundingClientRect();
            }, getBoundingClientRect: function() {
              return i4.root.getBoundingClientRect();
            }, getValueIndicatorContainerWidth: function(t3) {
              return i4.getThumbEl(t3).querySelector("." + h.cssClasses.VALUE_INDICATOR_CONTAINER).getBoundingClientRect().width;
            }, isRTL: function() {
              return "rtl" === getComputedStyle(i4.root).direction;
            }, setThumbStyleProperty: function(t3, e2, n3) {
              i4.getThumbEl(n3).style.setProperty(t3, e2);
            }, removeThumbStyleProperty: function(t3, e2) {
              i4.getThumbEl(e2).style.removeProperty(t3);
            }, setTrackActiveStyleProperty: function(t3, e2) {
              i4.trackActive.style.setProperty(t3, e2);
            }, removeTrackActiveStyleProperty: function(t3) {
              i4.trackActive.style.removeProperty(t3);
            }, setValueIndicatorText: function(t3, e2) {
              i4.getThumbEl(e2).querySelector("." + h.cssClasses.VALUE_INDICATOR_TEXT).textContent = String(t3);
            }, getValueToAriaValueTextFn: function() {
              return i4.valueToAriaValueTextFn;
            }, updateTickMarks: function(t3) {
              var e2 = i4.root.querySelector("." + h.cssClasses.TICK_MARKS_CONTAINER);
              if (e2 || ((e2 = document.createElement("div")).classList.add(h.cssClasses.TICK_MARKS_CONTAINER), i4.root.querySelector("." + h.cssClasses.TRACK).appendChild(e2)), t3.length !== e2.children.length) {
                for (; e2.firstChild; ) e2.removeChild(e2.firstChild);
                i4.addTickMarks(e2, t3);
              } else i4.updateTickMarks(e2, t3);
            }, setPointerCapture: function(t3) {
              i4.root.setPointerCapture(t3);
            }, emitChangeEvent: function(t3, e2) {
              i4.emit(h.events.CHANGE, { value: t3, thumb: e2 });
            }, emitInputEvent: function(t3, e2) {
              i4.emit(h.events.INPUT, { value: t3, thumb: e2 });
            }, emitDragStartEvent: function(t3, e2) {
              i4.getRipple(e2).activate();
            }, emitDragEndEvent: function(t3, e2) {
              i4.getRipple(e2).deactivate();
            }, registerEventHandler: function(t3, e2) {
              i4.listen(t3, e2);
            }, deregisterEventHandler: function(t3, e2) {
              i4.unlisten(t3, e2);
            }, registerThumbEventHandler: function(t3, e2, n3) {
              i4.getThumbEl(t3).addEventListener(e2, n3);
            }, deregisterThumbEventHandler: function(t3, e2, n3) {
              i4.getThumbEl(t3).removeEventListener(e2, n3);
            }, registerInputEventHandler: function(t3, e2, n3) {
              i4.getInput(t3).addEventListener(e2, n3);
            }, deregisterInputEventHandler: function(t3, e2, n3) {
              i4.getInput(t3).removeEventListener(e2, n3);
            }, registerBodyEventHandler: function(t3, e2) {
              document.body.addEventListener(t3, e2);
            }, deregisterBodyEventHandler: function(t3, e2) {
              document.body.removeEventListener(t3, e2);
            }, registerWindowEventHandler: function(t3, e2) {
              window.addEventListener(t3, e2);
            }, deregisterWindowEventHandler: function(t3, e2) {
              window.removeEventListener(t3, e2);
            } };
            return new a.MDCSliderFoundation(t2);
          }, C.prototype.initialize = function(t2) {
            var e2 = (void 0 === t2 ? {} : t2).skipInitialUIUpdate;
            this.inputs = [].slice.call(this.root.querySelectorAll("." + h.cssClasses.INPUT)), this.thumbs = [].slice.call(this.root.querySelectorAll("." + h.cssClasses.THUMB)), this.trackActive = this.root.querySelector("." + h.cssClasses.TRACK_ACTIVE), this.ripples = this.createRipples(), e2 && (this.skipInitialUIUpdate = true);
          }, C.prototype.initialSyncWithDOM = function() {
            this.foundation.layout({ skipUpdateUI: this.skipInitialUIUpdate });
          }, C.prototype.layout = function() {
            this.foundation.layout();
          }, C.prototype.getValueStart = function() {
            return this.foundation.getValueStart();
          }, C.prototype.setValueStart = function(t2) {
            this.foundation.setValueStart(t2);
          }, C.prototype.getValue = function() {
            return this.foundation.getValue();
          }, C.prototype.setValue = function(t2) {
            this.foundation.setValue(t2);
          }, C.prototype.getDisabled = function() {
            return this.foundation.getDisabled();
          }, C.prototype.setDisabled = function(t2) {
            this.foundation.setDisabled(t2);
          }, C.prototype.setValueToAriaValueTextFn = function(t2) {
            this.valueToAriaValueTextFn = t2;
          }, C.prototype.getThumbEl = function(t2) {
            return t2 === f.Thumb.END ? this.thumbs[this.thumbs.length - 1] : this.thumbs[0];
          }, C.prototype.getInput = function(t2) {
            return t2 === f.Thumb.END ? this.inputs[this.inputs.length - 1] : this.inputs[0];
          }, C.prototype.getRipple = function(t2) {
            return t2 === f.Thumb.END ? this.ripples[this.ripples.length - 1] : this.ripples[0];
          }, C.prototype.addTickMarks = function(t2, e2) {
            for (var n3 = document.createDocumentFragment(), i4 = 0; i4 < e2.length; i4++) {
              var r3 = document.createElement("div"), o2 = e2[i4] === f.TickMark.ACTIVE ? h.cssClasses.TICK_MARK_ACTIVE : h.cssClasses.TICK_MARK_INACTIVE;
              r3.classList.add(o2), n3.appendChild(r3);
            }
            t2.appendChild(n3);
          }, C.prototype.updateTickMarks = function(t2, e2) {
            for (var n3 = Array.from(t2.children), i4 = 0; i4 < n3.length; i4++) e2[i4] === f.TickMark.ACTIVE ? (n3[i4].classList.add(h.cssClasses.TICK_MARK_ACTIVE), n3[i4].classList.remove(h.cssClasses.TICK_MARK_INACTIVE)) : (n3[i4].classList.add(h.cssClasses.TICK_MARK_INACTIVE), n3[i4].classList.remove(h.cssClasses.TICK_MARK_ACTIVE));
          }, C.prototype.createRipples = function() {
            for (var o2 = [], s2 = [].slice.call(this.root.querySelectorAll("." + h.cssClasses.THUMB)), t2 = function(t3) {
              var n3 = s2[t3], i4 = a2.inputs[t3], e3 = c(c({}, d.MDCRipple.createAdapter(a2)), { addClass: function(t4) {
                n3.classList.add(t4);
              }, computeBoundingRect: function() {
                return n3.getBoundingClientRect();
              }, deregisterInteractionHandler: function(t4, e4) {
                i4.removeEventListener(t4, e4);
              }, isSurfaceActive: function() {
                return l.matches(i4, ":active");
              }, isUnbounded: function() {
                return true;
              }, registerInteractionHandler: function(t4, e4) {
                i4.addEventListener(t4, e4, u.applyPassive());
              }, removeClass: function(t4) {
                n3.classList.remove(t4);
              }, updateCssVariable: function(t4, e4) {
                n3.style.setProperty(t4, e4);
              } }), r3 = new d.MDCRipple(n3, new p2.MDCRippleFoundation(e3));
              r3.unbounded = true, o2.push(r3);
            }, a2 = this, e2 = 0; e2 < s2.length; e2++) t2(e2);
            return o2;
          }, C);
          function C() {
            var t2 = null !== o && o.apply(this, arguments) || this;
            return t2.skipInitialUIUpdate = false, t2.valueToAriaValueTextFn = null, t2;
          }
          e.MDCSlider = y;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), o = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && i3(e2, t2, n3);
            return r2(e2, t2), e2;
          }, s = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.util = void 0;
          var a = o(n2(93));
          e.util = a, s(n2(207), e), s(n2(208), e), s(n2(16), e), s(n2(94), e), s(n2(209), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), s = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), a = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && o(e2, t2, n3);
            return s(e2, t2), e2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSnackbar = void 0;
          var c, u = n2(1), l = n2(3), d = n2(16), p2 = n2(94), h = a(n2(93)), f = d.strings.SURFACE_SELECTOR, y = d.strings.LABEL_SELECTOR, C = d.strings.ACTION_SELECTOR, E = d.strings.DISMISS_SELECTOR, g = d.strings.OPENING_EVENT, _ = d.strings.OPENED_EVENT, m = d.strings.CLOSING_EVENT, v = d.strings.CLOSED_EVENT, T = (c = u.MDCComponent, r2(b, c), b.attachTo = function(t2) {
            return new b(t2);
          }, b.prototype.initialize = function(t2) {
            void 0 === t2 && (t2 = function() {
              return h.announce;
            }), this.announce = t2();
          }, b.prototype.initialSyncWithDOM = function() {
            var n3 = this;
            this.surfaceEl = this.root.querySelector(f), this.labelEl = this.root.querySelector(y), this.actionEl = this.root.querySelector(C), this.handleKeyDown = function(t2) {
              n3.foundation.handleKeyDown(t2);
            }, this.handleSurfaceClick = function(t2) {
              var e2 = t2.target;
              n3.isActionButton(e2) ? n3.foundation.handleActionButtonClick(t2) : n3.isActionIcon(e2) && n3.foundation.handleActionIconClick(t2);
            }, this.registerKeyDownHandler(this.handleKeyDown), this.registerSurfaceClickHandler(this.handleSurfaceClick);
          }, b.prototype.destroy = function() {
            c.prototype.destroy.call(this), this.deregisterKeyDownHandler(this.handleKeyDown), this.deregisterSurfaceClickHandler(this.handleSurfaceClick);
          }, b.prototype.open = function() {
            this.foundation.open();
          }, b.prototype.close = function(t2) {
            void 0 === t2 && (t2 = ""), this.foundation.close(t2);
          }, b.prototype.getDefaultFoundation = function() {
            var e2 = this, t2 = { addClass: function(t3) {
              e2.root.classList.add(t3);
            }, announce: function() {
              e2.announce(e2.labelEl);
            }, notifyClosed: function(t3) {
              return e2.emit(v, t3 ? { reason: t3 } : {});
            }, notifyClosing: function(t3) {
              return e2.emit(m, t3 ? { reason: t3 } : {});
            }, notifyOpened: function() {
              return e2.emit(_, {});
            }, notifyOpening: function() {
              return e2.emit(g, {});
            }, removeClass: function(t3) {
              return e2.root.classList.remove(t3);
            } };
            return new p2.MDCSnackbarFoundation(t2);
          }, Object.defineProperty(b.prototype, "timeoutMs", { get: function() {
            return this.foundation.getTimeoutMs();
          }, set: function(t2) {
            this.foundation.setTimeoutMs(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(b.prototype, "closeOnEscape", { get: function() {
            return this.foundation.getCloseOnEscape();
          }, set: function(t2) {
            this.foundation.setCloseOnEscape(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(b.prototype, "isOpen", { get: function() {
            return this.foundation.isOpen();
          }, enumerable: false, configurable: true }), Object.defineProperty(b.prototype, "labelText", { get: function() {
            return this.labelEl.textContent;
          }, set: function(t2) {
            this.labelEl.textContent = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(b.prototype, "actionButtonText", { get: function() {
            return this.actionEl.textContent;
          }, set: function(t2) {
            this.actionEl.textContent = t2;
          }, enumerable: false, configurable: true }), b.prototype.registerKeyDownHandler = function(t2) {
            this.listen("keydown", t2);
          }, b.prototype.deregisterKeyDownHandler = function(t2) {
            this.unlisten("keydown", t2);
          }, b.prototype.registerSurfaceClickHandler = function(t2) {
            this.surfaceEl.addEventListener("click", t2);
          }, b.prototype.deregisterSurfaceClickHandler = function(t2) {
            this.surfaceEl.removeEventListener("click", t2);
          }, b.prototype.isActionButton = function(t2) {
            return Boolean(l.closest(t2, C));
          }, b.prototype.isActionIcon = function(t2) {
            return Boolean(l.closest(t2, E));
          }, b);
          function b() {
            return null !== c && c.apply(this, arguments) || this;
          }
          e.MDCSnackbar = T;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), o = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && i3(e2, t2, n3);
            return r2(e2, t2), e2;
          }, s = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.deprecated = void 0;
          var a = o(n2(211));
          e.deprecated = a, s(n2(214), e), s(n2(215), e), s(n2(35), e), s(n2(97), e);
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(212), e), r2(n2(213), e), r2(n2(96), e), r2(n2(95), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__read || function(t2, e2) {
            var n3 = "function" == typeof Symbol && t2[Symbol.iterator];
            if (!n3) return t2;
            var i4, r3, o2 = n3.call(t2), s2 = [];
            try {
              for (; (void 0 === e2 || 0 < e2--) && !(i4 = o2.next()).done; ) s2.push(i4.value);
            } catch (t3) {
              r3 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (n3 = o2.return) && n3.call(o2);
              } finally {
                if (r3) throw r3.error;
              }
            }
            return s2;
          }, a = this && this.__spreadArray || function(t2, e2) {
            for (var n3 = 0, i4 = e2.length, r3 = t2.length; n3 < i4; n3++, r3++) t2[r3] = e2[n3];
            return t2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSwitch = void 0;
          var c, u = n2(1), l = n2(5), d = n2(3), p2 = n2(2), h = n2(4), f = n2(95), y = (c = u.MDCComponent, r2(C, c), C.attachTo = function(t2) {
            return new C(t2);
          }, C.prototype.destroy = function() {
            c.prototype.destroy.call(this), this.rippleSurface.destroy(), this.nativeControl.removeEventListener("change", this.changeHandler);
          }, C.prototype.initialSyncWithDOM = function() {
            var i4 = this;
            this.changeHandler = function() {
              for (var t2, e2 = [], n3 = 0; n3 < arguments.length; n3++) e2[n3] = arguments[n3];
              (t2 = i4.foundation).handleChange.apply(t2, a([], s(e2)));
            }, this.nativeControl.addEventListener("change", this.changeHandler), this.checked = this.checked;
          }, C.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, setNativeControlChecked: function(t3) {
              return n3.nativeControl.checked = t3;
            }, setNativeControlDisabled: function(t3) {
              return n3.nativeControl.disabled = t3;
            }, setNativeControlAttr: function(t3, e2) {
              n3.nativeControl.setAttribute(t3, e2);
            } };
            return new f.MDCSwitchFoundation(t2);
          }, Object.defineProperty(C.prototype, "ripple", { get: function() {
            return this.rippleSurface;
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "checked", { get: function() {
            return this.nativeControl.checked;
          }, set: function(t2) {
            this.foundation.setChecked(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "disabled", { get: function() {
            return this.nativeControl.disabled;
          }, set: function(t2) {
            this.foundation.setDisabled(t2);
          }, enumerable: false, configurable: true }), C.prototype.createRipple = function() {
            var n3 = this, t2 = f.MDCSwitchFoundation.strings.RIPPLE_SURFACE_SELECTOR, i4 = this.root.querySelector(t2), e2 = o(o({}, p2.MDCRipple.createAdapter(this)), { addClass: function(t3) {
              return i4.classList.add(t3);
            }, computeBoundingRect: function() {
              return i4.getBoundingClientRect();
            }, deregisterInteractionHandler: function(t3, e3) {
              n3.nativeControl.removeEventListener(t3, e3, l.applyPassive());
            }, isSurfaceActive: function() {
              return d.matches(n3.nativeControl, ":active");
            }, isUnbounded: function() {
              return true;
            }, registerInteractionHandler: function(t3, e3) {
              n3.nativeControl.addEventListener(t3, e3, l.applyPassive());
            }, removeClass: function(t3) {
              i4.classList.remove(t3);
            }, updateCssVariable: function(t3, e3) {
              i4.style.setProperty(t3, e3);
            } });
            return new p2.MDCRipple(this.root, new h.MDCRippleFoundation(e2));
          }, Object.defineProperty(C.prototype, "nativeControl", { get: function() {
            var t2 = f.MDCSwitchFoundation.strings.NATIVE_CONTROL_SELECTOR;
            return this.root.querySelector(t2);
          }, enumerable: false, configurable: true }), C);
          function C() {
            var t2 = null !== c && c.apply(this, arguments) || this;
            return t2.rippleSurface = t2.createRipple(), t2;
          }
          e.MDCSwitch = y;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCSwitch = void 0;
          var s, a = n2(1), c = n2(2), u = n2(4), l = n2(35), d = n2(97), p2 = (s = a.MDCComponent, r2(h, s), h.attachTo = function(t2) {
            return new h(t2);
          }, h.prototype.initialize = function() {
            this.ripple = new c.MDCRipple(this.root, this.createRippleFoundation());
          }, h.prototype.initialSyncWithDOM = function() {
            var t2 = this.root.querySelector(l.Selectors.RIPPLE);
            if (!t2) throw new Error("Switch " + l.Selectors.RIPPLE + " element is required.");
            this.rippleElement = t2, this.root.addEventListener("click", this.foundation.handleClick), this.foundation.initFromDOM();
          }, h.prototype.destroy = function() {
            s.prototype.destroy.call(this), this.ripple.destroy(), this.root.removeEventListener("click", this.foundation.handleClick);
          }, h.prototype.getDefaultFoundation = function() {
            return new d.MDCSwitchRenderFoundation(this.createAdapter());
          }, h.prototype.createAdapter = function() {
            var e2 = this;
            return { addClass: function(t2) {
              e2.root.classList.add(t2);
            }, hasClass: function(t2) {
              return e2.root.classList.contains(t2);
            }, isDisabled: function() {
              return e2.root.disabled;
            }, removeClass: function(t2) {
              e2.root.classList.remove(t2);
            }, setAriaChecked: function(t2) {
              return e2.root.setAttribute("aria-checked", t2);
            }, setDisabled: function(t2) {
              e2.root.disabled = t2;
            }, state: this };
          }, h.prototype.createRippleFoundation = function() {
            return new u.MDCRippleFoundation(this.createRippleAdapter());
          }, h.prototype.createRippleAdapter = function() {
            var t2 = this;
            return o(o({}, c.MDCRipple.createAdapter(this)), { computeBoundingRect: function() {
              return t2.rippleElement.getBoundingClientRect();
            }, isUnbounded: function() {
              return true;
            } });
          }, h);
          function h(t2, e2) {
            var n3 = s.call(this, t2, e2) || this;
            return n3.root = t2, n3;
          }
          e.MDCSwitch = p2;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), d = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          }, o = this && this.__read || function(t2, e2) {
            var n3 = "function" == typeof Symbol && t2[Symbol.iterator];
            if (!n3) return t2;
            var i4, r3, o2 = n3.call(t2), s2 = [];
            try {
              for (; (void 0 === e2 || 0 < e2--) && !(i4 = o2.next()).done; ) s2.push(i4.value);
            } catch (t3) {
              r3 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (n3 = o2.return) && n3.call(o2);
              } finally {
                if (r3) throw r3.error;
              }
            }
            return s2;
          }, s = this && this.__spreadArray || function(t2, e2) {
            for (var n3 = 0, i4 = e2.length, r3 = t2.length; n3 < i4; n3++, r3++) t2[r3] = e2[n3];
            return t2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCObserverFoundation = void 0;
          var a, c = n2(0), u = n2(217), l = (a = c.MDCFoundation, r2(p2, a), p2.prototype.destroy = function() {
            a.prototype.destroy.call(this), this.unobserve();
          }, p2.prototype.observe = function(t2, e2) {
            var n3, i4, r3 = this, o2 = [];
            try {
              for (var s2 = d(Object.keys(e2)), a2 = s2.next(); !a2.done; a2 = s2.next()) {
                var c2 = a2.value, u2 = e2[c2].bind(this);
                o2.push(this.observeProperty(t2, c2, u2));
              }
            } catch (t3) {
              n3 = { error: t3 };
            } finally {
              try {
                a2 && !a2.done && (i4 = s2.return) && i4.call(s2);
              } finally {
                if (n3) throw n3.error;
              }
            }
            function l2() {
              var e3, t3;
              try {
                for (var n4 = d(o2), i5 = n4.next(); !i5.done; i5 = n4.next()) (0, i5.value)();
              } catch (t4) {
                e3 = { error: t4 };
              } finally {
                try {
                  i5 && !i5.done && (t3 = n4.return) && t3.call(n4);
                } finally {
                  if (e3) throw e3.error;
                }
              }
              r3.unobserves.delete(l2);
            }
            return this.unobserves.add(l2), l2;
          }, p2.prototype.observeProperty = function(t2, e2, n3) {
            return u.observeProperty(t2, e2, n3);
          }, p2.prototype.setObserversEnabled = function(t2, e2) {
            u.setObserversEnabled(t2, e2);
          }, p2.prototype.unobserve = function() {
            var e2, t2;
            try {
              for (var n3 = d(s([], o(this.unobserves))), i4 = n3.next(); !i4.done; i4 = n3.next()) (0, i4.value)();
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, p2);
          function p2(t2) {
            var e2 = a.call(this, t2) || this;
            return e2.unobserves = /* @__PURE__ */ new Set(), e2;
          }
          e.MDCObserverFoundation = l;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, h = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          }, s = this && this.__read || function(t2, e2) {
            var n3 = "function" == typeof Symbol && t2[Symbol.iterator];
            if (!n3) return t2;
            var i4, r3, o2 = n3.call(t2), s2 = [];
            try {
              for (; (void 0 === e2 || 0 < e2--) && !(i4 = o2.next()).done; ) s2.push(i4.value);
            } catch (t3) {
              r3 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (n3 = o2.return) && n3.call(o2);
              } finally {
                if (r3) throw r3.error;
              }
            }
            return s2;
          }, a = this && this.__spreadArray || function(t2, e2) {
            for (var n3 = 0, i4 = e2.length, r3 = t2.length; n3 < i4; n3++, r3++) t2[r3] = e2[n3];
            return t2;
          };
          function f(t2, e2, n3) {
            var i4 = function(t3, s2) {
              var n4 = /* @__PURE__ */ new Map();
              l.has(t3) || l.set(t3, { isEnabled: true, getObservers: function(t4) {
                var e4 = n4.get(t4) || [];
                return n4.has(t4) || n4.set(t4, e4), e4;
              }, installedProperties: /* @__PURE__ */ new Set() });
              var a2 = l.get(t3);
              if (a2.installedProperties.has(s2)) return a2;
              var e3 = d(t3, s2) || { configurable: true, enumerable: true, value: t3[s2], writable: true }, i5 = o({}, e3), c2 = e3.get, u = e3.set;
              if ("value" in e3) {
                delete i5.value, delete i5.writable;
                var r3 = e3.value;
                c2 = function() {
                  return r3;
                }, e3.writable && (u = function(t4) {
                  r3 = t4;
                });
              }
              c2 && (i5.get = function() {
                return c2.call(this);
              });
              u && (i5.set = function(t4) {
                var e4, n5, i6 = c2 ? c2.call(this) : t4;
                if (u.call(this, t4), a2.isEnabled && (!c2 || t4 !== i6)) try {
                  for (var r4 = h(a2.getObservers(s2)), o2 = r4.next(); !o2.done; o2 = r4.next()) {
                    (0, o2.value)(t4, i6);
                  }
                } catch (t5) {
                  e4 = { error: t5 };
                } finally {
                  try {
                    o2 && !o2.done && (n5 = r4.return) && n5.call(r4);
                  } finally {
                    if (e4) throw e4.error;
                  }
                }
              });
              return a2.installedProperties.add(s2), Object.defineProperty(t3, s2, i5), a2;
            }(t2, e2).getObservers(e2);
            return i4.push(n3), function() {
              i4.splice(i4.indexOf(n3), 1);
            };
          }
          Object.defineProperty(e, "__esModule", { value: true }), e.setObserversEnabled = e.getDescriptor = e.observeProperty = e.mdcObserver = void 0, e.mdcObserver = function(t2) {
            void 0 === t2 && (t2 = function() {
            });
            var e2, p2 = /* @__PURE__ */ new WeakMap();
            return r2(n3, e2 = t2), n3.prototype.observe = function(t3, e3) {
              var n4, i4, s2 = this, a2 = [];
              try {
                for (var r3 = h(Object.keys(e3)), o2 = r3.next(); !o2.done; o2 = r3.next()) {
                  var c2 = o2.value, u = e3[c2].bind(this);
                  a2.push(f(t3, c2, u));
                }
              } catch (t4) {
                n4 = { error: t4 };
              } finally {
                try {
                  o2 && !o2.done && (i4 = r3.return) && i4.call(r3);
                } finally {
                  if (n4) throw n4.error;
                }
              }
              function l2() {
                var e4, t4;
                try {
                  for (var n5 = h(a2), i5 = n5.next(); !i5.done; i5 = n5.next()) (0, i5.value)();
                } catch (t5) {
                  e4 = { error: t5 };
                } finally {
                  try {
                    i5 && !i5.done && (t4 = n5.return) && t4.call(n5);
                  } finally {
                    if (e4) throw e4.error;
                  }
                }
                var r4 = p2.get(s2) || [], o3 = r4.indexOf(l2);
                -1 < o3 && r4.splice(o3, 1);
              }
              var d2 = p2.get(this);
              return d2 || (d2 = [], p2.set(this, d2)), d2.push(l2), l2;
            }, n3.prototype.setObserversEnabled = function(t3, e3) {
              c(t3, e3);
            }, n3.prototype.unobserve = function() {
              var e3, t3, n4 = p2.get(this) || [];
              try {
                for (var i4 = h(a([], s(n4))), r3 = i4.next(); !r3.done; r3 = i4.next()) (0, r3.value)();
              } catch (t4) {
                e3 = { error: t4 };
              } finally {
                try {
                  r3 && !r3.done && (t3 = i4.return) && t3.call(i4);
                } finally {
                  if (e3) throw e3.error;
                }
              }
            }, n3;
            function n3() {
              return null !== e2 && e2.apply(this, arguments) || this;
            }
          }, e.observeProperty = f;
          var l = /* @__PURE__ */ new WeakMap();
          function d(t2, e2) {
            for (var n3, i4 = t2; i4 && !(n3 = Object.getOwnPropertyDescriptor(i4, e2)); ) i4 = Object.getPrototypeOf(i4);
            return n3;
          }
          function c(t2, e2) {
            var n3 = l.get(t2);
            n3 && (n3.isEnabled = e2);
          }
          e.getDescriptor = d, e.setObserversEnabled = c;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(219), e), r2(n2(220), e), r2(n2(108), e), r2(n2(107), e), r2(n2(224), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabBar = void 0;
          var s, a = n2(1), c = n2(98), u = n2(101), l = n2(38), d = n2(107), p2 = d.MDCTabBarFoundation.strings, h = 0, f = (s = a.MDCComponent, r2(y, s), y.attachTo = function(t2) {
            return new y(t2);
          }, Object.defineProperty(y.prototype, "focusOnActivate", { set: function(t2) {
            var e2, n3;
            try {
              for (var i4 = o(this.tabList), r3 = i4.next(); !r3.done; r3 = i4.next()) r3.value.focusOnActivate = t2;
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                r3 && !r3.done && (n3 = i4.return) && n3.call(i4);
              } finally {
                if (e2) throw e2.error;
              }
            }
          }, enumerable: false, configurable: true }), Object.defineProperty(y.prototype, "useAutomaticActivation", { set: function(t2) {
            this.foundation.setUseAutomaticActivation(t2);
          }, enumerable: false, configurable: true }), y.prototype.initialize = function(t2, e2) {
            void 0 === t2 && (t2 = function(t3) {
              return new u.MDCTab(t3);
            }), void 0 === e2 && (e2 = function(t3) {
              return new c.MDCTabScroller(t3);
            }), this.tabList = this.instantiateTabs(t2), this.tabScroller = this.instantiatetabScroller(e2);
          }, y.prototype.initialSyncWithDOM = function() {
            var e2 = this;
            this.handleTabInteraction = function(t3) {
              e2.foundation.handleTabInteraction(t3);
            }, this.handleKeyDown = function(t3) {
              e2.foundation.handleKeyDown(t3);
            }, this.listen(l.MDCTabFoundation.strings.INTERACTED_EVENT, this.handleTabInteraction), this.listen("keydown", this.handleKeyDown);
            for (var t2 = 0; t2 < this.tabList.length; t2++) if (this.tabList[t2].active) {
              this.scrollIntoView(t2);
              break;
            }
          }, y.prototype.destroy = function() {
            var e2, t2;
            s.prototype.destroy.call(this), this.unlisten(l.MDCTabFoundation.strings.INTERACTED_EVENT, this.handleTabInteraction), this.unlisten("keydown", this.handleKeyDown);
            try {
              for (var n3 = o(this.tabList), i4 = n3.next(); !i4.done; i4 = n3.next()) i4.value.destroy();
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
            this.tabScroller && this.tabScroller.destroy();
          }, y.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { scrollTo: function(t3) {
              n3.tabScroller.scrollTo(t3);
            }, incrementScroll: function(t3) {
              n3.tabScroller.incrementScroll(t3);
            }, getScrollPosition: function() {
              return n3.tabScroller.getScrollPosition();
            }, getScrollContentWidth: function() {
              return n3.tabScroller.getScrollContentWidth();
            }, getOffsetWidth: function() {
              return n3.root.offsetWidth;
            }, isRTL: function() {
              return "rtl" === window.getComputedStyle(n3.root).getPropertyValue("direction");
            }, setActiveTab: function(t3) {
              n3.foundation.activateTab(t3);
            }, activateTabAtIndex: function(t3, e2) {
              n3.tabList[t3].activate(e2);
            }, deactivateTabAtIndex: function(t3) {
              n3.tabList[t3].deactivate();
            }, focusTabAtIndex: function(t3) {
              n3.tabList[t3].focus();
            }, getTabIndicatorClientRectAtIndex: function(t3) {
              return n3.tabList[t3].computeIndicatorClientRect();
            }, getTabDimensionsAtIndex: function(t3) {
              return n3.tabList[t3].computeDimensions();
            }, getPreviousActiveTabIndex: function() {
              for (var t3 = 0; t3 < n3.tabList.length; t3++) if (n3.tabList[t3].active) return t3;
              return -1;
            }, getFocusedTabIndex: function() {
              var t3 = n3.getTabElements(), e2 = document.activeElement;
              return t3.indexOf(e2);
            }, getIndexOfTabById: function(t3) {
              for (var e2 = 0; e2 < n3.tabList.length; e2++) if (n3.tabList[e2].id === t3) return e2;
              return -1;
            }, getTabListLength: function() {
              return n3.tabList.length;
            }, notifyTabActivated: function(t3) {
              return n3.emit(p2.TAB_ACTIVATED_EVENT, { index: t3 }, true);
            } };
            return new d.MDCTabBarFoundation(t2);
          }, y.prototype.activateTab = function(t2) {
            this.foundation.activateTab(t2);
          }, y.prototype.scrollIntoView = function(t2) {
            this.foundation.scrollIntoView(t2);
          }, y.prototype.getTabElements = function() {
            return [].slice.call(this.root.querySelectorAll(p2.TAB_SELECTOR));
          }, y.prototype.instantiateTabs = function(e2) {
            return this.getTabElements().map(function(t2) {
              return t2.id = t2.id || "mdc-tab-" + ++h, e2(t2);
            });
          }, y.prototype.instantiatetabScroller = function(t2) {
            var e2 = this.root.querySelector(p2.TAB_SCROLLER_SELECTOR);
            return e2 ? t2(e2) : null;
          }, y);
          function y() {
            return null !== s && s.apply(this, arguments) || this;
          }
          e.MDCTabBar = f;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabScrollerRTLDefault = void 0;
          var o, s = n2(37), a = (o = s.MDCTabScrollerRTL, r2(c, o), c.prototype.getScrollPositionRTL = function() {
            var t2 = this.adapter.getScrollAreaScrollLeft(), e2 = this.calculateScrollEdges().right;
            return Math.round(e2 - t2);
          }, c.prototype.scrollToRTL = function(t2) {
            var e2 = this.calculateScrollEdges(), n3 = this.adapter.getScrollAreaScrollLeft(), i4 = this.clampScrollValue(e2.right - t2);
            return { finalScrollPosition: i4, scrollDelta: i4 - n3 };
          }, c.prototype.incrementScrollRTL = function(t2) {
            var e2 = this.adapter.getScrollAreaScrollLeft(), n3 = this.clampScrollValue(e2 - t2);
            return { finalScrollPosition: n3, scrollDelta: n3 - e2 };
          }, c.prototype.getAnimatingScrollPosition = function(t2) {
            return t2;
          }, c.prototype.calculateScrollEdges = function() {
            return { left: 0, right: this.adapter.getScrollContentOffsetWidth() - this.adapter.getScrollAreaOffsetWidth() };
          }, c.prototype.clampScrollValue = function(t2) {
            var e2 = this.calculateScrollEdges();
            return Math.min(Math.max(e2.left, t2), e2.right);
          }, c);
          function c() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCTabScrollerRTLDefault = a, e.default = a;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabScrollerRTLNegative = void 0;
          var o, s = n2(37), a = (o = s.MDCTabScrollerRTL, r2(c, o), c.prototype.getScrollPositionRTL = function(t2) {
            var e2 = this.adapter.getScrollAreaScrollLeft();
            return Math.round(t2 - e2);
          }, c.prototype.scrollToRTL = function(t2) {
            var e2 = this.adapter.getScrollAreaScrollLeft(), n3 = this.clampScrollValue(-t2);
            return { finalScrollPosition: n3, scrollDelta: n3 - e2 };
          }, c.prototype.incrementScrollRTL = function(t2) {
            var e2 = this.adapter.getScrollAreaScrollLeft(), n3 = this.clampScrollValue(e2 - t2);
            return { finalScrollPosition: n3, scrollDelta: n3 - e2 };
          }, c.prototype.getAnimatingScrollPosition = function(t2, e2) {
            return t2 - e2;
          }, c.prototype.calculateScrollEdges = function() {
            var t2 = this.adapter.getScrollContentOffsetWidth();
            return { left: this.adapter.getScrollAreaOffsetWidth() - t2, right: 0 };
          }, c.prototype.clampScrollValue = function(t2) {
            var e2 = this.calculateScrollEdges();
            return Math.max(Math.min(e2.right, t2), e2.left);
          }, c);
          function c() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCTabScrollerRTLNegative = a, e.default = a;
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTabScrollerRTLReverse = void 0;
          var o, s = n2(37), a = (o = s.MDCTabScrollerRTL, r2(c, o), c.prototype.getScrollPositionRTL = function(t2) {
            var e2 = this.adapter.getScrollAreaScrollLeft();
            return Math.round(e2 - t2);
          }, c.prototype.scrollToRTL = function(t2) {
            var e2 = this.adapter.getScrollAreaScrollLeft(), n3 = this.clampScrollValue(t2);
            return { finalScrollPosition: n3, scrollDelta: e2 - n3 };
          }, c.prototype.incrementScrollRTL = function(t2) {
            var e2 = this.adapter.getScrollAreaScrollLeft(), n3 = this.clampScrollValue(e2 + t2);
            return { finalScrollPosition: n3, scrollDelta: e2 - n3 };
          }, c.prototype.getAnimatingScrollPosition = function(t2, e2) {
            return t2 + e2;
          }, c.prototype.calculateScrollEdges = function() {
            return { left: this.adapter.getScrollContentOffsetWidth() - this.adapter.getScrollAreaOffsetWidth(), right: 0 };
          }, c.prototype.clampScrollValue = function(t2) {
            var e2 = this.calculateScrollEdges();
            return Math.min(Math.max(e2.right, t2), e2.left);
          }, c);
          function c() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCTabScrollerRTLReverse = a, e.default = a;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(226), e), r2(n2(102), e), r2(n2(104), e), r2(n2(17), e), r2(n2(103), e), r2(n2(105), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), o = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && i3(e2, t2, n3);
            return r2(e2, t2), e2;
          }, s = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.util = void 0;
          var a = o(n2(100));
          e.util = a, s(n2(228), e), s(n2(98), e), s(n2(36), e), s(n2(99), e), s(n2(229), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(231), e), r2(n2(101), e), r2(n2(106), e), r2(n2(38), e), r2(n2(232), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(234), e), r2(n2(235), e), r2(n2(40), e), r2(n2(111), e), r2(n2(236), e), r2(n2(237), e), r2(n2(239), e), r2(n2(241), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__assign || function() {
            return (o = Object.assign || function(t2) {
              for (var e2, n3 = 1, i4 = arguments.length; n3 < i4; n3++) for (var r3 in e2 = arguments[n3]) Object.prototype.hasOwnProperty.call(e2, r3) && (t2[r3] = e2[r3]);
              return t2;
            }).apply(this, arguments);
          }, s = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), a = this && this.__setModuleDefault || (Object.create ? function(t2, e2) {
            Object.defineProperty(t2, "default", { enumerable: true, value: e2 });
          } : function(t2, e2) {
            t2.default = e2;
          }), c = this && this.__importStar || function(t2) {
            if (t2 && t2.__esModule) return t2;
            var e2 = {};
            if (null != t2) for (var n3 in t2) "default" !== n3 && Object.prototype.hasOwnProperty.call(t2, n3) && s(e2, t2, n3);
            return a(e2, t2), e2;
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTextField = void 0;
          var u, l = n2(1), d = n2(5), p2 = c(n2(3)), g = n2(27), _ = n2(29), m = n2(30), v = n2(2), h = n2(4), T = n2(109), b = n2(39), A = n2(40), f = n2(111), O = n2(112), I = n2(41), S = n2(114), y = (u = l.MDCComponent, r2(C, u), C.attachTo = function(t2) {
            return new C(t2);
          }, C.prototype.initialize = function(t2, e2, n3, i4, r3, o2, s2) {
            void 0 === t2 && (t2 = function(t3, e3) {
              return new v.MDCRipple(t3, e3);
            }), void 0 === e2 && (e2 = function(t3) {
              return new _.MDCLineRipple(t3);
            }), void 0 === n3 && (n3 = function(t3) {
              return new O.MDCTextFieldHelperText(t3);
            }), void 0 === i4 && (i4 = function(t3) {
              return new T.MDCTextFieldCharacterCounter(t3);
            }), void 0 === r3 && (r3 = function(t3) {
              return new S.MDCTextFieldIcon(t3);
            }), void 0 === o2 && (o2 = function(t3) {
              return new g.MDCFloatingLabel(t3);
            }), void 0 === s2 && (s2 = function(t3) {
              return new m.MDCNotchedOutline(t3);
            }), this.input = this.root.querySelector(A.strings.INPUT_SELECTOR);
            var a2 = this.root.querySelector(A.strings.LABEL_SELECTOR);
            this.label = a2 ? o2(a2) : null;
            var c2 = this.root.querySelector(A.strings.LINE_RIPPLE_SELECTOR);
            this.lineRipple = c2 ? e2(c2) : null;
            var u2 = this.root.querySelector(A.strings.OUTLINE_SELECTOR);
            this.outline = u2 ? s2(u2) : null;
            var l2 = I.MDCTextFieldHelperTextFoundation.strings, d2 = this.root.nextElementSibling, p3 = d2 && d2.classList.contains(A.cssClasses.HELPER_LINE), h7 = p3 && d2 && d2.querySelector(l2.ROOT_SELECTOR);
            this.helperText = h7 ? n3(h7) : null;
            var f2 = b.MDCTextFieldCharacterCounterFoundation.strings, y2 = this.root.querySelector(f2.ROOT_SELECTOR);
            !y2 && p3 && d2 && (y2 = d2.querySelector(f2.ROOT_SELECTOR)), this.characterCounter = y2 ? i4(y2) : null;
            var C2 = this.root.querySelector(A.strings.LEADING_ICON_SELECTOR);
            this.leadingIcon = C2 ? r3(C2) : null;
            var E = this.root.querySelector(A.strings.TRAILING_ICON_SELECTOR);
            this.trailingIcon = E ? r3(E) : null, this.prefix = this.root.querySelector(A.strings.PREFIX_SELECTOR), this.suffix = this.root.querySelector(A.strings.SUFFIX_SELECTOR), this.ripple = this.createRipple(t2);
          }, C.prototype.destroy = function() {
            this.ripple && this.ripple.destroy(), this.lineRipple && this.lineRipple.destroy(), this.helperText && this.helperText.destroy(), this.characterCounter && this.characterCounter.destroy(), this.leadingIcon && this.leadingIcon.destroy(), this.trailingIcon && this.trailingIcon.destroy(), this.label && this.label.destroy(), this.outline && this.outline.destroy(), u.prototype.destroy.call(this);
          }, C.prototype.initialSyncWithDOM = function() {
            this.disabled = this.input.disabled;
          }, Object.defineProperty(C.prototype, "value", { get: function() {
            return this.foundation.getValue();
          }, set: function(t2) {
            this.foundation.setValue(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "disabled", { get: function() {
            return this.foundation.isDisabled();
          }, set: function(t2) {
            this.foundation.setDisabled(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "valid", { get: function() {
            return this.foundation.isValid();
          }, set: function(t2) {
            this.foundation.setValid(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "required", { get: function() {
            return this.input.required;
          }, set: function(t2) {
            this.input.required = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "pattern", { get: function() {
            return this.input.pattern;
          }, set: function(t2) {
            this.input.pattern = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "minLength", { get: function() {
            return this.input.minLength;
          }, set: function(t2) {
            this.input.minLength = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "maxLength", { get: function() {
            return this.input.maxLength;
          }, set: function(t2) {
            t2 < 0 ? this.input.removeAttribute("maxLength") : this.input.maxLength = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "min", { get: function() {
            return this.input.min;
          }, set: function(t2) {
            this.input.min = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "max", { get: function() {
            return this.input.max;
          }, set: function(t2) {
            this.input.max = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "step", { get: function() {
            return this.input.step;
          }, set: function(t2) {
            this.input.step = t2;
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "helperTextContent", { set: function(t2) {
            this.foundation.setHelperTextContent(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "leadingIconAriaLabel", { set: function(t2) {
            this.foundation.setLeadingIconAriaLabel(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "leadingIconContent", { set: function(t2) {
            this.foundation.setLeadingIconContent(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "trailingIconAriaLabel", { set: function(t2) {
            this.foundation.setTrailingIconAriaLabel(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "trailingIconContent", { set: function(t2) {
            this.foundation.setTrailingIconContent(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "useNativeValidation", { set: function(t2) {
            this.foundation.setUseNativeValidation(t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "prefixText", { get: function() {
            return this.prefix ? this.prefix.textContent : null;
          }, set: function(t2) {
            this.prefix && (this.prefix.textContent = t2);
          }, enumerable: false, configurable: true }), Object.defineProperty(C.prototype, "suffixText", { get: function() {
            return this.suffix ? this.suffix.textContent : null;
          }, set: function(t2) {
            this.suffix && (this.suffix.textContent = t2);
          }, enumerable: false, configurable: true }), C.prototype.focus = function() {
            this.input.focus();
          }, C.prototype.layout = function() {
            var t2 = this.foundation.shouldFloat;
            this.foundation.notchOutline(t2);
          }, C.prototype.getDefaultFoundation = function() {
            var t2 = o(o(o(o(o({}, this.getRootAdapterMethods()), this.getInputAdapterMethods()), this.getLabelAdapterMethods()), this.getLineRippleAdapterMethods()), this.getOutlineAdapterMethods());
            return new f.MDCTextFieldFoundation(t2, this.getFoundationMap());
          }, C.prototype.getRootAdapterMethods = function() {
            var n3 = this;
            return { addClass: function(t2) {
              return n3.root.classList.add(t2);
            }, removeClass: function(t2) {
              return n3.root.classList.remove(t2);
            }, hasClass: function(t2) {
              return n3.root.classList.contains(t2);
            }, registerTextFieldInteractionHandler: function(t2, e2) {
              n3.listen(t2, e2);
            }, deregisterTextFieldInteractionHandler: function(t2, e2) {
              n3.unlisten(t2, e2);
            }, registerValidationAttributeChangeHandler: function(e2) {
              var t2 = new MutationObserver(function(t3) {
                return e2(function(t4) {
                  return t4.map(function(t5) {
                    return t5.attributeName;
                  }).filter(function(t5) {
                    return t5;
                  });
                }(t3));
              });
              return t2.observe(n3.input, { attributes: true }), t2;
            }, deregisterValidationAttributeChangeHandler: function(t2) {
              t2.disconnect();
            } };
          }, C.prototype.getInputAdapterMethods = function() {
            var n3 = this;
            return { getNativeInput: function() {
              return n3.input;
            }, setInputAttr: function(t2, e2) {
              n3.input.setAttribute(t2, e2);
            }, removeInputAttr: function(t2) {
              n3.input.removeAttribute(t2);
            }, isFocused: function() {
              return document.activeElement === n3.input;
            }, registerInputInteractionHandler: function(t2, e2) {
              n3.input.addEventListener(t2, e2, d.applyPassive());
            }, deregisterInputInteractionHandler: function(t2, e2) {
              n3.input.removeEventListener(t2, e2, d.applyPassive());
            } };
          }, C.prototype.getLabelAdapterMethods = function() {
            var e2 = this;
            return { floatLabel: function(t2) {
              e2.label && e2.label.float(t2);
            }, getLabelWidth: function() {
              return e2.label ? e2.label.getWidth() : 0;
            }, hasLabel: function() {
              return Boolean(e2.label);
            }, shakeLabel: function(t2) {
              e2.label && e2.label.shake(t2);
            }, setLabelRequired: function(t2) {
              e2.label && e2.label.setRequired(t2);
            } };
          }, C.prototype.getLineRippleAdapterMethods = function() {
            var e2 = this;
            return { activateLineRipple: function() {
              e2.lineRipple && e2.lineRipple.activate();
            }, deactivateLineRipple: function() {
              e2.lineRipple && e2.lineRipple.deactivate();
            }, setLineRippleTransformOrigin: function(t2) {
              e2.lineRipple && e2.lineRipple.setRippleCenter(t2);
            } };
          }, C.prototype.getOutlineAdapterMethods = function() {
            var e2 = this;
            return { closeOutline: function() {
              e2.outline && e2.outline.closeNotch();
            }, hasOutline: function() {
              return Boolean(e2.outline);
            }, notchOutline: function(t2) {
              e2.outline && e2.outline.notch(t2);
            } };
          }, C.prototype.getFoundationMap = function() {
            return { characterCounter: this.characterCounter ? this.characterCounter.foundationForTextField : void 0, helperText: this.helperText ? this.helperText.foundationForTextField : void 0, leadingIcon: this.leadingIcon ? this.leadingIcon.foundationForTextField : void 0, trailingIcon: this.trailingIcon ? this.trailingIcon.foundationForTextField : void 0 };
          }, C.prototype.createRipple = function(t2) {
            var n3 = this, e2 = this.root.classList.contains(A.cssClasses.TEXTAREA), i4 = this.root.classList.contains(A.cssClasses.OUTLINED);
            if (e2 || i4) return null;
            var r3 = o(o({}, v.MDCRipple.createAdapter(this)), { isSurfaceActive: function() {
              return p2.matches(n3.input, ":active");
            }, registerInteractionHandler: function(t3, e3) {
              n3.input.addEventListener(t3, e3, d.applyPassive());
            }, deregisterInteractionHandler: function(t3, e3) {
              n3.input.removeEventListener(t3, e3, d.applyPassive());
            } });
            return t2(this.root, new h.MDCRippleFoundation(r3));
          }, C);
          function C() {
            return null !== u && u.apply(this, arguments) || this;
          }
          e.MDCTextField = y;
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.characterCountStrings = e.characterCountCssClasses = void 0, r2(n2(238), e), r2(n2(109), e), r2(n2(39), e);
          var o = n2(110);
          Object.defineProperty(e, "characterCountCssClasses", { enumerable: true, get: function() {
            return o.cssClasses;
          } }), Object.defineProperty(e, "characterCountStrings", { enumerable: true, get: function() {
            return o.strings;
          } });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.helperTextStrings = e.helperTextCssClasses = void 0, r2(n2(240), e), r2(n2(112), e), r2(n2(41), e);
          var o = n2(113);
          Object.defineProperty(e, "helperTextCssClasses", { enumerable: true, get: function() {
            return o.cssClasses;
          } }), Object.defineProperty(e, "helperTextStrings", { enumerable: true, get: function() {
            return o.strings;
          } });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.iconStrings = e.iconCssClasses = void 0, r2(n2(242), e), r2(n2(114), e), r2(n2(115), e);
          var o = n2(116);
          Object.defineProperty(e, "iconCssClasses", { enumerable: true, get: function() {
            return o.cssClasses;
          } }), Object.defineProperty(e, "iconStrings", { enumerable: true, get: function() {
            return o.strings;
          } });
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(244), e), r2(n2(245), e), r2(n2(117), e), r2(n2(42), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          });
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTooltip = void 0;
          var o, s = n2(1), a = n2(42), c = n2(117), u = (o = s.MDCComponent, r2(l, o), l.attachTo = function(t2) {
            return new l(t2);
          }, l.prototype.initialize = function() {
            var t2 = this.root.getAttribute("id");
            if (!t2) throw new Error("MDCTooltip: Tooltip component must have an id.");
            var e2 = document.querySelector('[data-tooltip-id="' + t2 + '"]') || document.querySelector('[aria-describedby="' + t2 + '"]');
            if (!e2) throw new Error("MDCTooltip: Tooltip component requires an anchor element annotated with [aria-describedby] or [data-tooltip-id].");
            this.anchorElem = e2;
          }, l.prototype.initialSyncWithDOM = function() {
            var e2 = this;
            this.isTooltipRich = this.foundation.isRich(), this.isTooltipPersistent = this.foundation.isPersistent(), this.handleMouseEnter = function() {
              e2.foundation.handleAnchorMouseEnter();
            }, this.handleFocus = function(t2) {
              e2.foundation.handleAnchorFocus(t2);
            }, this.handleMouseLeave = function() {
              e2.foundation.handleAnchorMouseLeave();
            }, this.handleTransitionEnd = function() {
              e2.foundation.handleTransitionEnd();
            }, this.handleClick = function() {
              e2.foundation.handleAnchorClick();
            }, this.handleTouchstart = function() {
              e2.foundation.handleAnchorTouchstart();
            }, this.handleTouchend = function() {
              e2.foundation.handleAnchorTouchend();
            }, this.isTooltipRich && this.isTooltipPersistent ? this.anchorElem.addEventListener("click", this.handleClick) : (this.anchorElem.addEventListener("mouseenter", this.handleMouseEnter), this.anchorElem.addEventListener("focus", this.handleFocus), this.anchorElem.addEventListener("mouseleave", this.handleMouseLeave), this.anchorElem.addEventListener("touchstart", this.handleTouchstart), this.anchorElem.addEventListener("touchend", this.handleTouchend)), this.listen("transitionend", this.handleTransitionEnd);
          }, l.prototype.destroy = function() {
            this.anchorElem && (this.isTooltipRich && this.isTooltipPersistent ? this.anchorElem.removeEventListener("click", this.handleClick) : (this.anchorElem.removeEventListener("mouseenter", this.handleMouseEnter), this.anchorElem.removeEventListener("focus", this.handleFocus), this.anchorElem.removeEventListener("mouseleave", this.handleMouseLeave), this.anchorElem.removeEventListener("touchstart", this.handleTouchstart), this.anchorElem.removeEventListener("touchend", this.handleTouchend))), this.unlisten("transitionend", this.handleTransitionEnd), o.prototype.destroy.call(this);
          }, l.prototype.setTooltipPosition = function(t2) {
            this.foundation.setTooltipPosition(t2);
          }, l.prototype.setAnchorBoundaryType = function(t2) {
            this.foundation.setAnchorBoundaryType(t2);
          }, l.prototype.setShowDelay = function(t2) {
            this.foundation.setShowDelay(t2);
          }, l.prototype.setHideDelay = function(t2) {
            this.foundation.setHideDelay(t2);
          }, l.prototype.hide = function() {
            this.foundation.hide();
          }, l.prototype.isShown = function() {
            return this.foundation.isShown();
          }, l.prototype.attachScrollHandler = function(t2) {
            this.foundation.attachScrollHandler(t2);
          }, l.prototype.removeScrollHandler = function(t2) {
            this.foundation.removeScrollHandler(t2);
          }, l.prototype.getDefaultFoundation = function() {
            var r3 = this, t2 = { getAttribute: function(t3) {
              return r3.root.getAttribute(t3);
            }, setAttribute: function(t3, e2) {
              r3.root.setAttribute(t3, e2);
            }, removeAttribute: function(t3) {
              r3.root.removeAttribute(t3);
            }, addClass: function(t3) {
              r3.root.classList.add(t3);
            }, hasClass: function(t3) {
              return r3.root.classList.contains(t3);
            }, removeClass: function(t3) {
              r3.root.classList.remove(t3);
            }, getComputedStyleProperty: function(t3) {
              return window.getComputedStyle(r3.root).getPropertyValue(t3);
            }, setStyleProperty: function(t3, e2) {
              r3.root.style.setProperty(t3, e2);
            }, setSurfaceAnimationStyleProperty: function(t3, e2) {
              var n3 = r3.root.querySelector("." + a.CssClasses.SURFACE_ANIMATION);
              null == n3 || n3.style.setProperty(t3, e2);
            }, getViewportWidth: function() {
              return window.innerWidth;
            }, getViewportHeight: function() {
              return window.innerHeight;
            }, getTooltipSize: function() {
              return { width: r3.root.offsetWidth, height: r3.root.offsetHeight };
            }, getAnchorBoundingRect: function() {
              return r3.anchorElem ? r3.anchorElem.getBoundingClientRect() : null;
            }, getParentBoundingRect: function() {
              var t3, e2;
              return null !== (e2 = null === (t3 = r3.root.parentElement) || void 0 === t3 ? void 0 : t3.getBoundingClientRect()) && void 0 !== e2 ? e2 : null;
            }, getAnchorAttribute: function(t3) {
              return r3.anchorElem ? r3.anchorElem.getAttribute(t3) : null;
            }, setAnchorAttribute: function(t3, e2) {
              var n3;
              null === (n3 = r3.anchorElem) || void 0 === n3 || n3.setAttribute(t3, e2);
            }, isRTL: function() {
              return "rtl" === getComputedStyle(r3.root).direction;
            }, anchorContainsElement: function(t3) {
              var e2;
              return !!(null === (e2 = r3.anchorElem) || void 0 === e2 ? void 0 : e2.contains(t3));
            }, tooltipContainsElement: function(t3) {
              return r3.root.contains(t3);
            }, focusAnchorElement: function() {
              var t3;
              null === (t3 = r3.anchorElem) || void 0 === t3 || t3.focus();
            }, registerEventHandler: function(t3, e2) {
              r3.root instanceof HTMLElement && r3.root.addEventListener(t3, e2);
            }, deregisterEventHandler: function(t3, e2) {
              r3.root instanceof HTMLElement && r3.root.removeEventListener(t3, e2);
            }, registerAnchorEventHandler: function(t3, e2) {
              var n3;
              null === (n3 = r3.anchorElem) || void 0 === n3 || n3.addEventListener(t3, e2);
            }, deregisterAnchorEventHandler: function(t3, e2) {
              var n3;
              null === (n3 = r3.anchorElem) || void 0 === n3 || n3.removeEventListener(t3, e2);
            }, registerDocumentEventHandler: function(t3, e2) {
              document.body.addEventListener(t3, e2);
            }, deregisterDocumentEventHandler: function(t3, e2) {
              document.body.removeEventListener(t3, e2);
            }, registerWindowEventHandler: function(t3, e2) {
              window.addEventListener(t3, e2);
            }, deregisterWindowEventHandler: function(t3, e2) {
              window.removeEventListener(t3, e2);
            }, notifyHidden: function() {
              r3.emit(a.events.HIDDEN, {});
            }, getTooltipCaretBoundingRect: function() {
              var t3 = r3.root.querySelector("." + a.CssClasses.TOOLTIP_CARET_TOP);
              return t3 ? t3.getBoundingClientRect() : null;
            }, setTooltipCaretStyle: function(t3, e2) {
              var n3 = r3.root.querySelector("." + a.CssClasses.TOOLTIP_CARET_TOP), i4 = r3.root.querySelector("." + a.CssClasses.TOOLTIP_CARET_BOTTOM);
              n3 && i4 && (n3.style.setProperty(t3, e2), i4.style.setProperty(t3, e2));
            }, clearTooltipCaretStyles: function() {
              var t3 = r3.root.querySelector("." + a.CssClasses.TOOLTIP_CARET_TOP), e2 = r3.root.querySelector("." + a.CssClasses.TOOLTIP_CARET_BOTTOM);
              t3 && e2 && (t3.removeAttribute("style"), e2.removeAttribute("style"));
            }, getActiveElement: function() {
              return document.activeElement;
            } };
            return new c.MDCTooltipFoundation(t2);
          }, l);
          function l() {
            return null !== o && o.apply(this, arguments) || this;
          }
          e.MDCTooltip = u;
        }, function(t, e, n2) {
          "use strict";
          var i3 = this && this.__createBinding || (Object.create ? function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), Object.defineProperty(t2, i4, { enumerable: true, get: function() {
              return e2[n3];
            } });
          } : function(t2, e2, n3, i4) {
            void 0 === i4 && (i4 = n3), t2[i4] = e2[n3];
          }), r2 = this && this.__exportStar || function(t2, e2) {
            for (var n3 in t2) "default" === n3 || Object.prototype.hasOwnProperty.call(e2, n3) || i3(e2, t2, n3);
          };
          Object.defineProperty(e, "__esModule", { value: true }), r2(n2(247), e), r2(n2(248), e), r2(n2(9), e), r2(n2(44), e), r2(n2(118), e), r2(n2(119), e), r2(n2(43), e);
        }, function(t, e, n2) {
          "use strict";
          Object.defineProperty(e, "__esModule", { value: true });
        }, function(t, e, n2) {
          "use strict";
          var i3, r2 = this && this.__extends || (i3 = function(t2, e2) {
            return (i3 = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(t3, e3) {
              t3.__proto__ = e3;
            } || function(t3, e3) {
              for (var n3 in e3) Object.prototype.hasOwnProperty.call(e3, n3) && (t3[n3] = e3[n3]);
            })(t2, e2);
          }, function(t2, e2) {
            if ("function" != typeof e2 && null !== e2) throw new TypeError("Class extends value " + String(e2) + " is not a constructor or null");
            function n3() {
              this.constructor = t2;
            }
            i3(t2, e2), t2.prototype = null === e2 ? Object.create(e2) : (n3.prototype = e2.prototype, new n3());
          }), o = this && this.__values || function(t2) {
            var e2 = "function" == typeof Symbol && Symbol.iterator, n3 = e2 && t2[e2], i4 = 0;
            if (n3) return n3.call(t2);
            if (t2 && "number" == typeof t2.length) return { next: function() {
              return t2 && i4 >= t2.length && (t2 = void 0), { value: t2 && t2[i4++], done: !t2 };
            } };
            throw new TypeError(e2 ? "Object is not iterable." : "Symbol.iterator is not defined.");
          };
          Object.defineProperty(e, "__esModule", { value: true }), e.MDCTopAppBar = void 0;
          var s, a = n2(1), c = n2(2), u = n2(9), l = n2(118), d = n2(119), p2 = n2(43), h = (s = a.MDCComponent, r2(f, s), f.attachTo = function(t2) {
            return new f(t2);
          }, f.prototype.initialize = function(n3) {
            void 0 === n3 && (n3 = function(t3) {
              return c.MDCRipple.attachTo(t3);
            }), this.navIcon = this.root.querySelector(u.strings.NAVIGATION_ICON_SELECTOR);
            var t2 = [].slice.call(this.root.querySelectorAll(u.strings.ACTION_ITEM_SELECTOR));
            this.navIcon && t2.push(this.navIcon), this.iconRipples = t2.map(function(t3) {
              var e2 = n3(t3);
              return e2.unbounded = true, e2;
            }), this.scrollTarget = window;
          }, f.prototype.initialSyncWithDOM = function() {
            this.handleNavigationClick = this.foundation.handleNavigationClick.bind(this.foundation), this.handleWindowResize = this.foundation.handleWindowResize.bind(this.foundation), this.handleTargetScroll = this.foundation.handleTargetScroll.bind(this.foundation), this.scrollTarget.addEventListener("scroll", this.handleTargetScroll), this.navIcon && this.navIcon.addEventListener("click", this.handleNavigationClick);
            var t2 = this.root.classList.contains(u.cssClasses.FIXED_CLASS);
            this.root.classList.contains(u.cssClasses.SHORT_CLASS) || t2 || window.addEventListener("resize", this.handleWindowResize);
          }, f.prototype.destroy = function() {
            var e2, t2;
            try {
              for (var n3 = o(this.iconRipples), i4 = n3.next(); !i4.done; i4 = n3.next()) i4.value.destroy();
            } catch (t3) {
              e2 = { error: t3 };
            } finally {
              try {
                i4 && !i4.done && (t2 = n3.return) && t2.call(n3);
              } finally {
                if (e2) throw e2.error;
              }
            }
            this.scrollTarget.removeEventListener("scroll", this.handleTargetScroll), this.navIcon && this.navIcon.removeEventListener("click", this.handleNavigationClick);
            var r3 = this.root.classList.contains(u.cssClasses.FIXED_CLASS);
            this.root.classList.contains(u.cssClasses.SHORT_CLASS) || r3 || window.removeEventListener("resize", this.handleWindowResize), s.prototype.destroy.call(this);
          }, f.prototype.setScrollTarget = function(t2) {
            this.scrollTarget.removeEventListener("scroll", this.handleTargetScroll), this.scrollTarget = t2, this.handleTargetScroll = this.foundation.handleTargetScroll.bind(this.foundation), this.scrollTarget.addEventListener("scroll", this.handleTargetScroll);
          }, f.prototype.getDefaultFoundation = function() {
            var n3 = this, t2 = { hasClass: function(t3) {
              return n3.root.classList.contains(t3);
            }, addClass: function(t3) {
              return n3.root.classList.add(t3);
            }, removeClass: function(t3) {
              return n3.root.classList.remove(t3);
            }, setStyle: function(t3, e2) {
              return n3.root.style.setProperty(t3, e2);
            }, getTopAppBarHeight: function() {
              return n3.root.clientHeight;
            }, notifyNavigationIconClicked: function() {
              return n3.emit(u.strings.NAVIGATION_EVENT, {});
            }, getViewportScrollY: function() {
              var t3 = n3.scrollTarget, e2 = n3.scrollTarget;
              return void 0 !== t3.pageYOffset ? t3.pageYOffset : e2.scrollTop;
            }, getTotalActionItems: function() {
              return n3.root.querySelectorAll(u.strings.ACTION_ITEM_SELECTOR).length;
            } };
            return this.root.classList.contains(u.cssClasses.SHORT_CLASS) ? new d.MDCShortTopAppBarFoundation(t2) : this.root.classList.contains(u.cssClasses.FIXED_CLASS) ? new l.MDCFixedTopAppBarFoundation(t2) : new p2.MDCTopAppBarFoundation(t2);
          }, f);
          function f() {
            return null !== s && s.apply(this, arguments) || this;
          }
          e.MDCTopAppBar = h;
        }], r.c = i2, r.d = function(t, e, n2) {
          r.o(t, e) || Object.defineProperty(t, e, { enumerable: true, get: n2 });
        }, r.r = function(t) {
          "undefined" != typeof Symbol && Symbol.toStringTag && Object.defineProperty(t, Symbol.toStringTag, { value: "Module" }), Object.defineProperty(t, "__esModule", { value: true });
        }, r.t = function(e, t) {
          if (1 & t && (e = r(e)), 8 & t) return e;
          if (4 & t && "object" == typeof e && e && e.__esModule) return e;
          var n2 = /* @__PURE__ */ Object.create(null);
          if (r.r(n2), Object.defineProperty(n2, "default", { enumerable: true, value: e }), 2 & t && "string" != typeof e) for (var i3 in e) r.d(n2, i3, function(t2) {
            return e[t2];
          }.bind(null, i3));
          return n2;
        }, r.n = function(t) {
          var e = t && t.__esModule ? function() {
            return t.default;
          } : function() {
            return t;
          };
          return r.d(e, "a", e), e;
        }, r.o = function(t, e) {
          return Object.prototype.hasOwnProperty.call(t, e);
        }, r.p = "", r(r.s = 120);
        function r(t) {
          if (i2[t]) return i2[t].exports;
          var e = i2[t] = { i: t, l: false, exports: {} };
          return n[t].call(e.exports, e, e.exports, r), e.l = true, e.exports;
        }
        var n, i2;
      });
    }
  });

  // output/Data.Array/foreign.js
  var replicateFill = function(count, value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
  var replicatePolyfill = function(count, value) {
    var result = [];
    var n = 0;
    for (var i2 = 0; i2 < count; i2++) {
      result[n++] = value;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var indexImpl = function(just, nothing, xs, i2) {
    return i2 < 0 || i2 >= xs.length ? nothing : just(xs[i2]);
  };
  var _deleteAt = function(just, nothing, i2, l) {
    if (i2 < 0 || i2 >= l.length) return nothing;
    var l1 = l.slice();
    l1.splice(i2, 1);
    return just(l1);
  };
  var _updateAt = function(just, nothing, i2, a, l) {
    if (i2 < 0 || i2 >= l.length) return nothing;
    var l1 = l.slice();
    l1[i2] = a;
    return just(l1);
  };
  var filterImpl = function(f, xs) {
    return xs.filter(f);
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };
  var composeFlipped = function(dictSemigroupoid) {
    var compose1 = compose(dictSemigroupoid);
    return function(f) {
      return function(g) {
        return compose1(g)(f);
      };
    };
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map1 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map1(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map1 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map1($$const(x))(f);
      };
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label2) {
    return function(rec) {
      return rec[label2];
    };
  };
  var unsafeSet = function(label2) {
    return function(value) {
      return function(rec) {
        var copy = {};
        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy[key] = rec[key];
          }
        }
        copy[label2] = value;
        return copy;
      };
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupUnit = {
    append: function(v) {
      return function(v1) {
        return unit;
      };
    }
  };
  var append = function(dict) {
    return dict.append;
  };
  var semigroupFn = function(dictSemigroup) {
    var append1 = append(dictSemigroup);
    return {
      append: function(f) {
        return function(g) {
          return function(x) {
            return append1(f(x))(g(x));
          };
        };
      }
    };
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map3 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map3($$const(identity2))(a))(b);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map3 = map(dictApply.Functor0());
    return function(f) {
      return function(a) {
        return function(b) {
          return apply1(map3(f)(a))(b);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure12 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure12(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure12 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply2(pure12(f))(a);
      };
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    var pure4 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind5(f)(function(f$prime) {
          return bind5(a)(function(a$prime) {
            return pure4(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;

  // output/Data.Eq/index.js
  var eqInt = {
    eq: eqIntImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };

  // output/Data.Show/index.js
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var fromMaybe = function(a) {
    return maybe(a)(identity3);
  };
  var eqMaybe = function(dictEq) {
    var eq3 = eq(dictEq);
    return {
      eq: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return true;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return eq3(x.value0)(y.value0);
          }
          ;
          return false;
        };
      }
    };
  };

  // output/Data.Monoid/index.js
  var monoidUnit = {
    mempty: unit,
    Semigroup0: function() {
      return semigroupUnit;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };
  var monoidFn = function(dictMonoid) {
    var mempty12 = mempty(dictMonoid);
    var semigroupFn2 = semigroupFn(dictMonoid.Semigroup0());
    return {
      mempty: function(v) {
        return mempty12;
      },
      Semigroup0: function() {
        return semigroupFn2;
      }
    };
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name2, moduleName, init2) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init2();
      state2 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
  var applyEffect = /* @__PURE__ */ $lazy_applyEffect(23);
  var lift22 = /* @__PURE__ */ lift2(applyEffect);
  var semigroupEffect = function(dictSemigroup) {
    return {
      append: lift22(append(dictSemigroup))
    };
  };
  var monoidEffect = function(dictMonoid) {
    var semigroupEffect1 = semigroupEffect(dictMonoid.Semigroup0());
    return {
      mempty: pureE(mempty(dictMonoid)),
      Semigroup0: function() {
        return semigroupEffect1;
      }
    };
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var write = function(val) {
    return function(ref) {
      return function() {
        ref.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$new = _new;

  // output/Data.Array.ST/foreign.js
  var pushAllImpl = function(as, xs) {
    return xs.push.apply(xs, as);
  };
  function unsafeFreezeThawImpl(xs) {
    return xs;
  }
  var unsafeFreezeImpl = unsafeFreezeThawImpl;
  function copyImpl(xs) {
    return xs.slice();
  }
  var thawImpl = copyImpl;

  // output/Control.Monad.ST.Uncurried/foreign.js
  var runSTFn1 = function runSTFn12(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };
  var runSTFn2 = function runSTFn22(fn) {
    return function(a) {
      return function(b) {
        return function() {
          return fn(a, b);
        };
      };
    };
  };

  // output/Data.Array.ST/index.js
  var unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
  var thaw = /* @__PURE__ */ runSTFn1(thawImpl);
  var withArray = function(f) {
    return function(xs) {
      return function __do() {
        var result = thaw(xs)();
        f(result)();
        return unsafeFreeze(result)();
      };
    };
  };
  var push = function(a) {
    return runSTFn2(pushAllImpl)([a]);
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var wrap = function() {
    return coerce2;
  };
  var unwrap = function() {
    return coerce2;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure4 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr2 = foldr(dictFoldable);
      return function(f) {
        return foldr2(function($454) {
          return applySecond2(f($454));
        })(pure4(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_1(dictFoldable));
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty5 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty5;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };

  // output/Data.Function.Uncurried/foreign.js
  var runFn2 = function(fn) {
    return function(a) {
      return function(b) {
        return fn(a, b);
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };
  var runFn5 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return function(e) {
              return fn(a, b, c, d, e);
            };
          };
        };
      };
    };
  };

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(i2)(xs[i2]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.Array/index.js
  var updateAt = /* @__PURE__ */ function() {
    return runFn5(_updateAt)(Just.create)(Nothing.value);
  }();
  var snoc = function(xs) {
    return function(x) {
      return withArray(push(x))(xs)();
    };
  };
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var index = /* @__PURE__ */ function() {
    return runFn4(indexImpl)(Just.create)(Nothing.value);
  }();
  var filter = /* @__PURE__ */ runFn2(filterImpl);
  var deleteAt = /* @__PURE__ */ function() {
    return runFn4(_deleteAt)(Just.create)(Nothing.value);
  }();

  // output/Data.Int/foreign.js
  var fromStringAsImpl = function(just) {
    return function(nothing) {
      return function(radix) {
        var digits;
        if (radix < 11) {
          digits = "[0-" + (radix - 1).toString() + "]";
        } else if (radix === 11) {
          digits = "[0-9a]";
        } else {
          digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
        }
        var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");
        return function(s) {
          if (pattern.test(s)) {
            var i2 = parseInt(s, radix);
            return (i2 | 0) === i2 ? just(i2) : nothing;
          } else {
            return nothing;
          }
        };
      };
    };
  };

  // output/Data.Int/index.js
  var fromStringAs = /* @__PURE__ */ function() {
    return fromStringAsImpl(Just.create)(Nothing.value);
  }();
  var fromString = /* @__PURE__ */ fromStringAs(10);

  // output/Data.Profunctor/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var dimap = function(dict) {
    return dict.dimap;
  };
  var lcmap = function(dictProfunctor) {
    var dimap1 = dimap(dictProfunctor);
    return function(a2b) {
      return dimap1(a2b)(identity4);
    };
  };
  var rmap = function(dictProfunctor) {
    var dimap1 = dimap(dictProfunctor);
    return function(b2c) {
      return dimap1(identity4)(b2c);
    };
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Data.Variant/index.js
  var on = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(p2) {
        return function(f) {
          return function(g) {
            return function(r) {
              if (r.type === reflectSymbol2(p2)) {
                return f(r.value);
              }
              ;
              return g(r);
            };
          };
        };
      };
    };
  };
  var inj = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(p2) {
        return function(value) {
          return {
            type: reflectSymbol2(p2),
            value
          };
        };
      };
    };
  };
  var expand = function() {
    return unsafeCoerce2;
  };
  var case_ = function(r) {
    return unsafeCrashWith("Data.Variant: pattern match failure [" + (r.type + "]"));
  };

  // output/Record.Unsafe.Union/foreign.js
  function unsafeUnionFn(r1, r2) {
    var copy = {};
    for (var k1 in r2) {
      if ({}.hasOwnProperty.call(r2, k1)) {
        copy[k1] = r2[k1];
      }
    }
    for (var k2 in r1) {
      if ({}.hasOwnProperty.call(r1, k2)) {
        copy[k2] = r1[k2];
      }
    }
    return copy;
  }

  // output/Record/index.js
  var union = function() {
    return function(l) {
      return function(r) {
        return unsafeUnionFn(l, r);
      };
    };
  };
  var insert = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function() {
        return function(l) {
          return function(a) {
            return function(r) {
              return unsafeSet(reflectSymbol2(l))(a)(r);
            };
          };
        };
      };
    };
  };
  var get = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(l) {
        return function(r) {
          return unsafeGet(reflectSymbol2(l))(r);
        };
      };
    };
  };

  // output/Record.Builder/foreign.js
  function copyRecord(rec) {
    var copy = {};
    for (var key in rec) {
      if ({}.hasOwnProperty.call(rec, key)) {
        copy[key] = rec[key];
      }
    }
    return copy;
  }
  function unsafeInsert(l) {
    return function(a) {
      return function(rec) {
        rec[l] = a;
        return rec;
      };
    };
  }

  // output/Record.Builder/index.js
  var semigroupoidBuilder = semigroupoidFn;
  var insert2 = function() {
    return function() {
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(l) {
          return function(a) {
            return function(r1) {
              return unsafeInsert(reflectSymbol2(l))(a)(r1);
            };
          };
        };
      };
    };
  };
  var categoryBuilder = categoryFn;
  var build = function(v) {
    return function(r1) {
      return v(copyRecord(r1));
    };
  };
  var buildFromScratch = /* @__PURE__ */ flip(build)({});

  // output/Data.Profunctor.Row/index.js
  var identity5 = /* @__PURE__ */ identity(categoryBuilder);
  var expand2 = /* @__PURE__ */ expand();
  var compose2 = /* @__PURE__ */ compose(semigroupoidBuilder);
  var insert3 = /* @__PURE__ */ insert2()();
  var ownedRecordOutputs = function() {
    return function(dictMergeableRecords) {
      return {
        ExclusiveRows0: function() {
          return void 0;
        },
        MergeableRecords1: function() {
          return dictMergeableRecords;
        }
      };
    };
  };
  var mergeableRecords = function() {
    return function(dictFieldNames) {
      return function() {
        return function(dictFieldNames1) {
          return {
            RowToList0: function() {
              return void 0;
            },
            FieldNames1: function() {
              return dictFieldNames;
            },
            RowToList2: function() {
              return void 0;
            },
            FieldNames3: function() {
              return dictFieldNames1;
            }
          };
        };
      };
    };
  };
  var fieldNamesNilRow = {
    fieldNames: function(v) {
      return function(v1) {
        return identity5;
      };
    }
  };
  var widenVariantOutput = function(dictProfunctor) {
    var rmap2 = rmap(dictProfunctor);
    return function() {
      return rmap2(expand2);
    };
  };
  var widenRecordInput = function(dictProfunctor) {
    var lcmap2 = lcmap(dictProfunctor);
    return function() {
      return lcmap2(unsafeCoerce2);
    };
  };
  var fieldNames = function(dict) {
    return dict.fieldNames;
  };
  var fieldNamesCons = function(dictIsSymbol) {
    var insert1 = insert3(dictIsSymbol);
    var get3 = get(dictIsSymbol)();
    return function() {
      return function() {
        return function() {
          return function(dictFieldNames) {
            var fieldNames1 = fieldNames(dictFieldNames);
            return {
              fieldNames: function(v) {
                return function(r) {
                  return compose2(insert1($$Proxy.value)(get3($$Proxy.value)(r)))(fieldNames1($$Proxy.value)(r));
                };
              }
            };
          };
        };
      };
    };
  };
  var exactRow = function() {
    return function(dictFieldNames) {
      var fieldNames1 = fieldNames(dictFieldNames);
      return function(r) {
        return buildFromScratch(fieldNames1($$Proxy.value)(r));
      };
    };
  };

  // output/Data.Profunctor.Strong/index.js
  var first = function(dict) {
    return dict.first;
  };

  // output/Data.Profunctor.Row.RecordToRecord/index.js
  var union2 = /* @__PURE__ */ union();
  var exactRow2 = /* @__PURE__ */ exactRow();
  var ownedRecordOutputs2 = /* @__PURE__ */ ownedRecordOutputs();
  var mergeableRecords2 = /* @__PURE__ */ mergeableRecords();
  var recordToRecord = function(dict) {
    return dict.recordToRecord;
  };
  var pempty = function(dict) {
    return dict.pempty;
  };
  var field = function(dictIsSymbol) {
    var get3 = get(dictIsSymbol)();
    var insert4 = insert(dictIsSymbol)()();
    return function(dictProfunctor) {
      var dimap2 = dimap(dictProfunctor);
      return function() {
        return function() {
          return function() {
            return dimap2(get3($$Proxy.value))(function(v) {
              return insert4($$Proxy.value)(v)({});
            });
          };
        };
      };
    };
  };
  var completed = function(dictStrong) {
    var Profunctor0 = dictStrong.Profunctor0();
    var dimap2 = dimap(Profunctor0);
    var first2 = first(dictStrong);
    var widenRecordInput2 = widenRecordInput(Profunctor0)();
    return function() {
      return function() {
        return function() {
          return function() {
            return function(dictFieldNames) {
              var exactRow1 = exactRow2(dictFieldNames);
              return function(w) {
                var overlay = function(o) {
                  return function(i2) {
                    return union2(o)(i2);
                  };
                };
                return dimap2(function(i2) {
                  return new Tuple(i2, i2);
                })(function(v) {
                  return overlay(exactRow1(v.value0))(v.value1);
                })(first2(widenRecordInput2(w)));
              };
            };
          };
        };
      };
    };
  };
  var bind2 = function(dictRecordToRecord) {
    var recordToRecord1 = recordToRecord(dictRecordToRecord)();
    return function() {
      return function(dictOwnedRecordOutputs) {
        var MergeableRecords1 = dictOwnedRecordOutputs.MergeableRecords1();
        var recordToRecord2 = recordToRecord1(ownedRecordOutputs2(mergeableRecords2(MergeableRecords1.FieldNames1())()(MergeableRecords1.FieldNames3())));
        return function(first2) {
          return function(cont) {
            return recordToRecord2(first2)(cont(first2));
          };
        };
      };
    };
  };
  var discard2 = function(dictRecordToRecord) {
    var bind12 = bind2(dictRecordToRecord)();
    return function() {
      return function(dictOwnedRecordOutputs) {
        var MergeableRecords1 = dictOwnedRecordOutputs.MergeableRecords1();
        var bind22 = bind12(ownedRecordOutputs2(mergeableRecords2(MergeableRecords1.FieldNames1())()(MergeableRecords1.FieldNames3())));
        return function(first2) {
          return function(cont) {
            return bind22(first2)(function(v) {
              return cont(unit);
            });
          };
        };
      };
    };
  };

  // output/Data.Profunctor.Row.RecordToVariant/index.js
  var inj2 = /* @__PURE__ */ inj();
  var recordToVariant = function(dict) {
    return dict.recordToVariant;
  };
  var recordToCase = function(dictIsSymbol) {
    var inj1 = inj2(dictIsSymbol);
    return function() {
      return function(dictProfunctor) {
        return rmap(dictProfunctor)(inj1($$Proxy.value));
      };
    };
  };
  var bind3 = function(dictRecordToVariant) {
    var recordToVariant1 = recordToVariant(dictRecordToVariant)()();
    return function() {
      return function() {
        return function(first2) {
          return function(cont) {
            return recordToVariant1(first2)(cont(first2));
          };
        };
      };
    };
  };
  var discard3 = function(dictRecordToVariant) {
    var bind12 = bind3(dictRecordToVariant)()();
    return function() {
      return function() {
        return function(first2) {
          return function(cont) {
            return bind12(first2)(function(v) {
              return cont(unit);
            });
          };
        };
      };
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var length2 = function(s) {
    return s.length;
  };
  var splitAt = function(i2) {
    return function(s) {
      return { before: s.substring(0, i2), after: s.substring(i2) };
    };
  };

  // output/Data.String.CodeUnits/index.js
  var stripPrefix = function(v) {
    return function(str) {
      var v1 = splitAt(length2(v))(str);
      var $20 = v1.before === v;
      if ($20) {
        return new Just(v1.after);
      }
      ;
      return Nothing.value;
    };
  };

  // output/Data.String.Common/foreign.js
  var replaceAll = function(s1) {
    return function(s2) {
      return function(s3) {
        return s3.replace(new RegExp(s1.replace(/[-\/\\^$*+?.()|[\]{}]/g, "\\$&"), "g"), s2);
      };
    };
  };
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/MDC/foreign.js
  var import_material_components_web_min = __toESM(require_material_components_web_min(), 1);

  // node_modules/tslib/tslib.es6.js
  var extendStatics = function(d, b) {
    extendStatics = Object.setPrototypeOf || { __proto__: [] } instanceof Array && function(d2, b2) {
      d2.__proto__ = b2;
    } || function(d2, b2) {
      for (var p2 in b2) if (Object.prototype.hasOwnProperty.call(b2, p2)) d2[p2] = b2[p2];
    };
    return extendStatics(d, b);
  };
  function __extends(d, b) {
    if (typeof b !== "function" && b !== null)
      throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
    extendStatics(d, b);
    function __() {
      this.constructor = d;
    }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
  }
  var __assign = function() {
    __assign = Object.assign || function __assign2(t) {
      for (var s, i2 = 1, n = arguments.length; i2 < n; i2++) {
        s = arguments[i2];
        for (var p2 in s) if (Object.prototype.hasOwnProperty.call(s, p2)) t[p2] = s[p2];
      }
      return t;
    };
    return __assign.apply(this, arguments);
  };
  function __read(o, n) {
    var m = typeof Symbol === "function" && o[Symbol.iterator];
    if (!m) return o;
    var i2 = m.call(o), r, ar = [], e;
    try {
      while ((n === void 0 || n-- > 0) && !(r = i2.next()).done) ar.push(r.value);
    } catch (error2) {
      e = { error: error2 };
    } finally {
      try {
        if (r && !r.done && (m = i2["return"])) m.call(i2);
      } finally {
        if (e) throw e.error;
      }
    }
    return ar;
  }
  function __spreadArray(to, from2, pack) {
    if (pack || arguments.length === 2) for (var i2 = 0, l = from2.length, ar; i2 < l; i2++) {
      if (ar || !(i2 in from2)) {
        if (!ar) ar = Array.prototype.slice.call(from2, 0, i2);
        ar[i2] = from2[i2];
      }
    }
    return to.concat(ar || Array.prototype.slice.call(from2));
  }

  // node_modules/@material/base/foundation.js
  var MDCFoundation = (
    /** @class */
    function() {
      function MDCFoundation2(adapter) {
        if (adapter === void 0) {
          adapter = {};
        }
        this.adapter = adapter;
      }
      Object.defineProperty(MDCFoundation2, "cssClasses", {
        get: function() {
          return {};
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(MDCFoundation2, "strings", {
        get: function() {
          return {};
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(MDCFoundation2, "numbers", {
        get: function() {
          return {};
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(MDCFoundation2, "defaultAdapter", {
        get: function() {
          return {};
        },
        enumerable: false,
        configurable: true
      });
      MDCFoundation2.prototype.init = function() {
      };
      MDCFoundation2.prototype.destroy = function() {
      };
      return MDCFoundation2;
    }()
  );

  // node_modules/@material/base/component.js
  var MDCComponent = (
    /** @class */
    function() {
      function MDCComponent2(root, foundation) {
        var args = [];
        for (var _i = 2; _i < arguments.length; _i++) {
          args[_i - 2] = arguments[_i];
        }
        this.root = root;
        this.initialize.apply(this, __spreadArray([], __read(args)));
        this.foundation = foundation === void 0 ? this.getDefaultFoundation() : foundation;
        this.foundation.init();
        this.initialSyncWithDOM();
      }
      MDCComponent2.attachTo = function(root) {
        return new MDCComponent2(root, new MDCFoundation({}));
      };
      MDCComponent2.prototype.initialize = function() {
        var _args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
          _args[_i] = arguments[_i];
        }
      };
      MDCComponent2.prototype.getDefaultFoundation = function() {
        throw new Error("Subclasses must override getDefaultFoundation to return a properly configured foundation class");
      };
      MDCComponent2.prototype.initialSyncWithDOM = function() {
      };
      MDCComponent2.prototype.destroy = function() {
        this.foundation.destroy();
      };
      MDCComponent2.prototype.listen = function(evtType, handler, options) {
        this.root.addEventListener(evtType, handler, options);
      };
      MDCComponent2.prototype.unlisten = function(evtType, handler, options) {
        this.root.removeEventListener(evtType, handler, options);
      };
      MDCComponent2.prototype.emit = function(evtType, evtData, shouldBubble) {
        if (shouldBubble === void 0) {
          shouldBubble = false;
        }
        var evt;
        if (typeof CustomEvent === "function") {
          evt = new CustomEvent(evtType, {
            bubbles: shouldBubble,
            detail: evtData
          });
        } else {
          evt = document.createEvent("CustomEvent");
          evt.initCustomEvent(evtType, shouldBubble, false, evtData);
        }
        this.root.dispatchEvent(evt);
      };
      return MDCComponent2;
    }()
  );

  // node_modules/@material/textfield/helper-text/constants.js
  var cssClasses = {
    HELPER_TEXT_PERSISTENT: "mdc-text-field-helper-text--persistent",
    HELPER_TEXT_VALIDATION_MSG: "mdc-text-field-helper-text--validation-msg",
    ROOT: "mdc-text-field-helper-text"
  };
  var strings = {
    ARIA_HIDDEN: "aria-hidden",
    ROLE: "role",
    ROOT_SELECTOR: "." + cssClasses.ROOT
  };

  // node_modules/@material/textfield/helper-text/foundation.js
  var MDCTextFieldHelperTextFoundation = (
    /** @class */
    function(_super) {
      __extends(MDCTextFieldHelperTextFoundation2, _super);
      function MDCTextFieldHelperTextFoundation2(adapter) {
        return _super.call(this, __assign(__assign({}, MDCTextFieldHelperTextFoundation2.defaultAdapter), adapter)) || this;
      }
      Object.defineProperty(MDCTextFieldHelperTextFoundation2, "cssClasses", {
        get: function() {
          return cssClasses;
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(MDCTextFieldHelperTextFoundation2, "strings", {
        get: function() {
          return strings;
        },
        enumerable: false,
        configurable: true
      });
      Object.defineProperty(MDCTextFieldHelperTextFoundation2, "defaultAdapter", {
        /**
         * See {@link MDCTextFieldHelperTextAdapter} for typing information on parameters and return types.
         */
        get: function() {
          return {
            addClass: function() {
              return void 0;
            },
            removeClass: function() {
              return void 0;
            },
            hasClass: function() {
              return false;
            },
            getAttr: function() {
              return null;
            },
            setAttr: function() {
              return void 0;
            },
            removeAttr: function() {
              return void 0;
            },
            setContent: function() {
              return void 0;
            }
          };
        },
        enumerable: false,
        configurable: true
      });
      MDCTextFieldHelperTextFoundation2.prototype.getId = function() {
        return this.adapter.getAttr("id");
      };
      MDCTextFieldHelperTextFoundation2.prototype.isVisible = function() {
        return this.adapter.getAttr(strings.ARIA_HIDDEN) !== "true";
      };
      MDCTextFieldHelperTextFoundation2.prototype.setContent = function(content) {
        this.adapter.setContent(content);
      };
      MDCTextFieldHelperTextFoundation2.prototype.isPersistent = function() {
        return this.adapter.hasClass(cssClasses.HELPER_TEXT_PERSISTENT);
      };
      MDCTextFieldHelperTextFoundation2.prototype.setPersistent = function(isPersistent) {
        if (isPersistent) {
          this.adapter.addClass(cssClasses.HELPER_TEXT_PERSISTENT);
        } else {
          this.adapter.removeClass(cssClasses.HELPER_TEXT_PERSISTENT);
        }
      };
      MDCTextFieldHelperTextFoundation2.prototype.isValidation = function() {
        return this.adapter.hasClass(cssClasses.HELPER_TEXT_VALIDATION_MSG);
      };
      MDCTextFieldHelperTextFoundation2.prototype.setValidation = function(isValidation) {
        if (isValidation) {
          this.adapter.addClass(cssClasses.HELPER_TEXT_VALIDATION_MSG);
        } else {
          this.adapter.removeClass(cssClasses.HELPER_TEXT_VALIDATION_MSG);
        }
      };
      MDCTextFieldHelperTextFoundation2.prototype.showToScreenReader = function() {
        this.adapter.removeAttr(strings.ARIA_HIDDEN);
      };
      MDCTextFieldHelperTextFoundation2.prototype.setValidity = function(inputIsValid) {
        var helperTextIsPersistent = this.adapter.hasClass(cssClasses.HELPER_TEXT_PERSISTENT);
        var helperTextIsValidationMsg = this.adapter.hasClass(cssClasses.HELPER_TEXT_VALIDATION_MSG);
        var validationMsgNeedsDisplay = helperTextIsValidationMsg && !inputIsValid;
        if (validationMsgNeedsDisplay) {
          this.showToScreenReader();
          if (this.adapter.getAttr(strings.ROLE) === "alert") {
            this.refreshAlertRole();
          } else {
            this.adapter.setAttr(strings.ROLE, "alert");
          }
        } else {
          this.adapter.removeAttr(strings.ROLE);
        }
        if (!helperTextIsPersistent && !validationMsgNeedsDisplay) {
          this.hide();
        }
      };
      MDCTextFieldHelperTextFoundation2.prototype.hide = function() {
        this.adapter.setAttr(strings.ARIA_HIDDEN, "true");
      };
      MDCTextFieldHelperTextFoundation2.prototype.refreshAlertRole = function() {
        var _this = this;
        this.adapter.removeAttr(strings.ROLE);
        requestAnimationFrame(function() {
          _this.adapter.setAttr(strings.ROLE, "alert");
        });
      };
      return MDCTextFieldHelperTextFoundation2;
    }(MDCFoundation)
  );

  // node_modules/@material/textfield/helper-text/component.js
  var MDCTextFieldHelperText = (
    /** @class */
    function(_super) {
      __extends(MDCTextFieldHelperText2, _super);
      function MDCTextFieldHelperText2() {
        return _super !== null && _super.apply(this, arguments) || this;
      }
      MDCTextFieldHelperText2.attachTo = function(root) {
        return new MDCTextFieldHelperText2(root);
      };
      Object.defineProperty(MDCTextFieldHelperText2.prototype, "foundationForTextField", {
        // Provided for access by MDCTextField component
        get: function() {
          return this.foundation;
        },
        enumerable: false,
        configurable: true
      });
      MDCTextFieldHelperText2.prototype.getDefaultFoundation = function() {
        var _this = this;
        var adapter = {
          addClass: function(className) {
            return _this.root.classList.add(className);
          },
          removeClass: function(className) {
            return _this.root.classList.remove(className);
          },
          hasClass: function(className) {
            return _this.root.classList.contains(className);
          },
          getAttr: function(attr2) {
            return _this.root.getAttribute(attr2);
          },
          setAttr: function(attr2, value) {
            return _this.root.setAttribute(attr2, value);
          },
          removeAttr: function(attr2) {
            return _this.root.removeAttribute(attr2);
          },
          setContent: function(content) {
            _this.root.textContent = content;
          }
        };
        return new MDCTextFieldHelperTextFoundation(adapter);
      };
      return MDCTextFieldHelperText2;
    }(MDCComponent)
  );

  // output/MDC/foreign.js
  var material = import_material_components_web_min.default;
  function mdcTextFieldHelperText(node) {
    return function() {
      const comp = new MDCTextFieldHelperText(node);
      comp.getDefaultFoundation().setValidation(true);
      return comp;
    };
  }
  function useNativeValidation(comp) {
    return function(value) {
      return function() {
        comp.useNativeValidation = value;
      };
    };
  }
  function setValid(comp) {
    return function(valid) {
      return function() {
        comp.valid = valid;
      };
    };
  }
  function setContent(comp) {
    return function(content) {
      return function() {
        comp.helperTextContent = content;
      };
    };
  }
  function newComponent(cls) {
    return function(node) {
      return function() {
        return new cls(node);
      };
    };
  }

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_ = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var gets = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(f(s), s);
      });
    };
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/QualifiedDo.Semigroupoid/index.js
  var discard4 = function(dictSemigroupoid) {
    var composeFlipped2 = composeFlipped(dictSemigroupoid);
    return function(a) {
      return function(b) {
        return composeFlipped2(a)(b(unit));
      };
    };
  };

  // output/Debug/foreign.js
  var req = typeof module === "undefined" ? void 0 : module.require;
  var util = function() {
    try {
      return req === void 0 ? void 0 : req("util");
    } catch (e) {
      return void 0;
    }
  }();
  var now = function() {
    var perf;
    if (typeof performance !== "undefined") {
      perf = performance;
    } else if (req) {
      try {
        perf = req("perf_hooks").performance;
      } catch (e) {
      }
    }
    return function() {
      return (perf || Date).now();
    };
  }();

  // output/Effect.AVar/foreign.js
  var AVar = function() {
    function MutableQueue() {
      this.head = null;
      this.last = null;
      this.size = 0;
    }
    function MutableCell(queue, value) {
      this.queue = queue;
      this.value = value;
      this.next = null;
      this.prev = null;
    }
    function AVar2(value) {
      this.draining = false;
      this.error = null;
      this.value = value;
      this.takes = new MutableQueue();
      this.reads = new MutableQueue();
      this.puts = new MutableQueue();
    }
    var EMPTY = {};
    function runEff(eff) {
      try {
        eff();
      } catch (error2) {
        setTimeout(function() {
          throw error2;
        }, 0);
      }
    }
    function putLast(queue, value) {
      var cell = new MutableCell(queue, value);
      switch (queue.size) {
        case 0:
          queue.head = cell;
          break;
        case 1:
          cell.prev = queue.head;
          queue.head.next = cell;
          queue.last = cell;
          break;
        default:
          cell.prev = queue.last;
          queue.last.next = cell;
          queue.last = cell;
      }
      queue.size++;
      return cell;
    }
    function takeLast(queue) {
      var cell;
      switch (queue.size) {
        case 0:
          return null;
        case 1:
          cell = queue.head;
          queue.head = null;
          break;
        case 2:
          cell = queue.last;
          queue.head.next = null;
          queue.last = null;
          break;
        default:
          cell = queue.last;
          queue.last = cell.prev;
          queue.last.next = null;
      }
      cell.prev = null;
      cell.queue = null;
      queue.size--;
      return cell.value;
    }
    function takeHead(queue) {
      var cell;
      switch (queue.size) {
        case 0:
          return null;
        case 1:
          cell = queue.head;
          queue.head = null;
          break;
        case 2:
          cell = queue.head;
          queue.last.prev = null;
          queue.head = queue.last;
          queue.last = null;
          break;
        default:
          cell = queue.head;
          queue.head = cell.next;
          queue.head.prev = null;
      }
      cell.next = null;
      cell.queue = null;
      queue.size--;
      return cell.value;
    }
    function deleteCell(cell) {
      if (cell.queue === null) {
        return;
      }
      if (cell.queue.last === cell) {
        takeLast(cell.queue);
        return;
      }
      if (cell.queue.head === cell) {
        takeHead(cell.queue);
        return;
      }
      if (cell.prev) {
        cell.prev.next = cell.next;
      }
      if (cell.next) {
        cell.next.prev = cell.prev;
      }
      cell.queue.size--;
      cell.queue = null;
      cell.value = null;
      cell.next = null;
      cell.prev = null;
    }
    function drainVar(util2, avar) {
      if (avar.draining) {
        return;
      }
      var ps = avar.puts;
      var ts = avar.takes;
      var rs = avar.reads;
      var p2, r, t, value, rsize;
      avar.draining = true;
      while (1) {
        p2 = null;
        r = null;
        t = null;
        value = avar.value;
        rsize = rs.size;
        if (avar.error !== null) {
          value = util2.left(avar.error);
          while (p2 = takeHead(ps)) {
            runEff(p2.cb(value));
          }
          while (r = takeHead(rs)) {
            runEff(r(value));
          }
          while (t = takeHead(ts)) {
            runEff(t(value));
          }
          break;
        }
        if (value === EMPTY && (p2 = takeHead(ps))) {
          avar.value = value = p2.value;
        }
        if (value !== EMPTY) {
          t = takeHead(ts);
          while (rsize-- && (r = takeHead(rs))) {
            runEff(r(util2.right(value)));
          }
          if (t !== null) {
            avar.value = EMPTY;
            runEff(t(util2.right(value)));
          }
        }
        if (p2 !== null) {
          runEff(p2.cb(util2.right(void 0)));
        }
        if (avar.value === EMPTY && ps.size === 0 || avar.value !== EMPTY && ts.size === 0) {
          break;
        }
      }
      avar.draining = false;
    }
    AVar2.EMPTY = EMPTY;
    AVar2.putLast = putLast;
    AVar2.takeLast = takeLast;
    AVar2.takeHead = takeHead;
    AVar2.deleteCell = deleteCell;
    AVar2.drainVar = drainVar;
    return AVar2;
  }();

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler(error2) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error2) {
        setTimeout(function() {
          throw error2;
        }, 0);
      }
    }
    function runSync(left2, right2, eff) {
      try {
        return right2(eff());
      } catch (error2) {
        return left2(error2);
      }
    }
    function runAsync(left2, eff, k) {
      try {
        return eff(k)();
      } catch (error2) {
        k(left2(error2))();
        return nonCanceler;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size !== 0) {
          size--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size) % limit] = cb;
          size++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util2) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util2.isLeft(result) && util2.fromLeft(result)) {
                    setTimeout(function() {
                      throw util2.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error2) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util2, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step = aff;
      var fail = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step = bhead(step);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail = util2.left(e);
                step = null;
              }
              break;
            case STEP_RESULT:
              if (util2.isLeft(step)) {
                status = RETURN;
                fail = step;
                step = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step = util2.fromRight(step);
              }
              break;
            case CONTINUE:
              switch (step.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step._2;
                  status = CONTINUE;
                  step = step._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step = util2.right(step._1);
                  } else {
                    status = STEP_BIND;
                    step = step._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step = runSync(util2.left, util2.right, step._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step = runAsync(util2.left, step._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail = util2.left(step._1);
                  step = null;
                  break;
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util2, supervisor, step._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step._1) {
                    tmp.run();
                  }
                  step = util2.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step = sequential2(util2, supervisor, step._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step = interrupt || fail || step;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  // We cannot recover from an unmasked interrupt. Otherwise we should
                  // continue stepping, or run the exception handler if an exception
                  // was raised.
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail) {
                      status = CONTINUE;
                      step = attempt._2(util2.fromLeft(fail));
                      fail = null;
                    }
                    break;
                  // We cannot resume from an unmasked interrupt or exception.
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step = util2.fromRight(step);
                    }
                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.
                  case BRACKET:
                    bracketCount--;
                    if (fail === null) {
                      result = util2.fromRight(step);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step = attempt._3(result);
                      }
                    }
                    break;
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step = attempt._1.killed(util2.fromLeft(interrupt))(attempt._2);
                    } else if (fail) {
                      step = attempt._1.failed(util2.fromLeft(fail))(attempt._2);
                    } else {
                      step = attempt._1.completed(util2.fromRight(step))(attempt._2);
                    }
                    fail = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step, fail), attempts, interrupt);
                    status = CONTINUE;
                    step = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step = attempt._1;
                    fail = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step));
                }
              }
              joins = null;
              if (interrupt && fail) {
                setTimeout(function() {
                  throw util2.fromLeft(fail);
                }, 0);
              } else if (util2.isLeft(step) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util2.fromLeft(step);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join3) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join3.rethrow;
            join3.handler(step)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join3;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill(error2, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util2.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util2.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util2.left(error2);
              status = COMPLETED;
              step = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util2.left(error2);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step(error2)), attempts, interrupt);
                }
                status = RETURN;
                step = null;
                fail = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util2.left(error2);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step = null;
                fail = null;
              }
          }
          return canceler;
        };
      }
      function join2(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill,
        join: join2,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util2, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill(error2, par2, cb2) {
        var step = par2;
        var head = null;
        var tail = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop: while (true) {
          tmp = null;
          switch (step.tag) {
            case FORKED:
              if (step._3 === EMPTY) {
                tmp = fibers[step._1];
                kills2[count++] = tmp.kill(error2, function(result) {
                  return function() {
                    count--;
                    if (count === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head === null) {
                break loop;
              }
              step = head._2;
              if (tail === null) {
                head = null;
              } else {
                head = tail._1;
                tail = tail._2;
              }
              break;
            case MAP:
              step = step._2;
              break;
            case APPLY:
            case ALT:
              if (head) {
                tail = new Aff2(CONS, head, tail);
              }
              head = step;
              step = step._1;
              break;
          }
        }
        if (count === 0) {
          cb2(util2.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join2(result, head, tail) {
        var fail, step, lhs, rhs, tmp, kid;
        if (util2.isLeft(result)) {
          fail = result;
          step = null;
        } else {
          step = result;
          fail = null;
        }
        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head === null) {
            cb(fail || step)();
            return;
          }
          if (head._3 !== EMPTY) {
            return;
          }
          switch (head.tag) {
            case MAP:
              if (fail === null) {
                head._3 = util2.right(head._1(util2.fromRight(step)));
                step = head._3;
              } else {
                head._3 = fail;
              }
              break;
            case APPLY:
              lhs = head._1._3;
              rhs = head._2._3;
              if (fail) {
                head._3 = fail;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, fail === lhs ? head._2 : head._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join2(fail, null, null);
                    } else {
                      join2(fail, tail._1, tail._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                return;
              } else {
                step = util2.right(util2.fromRight(lhs)(util2.fromRight(rhs)));
                head._3 = step;
              }
              break;
            case ALT:
              lhs = head._1._3;
              rhs = head._2._3;
              if (lhs === EMPTY && util2.isLeft(rhs) || rhs === EMPTY && util2.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util2.isLeft(lhs) && rhs !== EMPTY && util2.isLeft(rhs)) {
                fail = step === lhs ? rhs : lhs;
                step = null;
                head._3 = fail;
              } else {
                head._3 = step;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, step === lhs ? head._2 : head._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join2(step, null, null);
                    } else {
                      join2(step, tail._1, tail._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              }
              break;
          }
          if (tail === null) {
            head = null;
          } else {
            head = tail._1;
            tail = tail._2;
          }
        }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join2(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step = par;
        var head = null;
        var tail = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step.tag) {
                case MAP:
                  if (head) {
                    tail = new Aff2(CONS, head, tail);
                  }
                  head = new Aff2(MAP, step._1, EMPTY, EMPTY);
                  step = step._2;
                  break;
                case APPLY:
                  if (head) {
                    tail = new Aff2(CONS, head, tail);
                  }
                  head = new Aff2(APPLY, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;
                case ALT:
                  if (head) {
                    tail = new Aff2(CONS, head, tail);
                  }
                  head = new Aff2(ALT, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step;
                  step = new Aff2(FORKED, fid, new Aff2(CONS, head, tail), EMPTY);
                  tmp = Fiber(util2, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head === null) {
                break loop;
              }
              if (head._1 === EMPTY) {
                head._1 = step;
                status = CONTINUE;
                step = head._2;
                head._2 = EMPTY;
              } else {
                head._2 = step;
                step = head;
                if (tail === null) {
                  head = null;
                } else {
                  head = tail._1;
                  tail = tail._2;
                }
              }
          }
        }
        root = step;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error2, cb2) {
        interrupt = util2.left(error2);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill(error2, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential2(util2, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util2, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  var _liftEffect = Aff.Sync;
  var makeAff = Aff.Async;
  var _sequential = Aff.Seq;

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
  };

  // output/UI/index.js
  var wrap2 = /* @__PURE__ */ wrap();
  var unwrap2 = /* @__PURE__ */ unwrap();
  var mempty2 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidFn(/* @__PURE__ */ monoidEffect(monoidUnit)));
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var exactRow3 = /* @__PURE__ */ exactRow();
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var union3 = /* @__PURE__ */ union();
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var profunctorUI = function(dictFunctor) {
    var map3 = map(dictFunctor);
    return {
      dimap: function(pre) {
        return function(post) {
          return function(p2) {
            return wrap2(map3(function(v) {
              return {
                toUser: function($332) {
                  return v.toUser(pre($332));
                },
                fromUser: function(prop2) {
                  return v.fromUser(function($333) {
                    return prop2(post($333));
                  });
                }
              };
            })(unwrap2(p2)));
          };
        };
      }
    };
  };
  var recordToRecordUI = function(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    var apply2 = apply(Apply0);
    var Functor0 = Apply0.Functor0();
    var map3 = map(Functor0);
    var profunctorUI1 = profunctorUI(Functor0);
    var widenRecordInput2 = widenRecordInput(profunctorUI1)();
    return {
      pempty: wrap2(pure(dictApplicative)({
        toUser: mempty2,
        fromUser: function(prop2) {
          return $$void2(prop2({}));
        }
      })),
      recordToRecord: function() {
        return function(dictOwnedRecordOutputs) {
          var MergeableRecords1 = dictOwnedRecordOutputs.MergeableRecords1();
          var exactRow1 = exactRow3(MergeableRecords1.FieldNames1());
          var exactRow22 = exactRow3(MergeableRecords1.FieldNames3());
          return function(p1) {
            return function(p2) {
              return wrap2(apply2(map3(function() {
                var p1Last = unsafePerformEffect($$new(Nothing.value));
                var p2Last = unsafePerformEffect($$new(Nothing.value));
                return function(v) {
                  return function(v1) {
                    return {
                      toUser: function($$new2) {
                        return function __do() {
                          v.toUser($$new2)();
                          return v1.toUser($$new2)();
                        };
                      },
                      fromUser: function(prop2) {
                        return function __do() {
                          v.fromUser(function(partial) {
                            var exact = exactRow1(partial);
                            var v2 = unsafePerformEffect(write(new Just(exact))(p1Last));
                            var mp2 = unsafePerformEffect(read(p2Last));
                            if (mp2 instanceof Nothing) {
                              return pure2(Nothing.value);
                            }
                            ;
                            if (mp2 instanceof Just) {
                              return prop2(union3(exact)(mp2.value0));
                            }
                            ;
                            throw new Error("Failed pattern match at UI (line 451, column 13 - line 453, column 60): " + [mp2.constructor.name]);
                          })();
                          return v1.fromUser(function(partial) {
                            var exact = exactRow22(partial);
                            var v2 = unsafePerformEffect(write(new Just(exact))(p2Last));
                            var mp1 = unsafePerformEffect(read(p1Last));
                            if (mp1 instanceof Nothing) {
                              return pure2(Nothing.value);
                            }
                            ;
                            if (mp1 instanceof Just) {
                              return prop2(union3(mp1.value0)(exact));
                            }
                            ;
                            throw new Error("Failed pattern match at UI (line 458, column 13 - line 460, column 60): " + [mp1.constructor.name]);
                          })();
                        };
                      }
                    };
                  };
                };
              }())(unwrap2(widenRecordInput2(p1))))(unwrap2(widenRecordInput2(p2))));
            };
          };
        };
      },
      Profunctor0: function() {
        return profunctorUI1;
      }
    };
  };
  var semigroupoidUI = function(dictApply) {
    var apply2 = apply(dictApply);
    var map3 = map(dictApply.Functor0());
    return {
      compose: function(p2) {
        return function(p1) {
          return wrap2(apply2(map3(function(v) {
            return function(v1) {
              return {
                toUser: function(cha) {
                  return v.toUser(cha);
                },
                fromUser: function(prop2) {
                  return function __do() {
                    v1.fromUser(prop2)();
                    return v.fromUser(function(x) {
                      return function __do2() {
                        v1.toUser(x)();
                        return Nothing.value;
                      };
                    })();
                  };
                }
              };
            };
          })(unwrap2(p1)))(unwrap2(p2)));
        };
      }
    };
  };
  var strongUI = function(dictFunctor) {
    var map3 = map(dictFunctor);
    var profunctorUI1 = profunctorUI(dictFunctor);
    return {
      first: function(p2) {
        return wrap2(map3(function() {
          var lastab = unsafePerformEffect($$new(Nothing.value));
          return function(v) {
            return {
              toUser: function(ab) {
                return function __do() {
                  write(new Just(ab))(lastab)();
                  return v.toUser(fst(ab))();
                };
              },
              fromUser: function(prop2) {
                return v.fromUser(function(b) {
                  return function __do() {
                    var mab = read(lastab)();
                    if (mab instanceof Nothing) {
                      return Nothing.value;
                    }
                    ;
                    if (mab instanceof Just) {
                      return prop2(new Tuple(b, snd(mab.value0)))();
                    }
                    ;
                    throw new Error("Failed pattern match at UI (line 90, column 13 - line 92, column 57): " + [mab.constructor.name]);
                  };
                });
              }
            };
          };
        }())(unwrap2(p2)));
      },
      second: function(p2) {
        return wrap2(map3(function() {
          var lastab = unsafePerformEffect($$new(Nothing.value));
          return function(v) {
            return {
              toUser: function(ab) {
                return function __do() {
                  write(new Just(ab))(lastab)();
                  return v.toUser(snd(ab))();
                };
              },
              fromUser: function(prop2) {
                return v.fromUser(function(b) {
                  return function __do() {
                    var mab = read(lastab)();
                    if (mab instanceof Nothing) {
                      return Nothing.value;
                    }
                    ;
                    if (mab instanceof Just) {
                      return prop2(new Tuple(fst(mab.value0), b))();
                    }
                    ;
                    throw new Error("Failed pattern match at UI (line 104, column 13 - line 106, column 57): " + [mab.constructor.name]);
                  };
                });
              }
            };
          };
        }())(unwrap2(p2)));
      },
      Profunctor0: function() {
        return profunctorUI1;
      }
    };
  };
  var updates = function(dictFunctor) {
    var mapFlipped3 = mapFlipped(dictFunctor);
    return function(handler) {
      return function(events) {
        return wrap2(mapFlipped3(unwrap2(events))(function(evts) {
          var sRef = unsafePerformEffect($$new(Nothing.value));
          var mPropRef = unsafePerformEffect($$new(Nothing.value));
          return {
            toUser: function(s) {
              return function __do() {
                write(new Just(s))(sRef)();
                evts.toUser(s)();
                var mProp = read(mPropRef)();
                return for_2(mProp)(function(prop2) {
                  return $$void2(prop2(s));
                })();
              };
            },
            fromUser: function(prop2) {
              return function __do() {
                write(new Just(prop2))(mPropRef)();
                return evts.fromUser(function(e) {
                  return function __do2() {
                    var ms = read(sRef)();
                    if (ms instanceof Nothing) {
                      return Nothing.value;
                    }
                    ;
                    if (ms instanceof Just) {
                      var s$prime = handler(e)(ms.value0);
                      write(new Just(s$prime))(sRef)();
                      return prop2(s$prime)();
                    }
                    ;
                    throw new Error("Failed pattern match at UI (line 361, column 11 - line 366, column 22): " + [ms.constructor.name]);
                  };
                })();
              };
            }
          };
        }));
      };
    };
  };
  var silence = function(dictApplicative) {
    return wrap2(pure(dictApplicative)({
      toUser: mempty2,
      fromUser: mempty2
    }));
  };
  var recordToVariantUI = function(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    var apply2 = apply(Apply0);
    var Functor0 = Apply0.Functor0();
    var map3 = map(Functor0);
    var profunctorUI1 = profunctorUI(Functor0);
    var widenVariantOutput2 = widenVariantOutput(profunctorUI1)();
    var widenRecordInput2 = widenRecordInput(profunctorUI1)();
    return {
      pempty: silence(dictApplicative),
      recordToVariant: function() {
        return function() {
          return function(p1) {
            return function(p2) {
              return wrap2(apply2(map3(function(v) {
                return function(v1) {
                  return {
                    toUser: function($$new2) {
                      return function __do() {
                        v.toUser($$new2)();
                        return v1.toUser($$new2)();
                      };
                    },
                    fromUser: function(prop2) {
                      return function __do() {
                        v.fromUser(prop2)();
                        return v1.fromUser(prop2)();
                      };
                    }
                  };
                };
              })(unwrap2(widenVariantOutput2(widenRecordInput2(p1)))))(unwrap2(widenVariantOutput2(widenRecordInput2(p2)))));
            };
          };
        };
      },
      Profunctor0: function() {
        return profunctorUI1;
      }
    };
  };
  var looped = function(dictFunctor) {
    var mapFlipped3 = mapFlipped(dictFunctor);
    return function(p2) {
      return wrap2(mapFlipped3(unwrap2(p2))(function(p$prime) {
        var busyRef = unsafePerformEffect($$new(false));
        return {
          toUser: p$prime.toUser,
          fromUser: function(prop2) {
            return p$prime.fromUser(function(u) {
              return function __do() {
                var busy = read(busyRef)();
                if (busy) {
                  return Nothing.value;
                }
                ;
                write(true)(busyRef)();
                p$prime.toUser(u)();
                write(false)(busyRef)();
                return prop2(u)();
              };
            });
          }
        };
      }));
    };
  };

  // output/Web/foreign.js
  function randomElementId() {
    return "" + Math.floor(Math.random() * 99999999 + 1e8);
  }
  function documentBody() {
    return document.body;
  }
  function createTextNode(text2) {
    return function() {
      return document.createTextNode(text2);
    };
  }
  function createElement(tag) {
    return function() {
      return document.createElement(tag);
    };
  }
  function appendChild(newNode) {
    return function(parent) {
      return function() {
        parent.appendChild(newNode);
      };
    };
  }
  function appendRawHtml(html) {
    return function(parent) {
      return function() {
        var dummyElement = document.createElement("div");
        dummyElement.innerHTML = html;
        var node = dummyElement.firstChild;
        var last = null;
        while (node !== null) {
          var next = node.nextSibling;
          parent.appendChild(node);
          last = node;
          node = next;
        }
        return last;
      };
    };
  }
  function addEventListener(eventType) {
    return function(node) {
      return function(handler) {
        return function() {
          var listener = function(event) {
            handler(event)();
          };
          node.addEventListener(eventType, listener);
          return function() {
            node.removeEventListener(eventType, listener);
          };
        };
      };
    };
  }
  function isFocused(node) {
    return function() {
      return document.activeElement === node;
    };
  }
  function getValue(node) {
    return function() {
      return node.value;
    };
  }
  function setValue(node) {
    return function(value) {
      return function() {
        node.value = value;
      };
    };
  }
  function removeAttribute(node) {
    return function(name2) {
      return function() {
        node.removeAttribute(name2);
      };
    };
  }
  function setAttribute(node) {
    return function(name2) {
      return function(value) {
        return function() {
          node.setAttribute(name2, value);
        };
      };
    };
  }
  function addClass(node) {
    return function(name2) {
      return function() {
        node.classList.add(name2);
      };
    };
  }
  function removeClass(node) {
    return function(name2) {
      return function() {
        node.classList.remove(name2);
      };
    };
  }
  function setInnerHTML(node) {
    return function(html) {
      return function() {
        node.innerHTML = html;
      };
    };
  }
  function onKeyClick(node) {
    return function(callback) {
      return function() {
        node.addEventListener("click", function(event) {
          const el2 = event.target.closest("[data-key]");
          if (el2) callback(el2.dataset.key)();
        });
      };
    };
  }

  // output/Control.Monad.State.Trans/index.js
  var runStateT = function(v) {
    return v;
  };
  var monadTransStateT = {
    lift: function(dictMonad) {
      var bind5 = bind(dictMonad.Bind1());
      var pure4 = pure(dictMonad.Applicative0());
      return function(m) {
        return function(s) {
          return bind5(m)(function(x) {
            return pure4(new Tuple(x, s));
          });
        };
      };
    }
  };
  var lift3 = /* @__PURE__ */ lift(monadTransStateT);
  var functorStateT = function(dictFunctor) {
    var map3 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map3(function(v1) {
              return new Tuple(f(v1.value0), v1.value1);
            })(v(s));
          };
        };
      }
    };
  };
  var monadStateT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeStateT(dictMonad);
      },
      Bind1: function() {
        return bindStateT(dictMonad);
      }
    };
  };
  var bindStateT = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind5(v(s))(function(v1) {
              var v3 = f(v1.value0);
              return v3(v1.value1);
            });
          };
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var applyStateT = function(dictMonad) {
    var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT1;
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    var pure4 = pure(dictMonad.Applicative0());
    return {
      pure: function(a) {
        return function(s) {
          return pure4(new Tuple(a, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var monadEffectState = function(dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var monadStateT1 = monadStateT(Monad0);
    return {
      liftEffect: function() {
        var $197 = lift3(Monad0);
        var $198 = liftEffect(dictMonadEffect);
        return function($199) {
          return $197($198($199));
        };
      }(),
      Monad0: function() {
        return monadStateT1;
      }
    };
  };
  var monadStateStateT = function(dictMonad) {
    var pure4 = pure(dictMonad.Applicative0());
    var monadStateT1 = monadStateT(dictMonad);
    return {
      state: function(f) {
        return function($200) {
          return pure4(f($200));
        };
      },
      Monad0: function() {
        return monadStateT1;
      }
    };
  };

  // output/Web/index.js
  var wrap3 = /* @__PURE__ */ wrap();
  var discard5 = /* @__PURE__ */ discard(discardUnit);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var mempty3 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidFn(/* @__PURE__ */ monoidEffect(monoidUnit)));
  var unwrap3 = /* @__PURE__ */ unwrap();
  var map2 = /* @__PURE__ */ map(functorEffect);
  var unless2 = /* @__PURE__ */ unless(applicativeEffect);
  var for_3 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorMaybe);
  var monadStateDOMWeb = /* @__PURE__ */ monadStateStateT(monadEffect);
  var gets2 = /* @__PURE__ */ gets(monadStateDOMWeb);
  var modify_2 = /* @__PURE__ */ modify_(monadStateDOMWeb);
  var monadEffectWeb = /* @__PURE__ */ monadEffectState(monadEffectEffect);
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectWeb);
  var functorWeb = /* @__PURE__ */ functorStateT(functorEffect);
  var void1 = /* @__PURE__ */ $$void(functorWeb);
  var bindWeb = /* @__PURE__ */ bindStateT(monadEffect);
  var bind1 = /* @__PURE__ */ bind(bindWeb);
  var discard22 = /* @__PURE__ */ discard5(bindWeb);
  var applyWeb = /* @__PURE__ */ applyStateT(monadEffect);
  var applicativeWeb = /* @__PURE__ */ applicativeStateT(monadEffect);
  var pure1 = /* @__PURE__ */ pure(applicativeWeb);
  var uniqueId = randomElementId;
  var staticText = function(text1) {
    return wrap3(bind1(gets2(function(v) {
      return v.parent;
    }))(function(parentNode) {
      return bind1(liftEffect2(function __do() {
        var node = createTextNode(text1)();
        appendChild(node)(parentNode)();
        return node;
      }))(function(newNode) {
        return discard22(modify_2(function(v) {
          var $84 = {};
          for (var $85 in v) {
            if ({}.hasOwnProperty.call(v, $85)) {
              $84[$85] = v[$85];
            }
            ;
          }
          ;
          $84.sibling = newNode;
          return $84;
        }))(function() {
          return pure1({
            toUser: mempty3,
            fromUser: function(prop2) {
              return $$void3(prop2({}));
            }
          });
        });
      });
    }));
  };
  var staticHTML = function(html) {
    return wrap3(bind1(gets2(function(v) {
      return v.parent;
    }))(function(parent) {
      return bind1(liftEffect2(appendRawHtml(html)(parent)))(function(newNode) {
        return discard22(modify_2(function(v) {
          var $87 = {};
          for (var $88 in v) {
            if ({}.hasOwnProperty.call(v, $88)) {
              $87[$88] = v[$88];
            }
            ;
          }
          ;
          $87.sibling = newNode;
          return $87;
        }))(function() {
          return pure1({
            toUser: mempty3,
            fromUser: function(prop2) {
              return $$void3(prop2({}));
            }
          });
        });
      });
    }));
  };
  var viewEvents = function(shell) {
    return function(render) {
      return function(wire) {
        return wrap3(bind1(unwrap3(staticHTML(shell)))(function() {
          return bind1(gets2(function(v) {
            return v.sibling;
          }))(function(node) {
            return pure1({
              toUser: function(i1) {
                return setInnerHTML(node)(render(i1));
              },
              fromUser: function(prop2) {
                return wire(node)(function($137) {
                  return $$void3(prop2($137));
                });
              }
            });
          });
        }));
      };
    };
  };
  var runDomInNode = function(node) {
    return function(v) {
      return map2(fst)(runStateT(v)({
        sibling: node,
        parent: node
      }));
    };
  };
  var runWidgetInNode = function(node) {
    return function(initial2) {
      return function(callback) {
        return function(ui) {
          return runDomInNode(node)(bind1(unwrap3(ui))(function(v) {
            return discard22(liftEffect2(v.fromUser(function(b) {
              return function __do() {
                callback(b)();
                return Nothing.value;
              };
            })))(function() {
              return void1(liftEffect2(v.toUser(initial2)));
            });
          }));
        };
      };
    };
  };
  var init = function(nodeInitializer) {
    return function(pre) {
      return function(post) {
        return function(w) {
          return wrap3(bind1(unwrap3(w))(function(w$prime) {
            return bind1(gets2(function(v) {
              return v.sibling;
            }))(function(node) {
              return bind1(liftEffect2(nodeInitializer(node)))(function(ctx) {
                return pure1({
                  toUser: function($$new2) {
                    return function __do() {
                      pre(ctx)();
                      return w$prime.toUser($$new2)();
                    };
                  },
                  fromUser: function(prop2) {
                    return w$prime.fromUser(function(change) {
                      return function __do() {
                        var status = prop2(change)();
                        post(ctx)(status)();
                        return status;
                      };
                    });
                  }
                });
              });
            });
          }));
        };
      };
    };
  };
  var escapeHtml = function(s) {
    return replaceAll('"')("&quot;")(replaceAll(">")("&gt;")(replaceAll("<")("&lt;")(replaceAll("&")("&amp;")(s))));
  };
  var element = function(tagName) {
    return function(contents) {
      return bind1(liftEffect2(createElement(tagName)))(function(newNode) {
        return bind1(gets2(function(v) {
          return v.parent;
        }))(function(parentNode) {
          return discard22(liftEffect2(appendChild(newNode)(parentNode)))(function() {
            return discard22(modify_2(function(v) {
              var $96 = {};
              for (var $97 in v) {
                if ({}.hasOwnProperty.call(v, $97)) {
                  $96[$97] = v[$97];
                }
                ;
              }
              ;
              $96.parent = newNode;
              return $96;
            }))(function() {
              return bind1(contents)(function(result) {
                return discard22(modify_2(function(v) {
                  var $99 = {};
                  for (var $100 in v) {
                    if ({}.hasOwnProperty.call(v, $100)) {
                      $99[$100] = v[$100];
                    }
                    ;
                  }
                  ;
                  $99.parent = parentNode;
                  $99.sibling = newNode;
                  return $99;
                }))(function() {
                  return pure1(result);
                });
              });
            });
          });
        });
      });
    };
  };
  var el = function(tagName) {
    var $138 = element(tagName);
    return function($139) {
      return wrap3($138(unwrap3($139)));
    };
  };
  var i = /* @__PURE__ */ el("i");
  var label = /* @__PURE__ */ el("label");
  var span = /* @__PURE__ */ el("span");
  var div2 = /* @__PURE__ */ el("div");
  var clazz = function(name2) {
    return bind1(gets2(function(v) {
      return v.sibling;
    }))(function(node) {
      return discard22(liftEffect2(addClass(node)(name2)))(function() {
        return pure1(unit);
      });
    });
  };
  var clDyn = function(name2) {
    return function(pred2) {
      return function(w) {
        return wrap3(bind1(unwrap3(w))(function(w$prime) {
          return bind1(gets2(function(v) {
            return v.sibling;
          }))(function(node) {
            return discard22(liftEffect2(function() {
              var $102 = pred2(Nothing.value);
              if ($102) {
                return addClass;
              }
              ;
              return removeClass;
            }()(node)(name2)))(function() {
              return pure1({
                toUser: function(mch) {
                  return function __do() {
                    (function() {
                      var $103 = pred2(new Just(unit));
                      if ($103) {
                        return addClass;
                      }
                      ;
                      return removeClass;
                    })()(node)(name2)();
                    return w$prime.toUser(mch)();
                  };
                },
                fromUser: w$prime.fromUser
              });
            });
          });
        }));
      };
    };
  };
  var cl = function(name2) {
    return function(w) {
      return wrap3(bind1(unwrap3(w))(function(w$prime) {
        return discard22(clazz(name2))(function() {
          return pure1({
            toUser: w$prime.toUser,
            fromUser: w$prime.fromUser
          });
        });
      }));
    };
  };
  var bodyWith = function(initial2) {
    return function(ui) {
      return function __do() {
        var node = documentBody();
        return runWidgetInNode(node)(initial2)(mempty3)(ui)();
      };
    };
  };
  var attribute = function(name2) {
    return function(value) {
      return bind1(gets2(function(v) {
        return v.sibling;
      }))(function(node) {
        return liftEffect2(setAttribute(node)(name2)(value));
      });
    };
  };
  var attrDyn = function(name2) {
    return function(valueFunction) {
      return function(w) {
        var updateAttribute = function(node) {
          return function(mnewa) {
            var v = valueFunction(voidLeft2(mnewa)(unit));
            if (v instanceof Just) {
              return setAttribute(node)(name2)(v.value0);
            }
            ;
            if (v instanceof Nothing) {
              return removeAttribute(node)(name2);
            }
            ;
            throw new Error("Failed pattern match at Web (line 377, column 36 - line 379, column 45): " + [v.constructor.name]);
          };
        };
        return wrap3(bind1(unwrap3(w))(function(w$prime) {
          return bind1(gets2(function(v) {
            return v.sibling;
          }))(function(node) {
            return discard22(liftEffect2(updateAttribute(node)(Nothing.value)))(function() {
              return pure1({
                toUser: function(mch) {
                  return function __do() {
                    updateAttribute(node)(new Just(mch))();
                    return w$prime.toUser(mch)();
                  };
                },
                fromUser: w$prime.fromUser
              });
            });
          });
        }));
      };
    };
  };
  var button = function(w) {
    return wrap3(bind1(unwrap3(attrDyn("disabled")(function(x) {
      var $106 = isNothing(x);
      if ($106) {
        return new Just("true");
      }
      ;
      return Nothing.value;
    })(el("button")(w))))(function(w$prime) {
      return bind1(liftEffect2($$new(Nothing.value)))(function(mARef) {
        return bind1(gets2(function(v) {
          return v.sibling;
        }))(function(node) {
          return pure1({
            toUser: function(occur) {
              return function __do() {
                var status = w$prime.toUser({})();
                write(new Just(occur))(mARef)();
                return status;
              };
            },
            fromUser: function(prop2) {
              return $$void3(addEventListener("click")(node)($$const(function __do() {
                var mA = read(mARef)();
                return for_3(mA)(function(a1) {
                  return function __do2() {
                    setAttribute(node)("disabled")("true")();
                    return $$void3(prop2(a1))();
                  };
                })();
              })));
            }
          });
        });
      });
    }));
  };
  var attr = function(name2) {
    return function(value) {
      return function(w) {
        return wrap3(bind1(unwrap3(w))(function(w$prime) {
          return discard22(attribute(name2)(value))(function() {
            return pure1(w$prime);
          });
        }));
      };
    };
  };
  var input = function(type_) {
    return attr("type")(type_)(wrap3(discard22(element("input")(pure1(unit)))(function() {
      return bind1(gets2(function(v) {
        return v.sibling;
      }))(function(node) {
        return bind1(liftEffect2($$new(Nothing.value)))(function(mPropRef) {
          return pure1({
            toUser: function(newa) {
              return function __do() {
                var focused = isFocused(node)();
                unless2(focused)(setValue(node)(newa))();
                var mProp = read(mPropRef)();
                return for_3(mProp)(function(prop2) {
                  return $$void3(prop2(newa));
                })();
              };
            },
            fromUser: function(prop2) {
              return function __do() {
                write(new Just(prop2))(mPropRef)();
                return $$void3(addEventListener("input")(node)($$const(function __do2() {
                  var value = getValue(node)();
                  return $$void3(prop2(value))();
                })))();
              };
            }
          });
        });
      });
    })));
  };

  // output/MDC/index.js
  var wrap4 = /* @__PURE__ */ wrap();
  var bind4 = /* @__PURE__ */ bind(bindWeb);
  var unwrap4 = /* @__PURE__ */ unwrap();
  var pure3 = /* @__PURE__ */ pure(applicativeWeb);
  var discard6 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard6(bindWeb);
  var profunctorUI2 = /* @__PURE__ */ profunctorUI(functorWeb);
  var monoidFn2 = /* @__PURE__ */ monoidFn(/* @__PURE__ */ monoidEffect(monoidUnit));
  var mempty4 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidFn(monoidFn2));
  var recordToRecordUI2 = /* @__PURE__ */ recordToRecordUI(applicativeWeb);
  var discard42 = /* @__PURE__ */ discard2(recordToRecordUI2)()(/* @__PURE__ */ ownedRecordOutputs()(/* @__PURE__ */ mergeableRecords()(fieldNamesNilRow)()(fieldNamesNilRow)));
  var pempty2 = /* @__PURE__ */ pempty(recordToRecordUI2);
  var mempty1 = /* @__PURE__ */ mempty(monoidFn2);
  var for_32 = /* @__PURE__ */ for_(applicativeWeb);
  var void12 = /* @__PURE__ */ $$void(functorWeb);
  var for_5 = /* @__PURE__ */ for_32(foldableMaybe);
  var filledTextField = function(dictIsSymbol) {
    var field2 = field(dictIsSymbol)(profunctorUI2)()()();
    return function() {
      return function(v) {
        var id = unsafePerformEffect(uniqueId);
        var helperId = unsafePerformEffect(uniqueId);
        return function() {
          var $308 = init(function(node) {
            return function __do() {
              var comp = newComponent(material.textField.MDCTextField)(node)();
              useNativeValidation(comp)(false)();
              return comp;
            };
          })(mempty1)(function(node) {
            return function(validationStatus) {
              return function __do() {
                setValid(node)(isNothing(validationStatus))();
                return setContent(node)(fromMaybe("")(validationStatus))();
              };
            };
          });
          var $309 = cl("mdc-text-field--label-floating");
          var $310 = cl("mdc-text-field--filled");
          var $311 = cl("mdc-text-field");
          return function($312) {
            return $308($309($310($311($312))));
          };
        }()(label(wrap4(bind4(unwrap4(cl("mdc-text-field__ripple")(span(pempty2))))(function() {
          return bind4(unwrap4(function() {
            var $313 = clDyn("mdc-floating-label--float-above")(isJust);
            var $314 = attr("id")(id);
            var $315 = cl("mdc-floating-label");
            return function($316) {
              return $313($314($315($316)));
            };
          }()(span(staticText(v.floatingLabel)))))(function(floating) {
            return bind4(unwrap4(field2(attr("aria-describedby")(helperId)(attr("aria-controls")(helperId)(attr("aria-labelledby")(id)(cl("mdc-text-field__input")(input("text"))))))))(function(w) {
              return bind4(unwrap4(cl("mdc-text-field-helper-line")(div2(function() {
                var $317 = init(mdcTextFieldHelperText)(mempty1)(mempty4);
                var $318 = attr("aria-hidden")("true");
                var $319 = attr("id")(helperId);
                var $320 = cl("mdc-text-field-helper-text");
                return function($321) {
                  return $317($318($319($320($321))));
                };
              }()(div2(pempty2))))))(function() {
                return bind4(unwrap4(cl("mdc-line-ripple")(span(pempty2))))(function() {
                  return pure3({
                    toUser: function(u) {
                      return function __do() {
                        floating.toUser({})();
                        return w.toUser(u)();
                      };
                    },
                    fromUser: w.fromUser
                  });
                });
              });
            });
          });
        }))));
      };
    };
  };
  var elevation20 = function(w) {
    return attr("style")("padding: 25px")(cl("mdc-elevation--z20")(div2(w)));
  };
  var containedButton = function(v) {
    return function() {
      var $363 = init(newComponent(material.ripple.MDCRipple))(mempty1)(mempty4);
      var $364 = cl("initAside-button");
      var $365 = cl("mdc-button--raised");
      var $366 = cl("mdc-button");
      return function($367) {
        return $363($364($365($366($367))));
      };
    }()(button(discard42(cl("mdc-button__ripple")(div2(pempty2)))(function() {
      return discard42(function() {
        if (v.icon instanceof Just) {
          return function() {
            var $368 = attr("aria-hidden")("true");
            var $369 = cl("mdc-button__icon");
            var $370 = cl("material-icons");
            return function($371) {
              return $368($369($370($371)));
            };
          }()(i(staticText(v.icon.value0)));
        }
        ;
        if (v.icon instanceof Nothing) {
          return pempty2;
        }
        ;
        throw new Error("Failed pattern match at MDC (line 187, column 5 - line 189, column 24): " + [v.icon.constructor.name]);
      }())(function() {
        if (v.label instanceof Just) {
          return cl("mdc-button__label")(span(staticText(v.label.value0)));
        }
        ;
        if (v.label instanceof Nothing) {
          return pempty2;
        }
        ;
        throw new Error("Failed pattern match at MDC (line 190, column 5 - line 192, column 24): " + [v.label.constructor.name]);
      });
    })));
  };
  var caption = function(w) {
    return cl("mdc-typography--caption")(span(w));
  };
  var card = function(v) {
    return function(content) {
      return function() {
        var $382 = attr("style")("padding: 10px; margin: 15px 0 15px 0; text-align: justify;");
        var $383 = cl("mdc-card");
        return function($384) {
          return $382($383($384));
        };
      }()(div2(wrap4(discard1(for_5(v.caption)(function(c) {
        return void12(unwrap4(caption(staticText(c))));
      }))(function() {
        return unwrap4(content);
      }))));
    };
  };
  var button2 = function(dictIsSymbol) {
    var recordToCase2 = recordToCase(dictIsSymbol)()(profunctorUI2);
    return function() {
      return function(config) {
        return recordToCase2(containedButton(config));
      };
    };
  };

  // output/Main/index.js
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorArray);
  var eq2 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(eqInt));
  var show2 = /* @__PURE__ */ show(showInt);
  var for_4 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var on2 = /* @__PURE__ */ on();
  var deleteIsSymbol = {
    reflectSymbol: function() {
      return "delete";
    }
  };
  var on1 = /* @__PURE__ */ on2(deleteIsSymbol);
  var updateIsSymbol = {
    reflectSymbol: function() {
      return "update";
    }
  };
  var on22 = /* @__PURE__ */ on2(updateIsSymbol);
  var createIsSymbol = {
    reflectSymbol: function() {
      return "create";
    }
  };
  var on3 = /* @__PURE__ */ on2(createIsSymbol);
  var on4 = /* @__PURE__ */ on2({
    reflectSymbol: function() {
      return "picked";
    }
  });
  var nameIsSymbol = {
    reflectSymbol: function() {
      return "name";
    }
  };
  var fieldNamesCons2 = /* @__PURE__ */ fieldNamesCons(nameIsSymbol)()()();
  var prefixIsSymbol = {
    reflectSymbol: function() {
      return "prefix";
    }
  };
  var fieldNamesCons1 = /* @__PURE__ */ fieldNamesCons(prefixIsSymbol)()()();
  var surnameIsSymbol = {
    reflectSymbol: function() {
      return "surname";
    }
  };
  var fieldNamesCons22 = /* @__PURE__ */ fieldNamesCons(surnameIsSymbol)()()()(fieldNamesNilRow);
  var discard7 = /* @__PURE__ */ discard2(/* @__PURE__ */ recordToRecordUI(applicativeWeb))();
  var ownedRecordOutputs3 = /* @__PURE__ */ ownedRecordOutputs();
  var mergeableRecords3 = /* @__PURE__ */ mergeableRecords();
  var discard12 = /* @__PURE__ */ discard7(/* @__PURE__ */ ownedRecordOutputs3(/* @__PURE__ */ mergeableRecords3(/* @__PURE__ */ fieldNamesCons2(fieldNamesNilRow))()(fieldNamesCons22)));
  var filledTextField2 = /* @__PURE__ */ filledTextField(nameIsSymbol)();
  var filledTextField1 = /* @__PURE__ */ filledTextField(surnameIsSymbol)();
  var updates2 = /* @__PURE__ */ updates(functorWeb);
  var discard23 = /* @__PURE__ */ discard3(/* @__PURE__ */ recordToVariantUI(applicativeWeb))()();
  var button3 = /* @__PURE__ */ button2(createIsSymbol)();
  var button1 = /* @__PURE__ */ button2(updateIsSymbol)();
  var button22 = /* @__PURE__ */ button2(deleteIsSymbol)();
  var listBox = /* @__PURE__ */ function() {
    var hasPrefix = function(p2) {
      return function(s) {
        var v = stripPrefix(p2)(s);
        if (v instanceof Just) {
          return true;
        }
        ;
        if (v instanceof Nothing) {
          return false;
        }
        ;
        throw new Error("Failed pattern match at Main (line 93, column 19 - line 95, column 21): " + [v.constructor.name]);
      };
    };
    var entries = function(m) {
      return filter(function(e) {
        return hasPrefix(m.prefix)(e.surname);
      })(mapWithIndex2(function(i2) {
        return function(p2) {
          return {
            key: i2,
            label: p2.surname + (", " + p2.name),
            surname: p2.surname
          };
        };
      })(m.people));
    };
    var render = function(m) {
      return joinWith("")(mapFlipped2(entries(m))(function(e) {
        return '<li class="mdc-deprecated-list-item' + (function() {
          var $90 = eq2(m.selected)(new Just(e.key));
          if ($90) {
            return " mdc-deprecated-list-item--selected";
          }
          ;
          return "";
        }() + ('" style="cursor: pointer;" data-key="' + (show2(e.key) + ('">' + (escapeHtml(e.label) + "</li>")))));
      }));
    };
    return viewEvents('<ul class="mdc-deprecated-list" style="border: 1px solid #ccc; min-height: 120px; max-height: 200px; overflow-y: auto;"></ul>')(render)(function(node) {
      return function(emit) {
        return onKeyClick(node)(function(key) {
          return for_4(fromString(key))(function(i2) {
            return emit(/* @__PURE__ */ function(variant) {
              return {
                type: "picked",
                value: variant
              };
            }(i2));
          });
        });
      };
    });
  }();
  var initial = /* @__PURE__ */ function() {
    return {
      prefix: "",
      name: "",
      surname: "",
      people: [{
        name: "Hans",
        surname: "Emil"
      }, {
        name: "Max",
        surname: "Mustermann"
      }, {
        name: "Roman",
        surname: "Tisch"
      }],
      selected: Nothing.value
    };
  }();
  var handle = function(e) {
    return function(m) {
      return on1($$Proxy.value)(function(v) {
        if (m.selected instanceof Just) {
          return {
            name: m.name,
            prefix: m.prefix,
            surname: m.surname,
            people: fromMaybe(m.people)(deleteAt(m.selected.value0)(m.people)),
            selected: Nothing.value
          };
        }
        ;
        if (m.selected instanceof Nothing) {
          return m;
        }
        ;
        throw new Error("Failed pattern match at Main (line 74, column 41 - line 76, column 19): " + [m.selected.constructor.name]);
      })(on22($$Proxy.value)(function(v) {
        if (m.selected instanceof Just) {
          return {
            name: m.name,
            prefix: m.prefix,
            selected: m.selected,
            surname: m.surname,
            people: fromMaybe(m.people)(updateAt(m.selected.value0)({
              name: m.name,
              surname: m.surname
            })(m.people))
          };
        }
        ;
        if (m.selected instanceof Nothing) {
          return m;
        }
        ;
        throw new Error("Failed pattern match at Main (line 71, column 41 - line 73, column 19): " + [m.selected.constructor.name]);
      })(on3($$Proxy.value)(function(v) {
        return {
          name: m.name,
          prefix: m.prefix,
          selected: m.selected,
          surname: m.surname,
          people: snoc(m.people)({
            name: m.name,
            surname: m.surname
          })
        };
      })(on4($$Proxy.value)(function(i2) {
        var v = index(m.people)(i2);
        if (v instanceof Just) {
          return {
            people: m.people,
            prefix: m.prefix,
            selected: new Just(i2),
            name: v.value0.name,
            surname: v.value0.surname
          };
        }
        ;
        if (v instanceof Nothing) {
          return m;
        }
        ;
        throw new Error("Failed pattern match at Main (line 66, column 41 - line 68, column 19): " + [v.constructor.name]);
      })(case_))))(e);
    };
  };
  var main = /* @__PURE__ */ function() {
    return bodyWith(initial)(elevation20(card({
      caption: new Just("CRUD")
    })(looped(functorWeb)(discard4(semigroupoidUI(applyWeb))(completed(strongUI(functorWeb))()()()()(fieldNamesCons2(fieldNamesCons1(fieldNamesCons22)))(discard7(ownedRecordOutputs3(mergeableRecords3(fieldNamesCons1(fieldNamesNilRow))()(fieldNamesCons2(fieldNamesCons22))))(filledTextField(prefixIsSymbol)()({
      floatingLabel: "Filter prefix (surname)"
    }))(function() {
      return discard12(filledTextField2({
        floatingLabel: "Name"
      }))(function() {
        return filledTextField1({
          floatingLabel: "Surname"
        });
      });
    })))(function() {
      return updates2(handle)(discard23(listBox)(function() {
        return discard23(button3({
          label: new Just("Create"),
          icon: Nothing.value
        }))(function() {
          return discard23(button1({
            label: new Just("Update"),
            icon: Nothing.value
          }))(function() {
            return button22({
              label: new Just("Delete"),
              icon: Nothing.value
            });
          });
        });
      }));
    })))));
  }();

  // <stdin>
  main();
})();
/*! Bundled license information:

@material/base/foundation.js:
  (**
   * @license
   * Copyright 2016 Google Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   * THE SOFTWARE.
   *)

@material/base/component.js:
  (**
   * @license
   * Copyright 2016 Google Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   * THE SOFTWARE.
   *)

@material/textfield/helper-text/constants.js:
  (**
   * @license
   * Copyright 2016 Google Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   * THE SOFTWARE.
   *)

@material/textfield/helper-text/foundation.js:
  (**
   * @license
   * Copyright 2017 Google Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   * THE SOFTWARE.
   *)

@material/textfield/helper-text/component.js:
  (**
   * @license
   * Copyright 2017 Google Inc.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   * THE SOFTWARE.
   *)
*/

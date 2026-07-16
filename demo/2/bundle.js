(() => {
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

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label) {
    return function(rec) {
      return rec[label];
    };
  };
  var unsafeSet = function(label) {
    return function(value) {
      return function(rec) {
        var copy = {};
        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy[key] = rec[key];
          }
        }
        copy[label] = value;
        return copy;
      };
    };
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

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

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
          return function(a2) {
            return function(r) {
              return unsafeSet(reflectSymbol2(l))(a2)(r);
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

  // output/Data.Default/index.js
  var recordDefaultNilRow = {
    defaultRecord: function(v) {
      return {};
    }
  };
  var defaultRecord = function(dict) {
    return dict.defaultRecord;
  };
  var defaultRecord1 = function() {
    return function(dictRecordDefault) {
      return {
        "default": defaultRecord(dictRecordDefault)($$Proxy.value)
      };
    };
  };
  var $$default = function(dict) {
    return dict["default"];
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

  // output/Data.Profunctor/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var dimap = function(dict) {
    return dict.dimap;
  };
  var lcmap = function(dictProfunctor) {
    var dimap1 = dimap(dictProfunctor);
    return function(a2b) {
      return dimap1(a2b)(identity2);
    };
  };

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a2) {
        return f(a2)(b);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };

  // output/Control.Apply/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map3 = map(dictApply.Functor0());
    return function(a2) {
      return function(b) {
        return apply1(map3($$const(identity3))(a2))(b);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map3 = map(dictApply.Functor0());
    return function(f) {
      return function(a2) {
        return function(b) {
          return apply1(map3(f)(a2))(b);
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
      return function(a2) {
        return apply2(pure12(f))(a2);
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

  // output/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

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

  // output/Data.Maybe/index.js
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
    var mempty1 = mempty(dictMonoid);
    var semigroupFn2 = semigroupFn(dictMonoid.Semigroup0());
    return {
      mempty: function(v) {
        return mempty1;
      },
      Semigroup0: function() {
        return semigroupFn2;
      }
    };
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
  var fst = function(v) {
    return v.value0;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure3 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr2 = foldr(dictFoldable);
      return function(f) {
        return foldr2(function($454) {
          return applySecond2(f($454));
        })(pure3(unit));
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
      var mempty4 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty4;
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
    return function(a2) {
      return function(rec) {
        rec[l] = a2;
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
          return function(a2) {
            return function(r1) {
              return unsafeInsert(reflectSymbol2(l))(a2)(r1);
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
  var identity4 = /* @__PURE__ */ identity(categoryBuilder);
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
        return identity4;
      };
    }
  };
  var widenRecordInput = function(dictProfunctor) {
    var lcmap3 = lcmap(dictProfunctor);
    return function() {
      return lcmap3(unsafeCoerce2);
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

  // output/Data.Profunctor.Row.RecordToRecord/index.js
  var ownedRecordOutputs2 = /* @__PURE__ */ ownedRecordOutputs();
  var mergeableRecords2 = /* @__PURE__ */ mergeableRecords();
  var recordToRecord = function(dict) {
    return dict.recordToRecord;
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

  // output/QualifiedDo.Semigroupoid/index.js
  var discard3 = function(dictSemigroupoid) {
    var composeFlipped2 = composeFlipped(dictSemigroupoid);
    return function(a2) {
      return function(b) {
        return composeFlipped2(a2)(b(unit));
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

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind3 = bind(dictMonad.Bind1());
    var pure3 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind3(f)(function(f$prime) {
          return bind3(a2)(function(a$prime) {
            return pure3(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name2, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
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
          var i, tmp;
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
      function run2(localRunTick) {
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
                        run2(runTick);
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
              run2(runTick);
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
                run2(++runTick);
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
            run2(runTick);
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
                run2(runTick);
              });
            } else {
              run2(runTick);
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
      function run2() {
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
      run2();
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

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
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

  // output/UI/index.js
  var wrap2 = /* @__PURE__ */ wrap();
  var unwrap2 = /* @__PURE__ */ unwrap();
  var mempty2 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidFn(/* @__PURE__ */ monoidEffect(monoidUnit)));
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var exactRow2 = /* @__PURE__ */ exactRow();
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var union2 = /* @__PURE__ */ union();
  var profunctorUI = function(dictFunctor) {
    var map3 = map(dictFunctor);
    return {
      dimap: function(pre) {
        return function(post) {
          return function(p2) {
            return wrap2(map3(function(v) {
              return {
                toUser: function($321) {
                  return v.toUser(pre($321));
                },
                fromUser: function(prop2) {
                  return v.fromUser(function($322) {
                    return prop2(post($322));
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
          var exactRow1 = exactRow2(MergeableRecords1.FieldNames1());
          var exactRow22 = exactRow2(MergeableRecords1.FieldNames3());
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
                              return prop2(union2(exact)(mp2.value0));
                            }
                            ;
                            throw new Error("Failed pattern match at UI (line 382, column 13 - line 384, column 60): " + [mp2.constructor.name]);
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
                              return prop2(union2(mp1.value0)(exact));
                            }
                            ;
                            throw new Error("Failed pattern match at UI (line 389, column 13 - line 391, column 60): " + [mp1.constructor.name]);
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
  var silence = function(dictApplicative) {
    return wrap2(pure(dictApplicative)({
      toUser: mempty2,
      fromUser: mempty2
    }));
  };

  // output/Web/foreign.js
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
  function setAttribute(node) {
    return function(name2) {
      return function(value) {
        return function() {
          node.setAttribute(name2, value);
        };
      };
    };
  }
  function setTextNodeValue(node) {
    return function(value) {
      return function() {
        node.nodeValue = value;
      };
    };
  }

  // output/Control.Monad.State.Trans/index.js
  var runStateT = function(v) {
    return v;
  };
  var monadTransStateT = {
    lift: function(dictMonad) {
      var bind3 = bind(dictMonad.Bind1());
      var pure3 = pure(dictMonad.Applicative0());
      return function(m) {
        return function(s) {
          return bind3(m)(function(x) {
            return pure3(new Tuple(x, s));
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
    var bind3 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind3(v(s))(function(v1) {
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
    var pure3 = pure(dictMonad.Applicative0());
    return {
      pure: function(a2) {
        return function(s) {
          return pure3(new Tuple(a2, s));
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
    var pure3 = pure(dictMonad.Applicative0());
    var monadStateT1 = monadStateT(dictMonad);
    return {
      state: function(f) {
        return function($200) {
          return pure3(f($200));
        };
      },
      Monad0: function() {
        return monadStateT1;
      }
    };
  };

  // output/Web/index.js
  var wrap3 = /* @__PURE__ */ wrap();
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var mempty3 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidFn(/* @__PURE__ */ monoidEffect(monoidUnit)));
  var unwrap3 = /* @__PURE__ */ unwrap();
  var map2 = /* @__PURE__ */ map(functorEffect);
  var unless2 = /* @__PURE__ */ unless(applicativeEffect);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var monadStateDOMWeb = /* @__PURE__ */ monadStateStateT(monadEffect);
  var gets2 = /* @__PURE__ */ gets(monadStateDOMWeb);
  var modify_2 = /* @__PURE__ */ modify_(monadStateDOMWeb);
  var monadEffectWeb = /* @__PURE__ */ monadEffectState(monadEffectEffect);
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectWeb);
  var functorWeb = /* @__PURE__ */ functorStateT(functorEffect);
  var void1 = /* @__PURE__ */ $$void(functorWeb);
  var bindWeb = /* @__PURE__ */ bindStateT(monadEffect);
  var bind1 = /* @__PURE__ */ bind(bindWeb);
  var discard22 = /* @__PURE__ */ discard4(bindWeb);
  var applyWeb = /* @__PURE__ */ applyStateT(monadEffect);
  var applicativeWeb = /* @__PURE__ */ applicativeStateT(monadEffect);
  var pure1 = /* @__PURE__ */ pure(applicativeWeb);
  var text = /* @__PURE__ */ wrap3(/* @__PURE__ */ bind1(/* @__PURE__ */ gets2(function(v) {
    return v.parent;
  }))(function(parentNode) {
    return bind1(liftEffect2(function __do() {
      var node = createTextNode("")();
      appendChild(node)(parentNode)();
      return node;
    }))(function(newNode) {
      return discard22(modify_2(function(v) {
        var $80 = {};
        for (var $81 in v) {
          if ({}.hasOwnProperty.call(v, $81)) {
            $80[$81] = v[$81];
          }
          ;
        }
        ;
        $80.sibling = newNode;
        return $80;
      }))(function() {
        return bind1(gets2(function(v) {
          return v.sibling;
        }))(function(node) {
          return bind1(liftEffect2($$new(unit)))(function(propRef) {
            return pure1({
              toUser: function(s) {
                return function __do() {
                  setTextNodeValue(node)(s)();
                  var prop2 = read(propRef)();
                  return $$void3(prop2({}))();
                };
              },
              fromUser: function(prop2) {
                return write(prop2)(propRef);
              }
            });
          });
        });
      });
    });
  }));
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
          var $83 = {};
          for (var $84 in v) {
            if ({}.hasOwnProperty.call(v, $84)) {
              $83[$84] = v[$84];
            }
            ;
          }
          ;
          $83.sibling = newNode;
          return $83;
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
          var $86 = {};
          for (var $87 in v) {
            if ({}.hasOwnProperty.call(v, $87)) {
              $86[$87] = v[$87];
            }
            ;
          }
          ;
          $86.sibling = newNode;
          return $86;
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
  var runDomInNode = function(node) {
    return function(v) {
      return map2(fst)(runStateT(v)({
        sibling: node,
        parent: node
      }));
    };
  };
  var runWidgetInNode = function(node) {
    return function(initial) {
      return function(callback) {
        return function(ui) {
          return runDomInNode(node)(bind1(unwrap3(ui))(function(v) {
            return discard22(liftEffect2(v.fromUser(function(b) {
              return function __do() {
                callback(b)();
                return Nothing.value;
              };
            })))(function() {
              return void1(liftEffect2(v.toUser(initial)));
            });
          }));
        };
      };
    };
  };
  var element = function(tagName) {
    return function(contents) {
      return bind1(liftEffect2(createElement(tagName)))(function(newNode) {
        return bind1(gets2(function(v) {
          return v.parent;
        }))(function(parentNode) {
          return discard22(liftEffect2(appendChild(newNode)(parentNode)))(function() {
            return discard22(modify_2(function(v) {
              var $95 = {};
              for (var $96 in v) {
                if ({}.hasOwnProperty.call(v, $96)) {
                  $95[$96] = v[$96];
                }
                ;
              }
              ;
              $95.parent = newNode;
              return $95;
            }))(function() {
              return bind1(contents)(function(result) {
                return discard22(modify_2(function(v) {
                  var $98 = {};
                  for (var $99 in v) {
                    if ({}.hasOwnProperty.call(v, $99)) {
                      $98[$99] = v[$99];
                    }
                    ;
                  }
                  ;
                  $98.parent = parentNode;
                  $98.sibling = newNode;
                  return $98;
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
    var $136 = element(tagName);
    return function($137) {
      return wrap3($136(unwrap3($137)));
    };
  };
  var li = /* @__PURE__ */ el("li");
  var p = /* @__PURE__ */ el("p");
  var ul = /* @__PURE__ */ el("ul");
  var div2 = /* @__PURE__ */ el("div");
  var body = function(dictDefault) {
    var $$default2 = $$default(dictDefault);
    return function(ui) {
      return function __do() {
        var node = documentBody();
        return runWidgetInNode(node)($$default2)(mempty3)(ui)();
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
                return for_2(mProp)(function(prop2) {
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
  var a = /* @__PURE__ */ el("a");

  // output/Main/index.js
  var discard5 = /* @__PURE__ */ discard3(/* @__PURE__ */ semigroupoidUI(applyWeb));
  var discard1 = /* @__PURE__ */ discard2(/* @__PURE__ */ recordToRecordUI(applicativeWeb))();
  var ownedRecordOutputs3 = /* @__PURE__ */ ownedRecordOutputs();
  var mergeableRecords3 = /* @__PURE__ */ mergeableRecords();
  var discard23 = /* @__PURE__ */ discard1(/* @__PURE__ */ ownedRecordOutputs3(/* @__PURE__ */ mergeableRecords3(fieldNamesNilRow)()(fieldNamesNilRow)));
  var profunctorUI2 = /* @__PURE__ */ profunctorUI(functorWeb);
  var lcmap2 = /* @__PURE__ */ lcmap(profunctorUI2);
  var greetingIsSymbol = {
    reflectSymbol: function() {
      return "greeting";
    }
  };
  var nameIsSymbol = {
    reflectSymbol: function() {
      return "name";
    }
  };
  var discard32 = /* @__PURE__ */ discard1(/* @__PURE__ */ ownedRecordOutputs3(/* @__PURE__ */ mergeableRecords3(/* @__PURE__ */ fieldNamesCons(greetingIsSymbol)()()()(fieldNamesNilRow))()(/* @__PURE__ */ fieldNamesCons(nameIsSymbol)()()()(fieldNamesNilRow))));
  var field2 = /* @__PURE__ */ field(greetingIsSymbol)(profunctorUI2)()()();
  var field1 = /* @__PURE__ */ field(nameIsSymbol)(profunctorUI2)()()();
  var silence2 = /* @__PURE__ */ silence(applicativeWeb);
  var seed = function(v) {
    return {
      greeting: "Hello",
      name: "World"
    };
  };
  var main = /* @__PURE__ */ body(/* @__PURE__ */ defaultRecord1()(recordDefaultNilRow))(/* @__PURE__ */ div2(/* @__PURE__ */ discard5(/* @__PURE__ */ discard23(/* @__PURE__ */ p(/* @__PURE__ */ staticText("Hello World!")))(function() {
    return discard23(ul(discard23(li(staticText("One")))(function() {
      return discard23(li(staticText("Two")))(function() {
        return li(staticText("Three"));
      });
    })))(function() {
      return discard23(attr("href")("https://www.google.com")(a(staticText("Search for me!"))))(function() {
        return discard23(staticHTML("<hr/>"))(function() {
          return lcmap2(seed)(discard5(discard32(field2(input("text")))(function() {
            return field1(input("text"));
          }))(function() {
            return p(lcmap2(function(r) {
              return r.greeting + (", " + (r.name + "!"));
            })(text));
          }));
        });
      });
    });
  }))(function() {
    return silence2;
  })));

  // <stdin>
  main();
})();

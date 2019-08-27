// Generated by purs version 0.13.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Data_Unfoldable1 = require("../Data.Unfoldable1/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Cardinality = function (x) {
    return x;
};
var Enum = function (Ord0, pred, succ) {
    this.Ord0 = Ord0;
    this.pred = pred;
    this.succ = succ;
};
var BoundedEnum = function (Bounded0, Enum1, cardinality, fromEnum, toEnum) {
    this.Bounded0 = Bounded0;
    this.Enum1 = Enum1;
    this.cardinality = cardinality;
    this.fromEnum = fromEnum;
    this.toEnum = toEnum;
};
var toEnum = function (dict) {
    return dict.toEnum;
};
var succ = function (dict) {
    return dict.succ;
};
var upFromIncluding = function (dictEnum) {
    return function (dictUnfoldable1) {
        return Data_Unfoldable1.unfoldr1(dictUnfoldable1)(Control_Apply.apply(Control_Apply.applyFn)(Data_Tuple.Tuple.create)(succ(dictEnum)));
    };
};
var showCardinality = new Data_Show.Show(function (v) {
    return "(Cardinality " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var pred = function (dict) {
    return dict.pred;
};
var ordCardinality = Data_Ord.ordInt;
var newtypeCardinality = new Data_Newtype.Newtype(function (n) {
    return n;
}, Cardinality);
var fromEnum = function (dict) {
    return dict.fromEnum;
};
var toEnumWithDefaults = function (dictBoundedEnum) {
    return function (low) {
        return function (high) {
            return function (x) {
                var v = toEnum(dictBoundedEnum)(x);
                if (v instanceof Data_Maybe.Just) {
                    return v.value0;
                };
                if (v instanceof Data_Maybe.Nothing) {
                    var $54 = x < fromEnum(dictBoundedEnum)(Data_Bounded.bottom(dictBoundedEnum.Bounded0()));
                    if ($54) {
                        return low;
                    };
                    return high;
                };
                throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [ v.constructor.name ]);
            };
        };
    };
};
var eqCardinality = Data_Eq.eqInt;
var enumUnit = new Enum(function () {
    return Data_Ord.ordUnit;
}, Data_Function["const"](Data_Maybe.Nothing.value), Data_Function["const"](Data_Maybe.Nothing.value));
var enumTuple = function (dictEnum) {
    return function (dictBoundedEnum) {
        return new Enum(function () {
            return Data_Tuple.ordTuple(dictEnum.Ord0())((dictBoundedEnum.Enum1()).Ord0());
        }, function (v) {
            return Data_Maybe.maybe(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Tuple.Tuple.create)(Data_Bounded.top(dictBoundedEnum.Bounded0())))(pred(dictEnum)(v.value0)))((function () {
                var $96 = Data_Tuple.Tuple.create(v.value0);
                return function ($97) {
                    return Data_Maybe.Just.create($96($97));
                };
            })())(pred(dictBoundedEnum.Enum1())(v.value1));
        }, function (v) {
            return Data_Maybe.maybe(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Tuple.Tuple.create)(Data_Bounded.bottom(dictBoundedEnum.Bounded0())))(succ(dictEnum)(v.value0)))((function () {
                var $98 = Data_Tuple.Tuple.create(v.value0);
                return function ($99) {
                    return Data_Maybe.Just.create($98($99));
                };
            })())(succ(dictBoundedEnum.Enum1())(v.value1));
        });
    };
};
var enumOrdering = new Enum(function () {
    return Data_Ord.ordOrdering;
}, function (v) {
    if (v instanceof Data_Ordering.LT) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_Ordering.EQ) {
        return new Data_Maybe.Just(Data_Ordering.LT.value);
    };
    if (v instanceof Data_Ordering.GT) {
        return new Data_Maybe.Just(Data_Ordering.EQ.value);
    };
    throw new Error("Failed pattern match at Data.Enum (line 72, column 1 - line 78, column 20): " + [ v.constructor.name ]);
}, function (v) {
    if (v instanceof Data_Ordering.LT) {
        return new Data_Maybe.Just(Data_Ordering.EQ.value);
    };
    if (v instanceof Data_Ordering.EQ) {
        return new Data_Maybe.Just(Data_Ordering.GT.value);
    };
    if (v instanceof Data_Ordering.GT) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Enum (line 72, column 1 - line 78, column 20): " + [ v.constructor.name ]);
});
var enumMaybe = function (dictBoundedEnum) {
    return new Enum(function () {
        return Data_Maybe.ordMaybe((dictBoundedEnum.Enum1()).Ord0());
    }, function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(pred(dictBoundedEnum.Enum1())(v.value0));
        };
        throw new Error("Failed pattern match at Data.Enum (line 80, column 1 - line 84, column 32): " + [ v.constructor.name ]);
    }, function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return new Data_Maybe.Just(new Data_Maybe.Just(Data_Bounded.bottom(dictBoundedEnum.Bounded0())));
        };
        if (v instanceof Data_Maybe.Just) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(succ(dictBoundedEnum.Enum1())(v.value0));
        };
        throw new Error("Failed pattern match at Data.Enum (line 80, column 1 - line 84, column 32): " + [ v.constructor.name ]);
    });
};
var enumInt = new Enum(function () {
    return Data_Ord.ordInt;
}, function (n) {
    var $67 = n > Data_Bounded.bottom(Data_Bounded.boundedInt);
    if ($67) {
        return new Data_Maybe.Just(n - 1 | 0);
    };
    return Data_Maybe.Nothing.value;
}, function (n) {
    var $68 = n < Data_Bounded.top(Data_Bounded.boundedInt);
    if ($68) {
        return new Data_Maybe.Just(n + 1 | 0);
    };
    return Data_Maybe.Nothing.value;
});
var enumFromTo = function (dictEnum) {
    return function (dictUnfoldable1) {
        var go = function (step) {
            return function (op) {
                return function (to) {
                    return function (a) {
                        return new Data_Tuple.Tuple(a, Control_Bind.bind(Data_Maybe.bindMaybe)(step(a))(function (a$prime) {
                            return Data_Functor.voidLeft(Data_Maybe.functorMaybe)(Control_MonadZero.guard(Data_Maybe.monadZeroMaybe)(op(a$prime)(to)))(a$prime);
                        }));
                    };
                };
            };
        };
        return function (v) {
            return function (v1) {
                if (Data_Eq.eq((dictEnum.Ord0()).Eq0())(v)(v1)) {
                    return Data_Unfoldable1.singleton(dictUnfoldable1)(v);
                };
                if (Data_Ord.lessThan(dictEnum.Ord0())(v)(v1)) {
                    return Data_Unfoldable1.unfoldr1(dictUnfoldable1)(go(succ(dictEnum))(Data_Ord.lessThanOrEq(dictEnum.Ord0()))(v1))(v);
                };
                if (Data_Boolean.otherwise) {
                    return Data_Unfoldable1.unfoldr1(dictUnfoldable1)(go(pred(dictEnum))(Data_Ord.greaterThanOrEq(dictEnum.Ord0()))(v1))(v);
                };
                throw new Error("Failed pattern match at Data.Enum (line 183, column 14 - line 187, column 51): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    };
};
var enumFromThenTo = function (dictUnfoldable) {
    return function (dictFunctor) {
        return function (dictBoundedEnum) {
            var go = function (step) {
                return function (to) {
                    return function (e) {
                        if (e <= to) {
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(e, e + step | 0));
                        };
                        if (Data_Boolean.otherwise) {
                            return Data_Maybe.Nothing.value;
                        };
                        throw new Error("Failed pattern match at Data.Enum (line 214, column 5 - line 216, column 28): " + [ step.constructor.name, to.constructor.name, e.constructor.name ]);
                    };
                };
            };
            return function (a) {
                return function (b) {
                    return function (c) {
                        var c$prime = fromEnum(dictBoundedEnum)(c);
                        var b$prime = fromEnum(dictBoundedEnum)(b);
                        var a$prime = fromEnum(dictBoundedEnum)(a);
                        return Data_Functor.map(dictFunctor)((function () {
                            var $100 = Data_Maybe.fromJust();
                            var $101 = toEnum(dictBoundedEnum);
                            return function ($102) {
                                return $100($101($102));
                            };
                        })())(Data_Unfoldable.unfoldr(dictUnfoldable)(go(b$prime - a$prime | 0)(c$prime))(a$prime));
                    };
                };
            };
        };
    };
};
var enumEither = function (dictBoundedEnum) {
    return function (dictBoundedEnum1) {
        return new Enum(function () {
            return Data_Either.ordEither((dictBoundedEnum.Enum1()).Ord0())((dictBoundedEnum1.Enum1()).Ord0());
        }, function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Maybe.maybe(Data_Maybe.Nothing.value)(function ($103) {
                    return Data_Maybe.Just.create(Data_Either.Left.create($103));
                })(pred(dictBoundedEnum.Enum1())(v.value0));
            };
            if (v instanceof Data_Either.Right) {
                return Data_Maybe.maybe(new Data_Maybe.Just(new Data_Either.Left(Data_Bounded.top(dictBoundedEnum.Bounded0()))))(function ($104) {
                    return Data_Maybe.Just.create(Data_Either.Right.create($104));
                })(pred(dictBoundedEnum1.Enum1())(v.value0));
            };
            throw new Error("Failed pattern match at Data.Enum (line 86, column 1 - line 90, column 69): " + [ v.constructor.name ]);
        }, function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Maybe.maybe(new Data_Maybe.Just(new Data_Either.Right(Data_Bounded.bottom(dictBoundedEnum1.Bounded0()))))(function ($105) {
                    return Data_Maybe.Just.create(Data_Either.Left.create($105));
                })(succ(dictBoundedEnum.Enum1())(v.value0));
            };
            if (v instanceof Data_Either.Right) {
                return Data_Maybe.maybe(Data_Maybe.Nothing.value)(function ($106) {
                    return Data_Maybe.Just.create(Data_Either.Right.create($106));
                })(succ(dictBoundedEnum1.Enum1())(v.value0));
            };
            throw new Error("Failed pattern match at Data.Enum (line 86, column 1 - line 90, column 69): " + [ v.constructor.name ]);
        });
    };
};
var enumBoolean = new Enum(function () {
    return Data_Ord.ordBoolean;
}, function (v) {
    if (v) {
        return new Data_Maybe.Just(false);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    if (!v) {
        return new Data_Maybe.Just(true);
    };
    return Data_Maybe.Nothing.value;
});
var downFromIncluding = function (dictEnum) {
    return function (dictUnfoldable1) {
        return Data_Unfoldable1.unfoldr1(dictUnfoldable1)(Control_Apply.apply(Control_Apply.applyFn)(Data_Tuple.Tuple.create)(pred(dictEnum)));
    };
};
var diag = function (a) {
    return new Data_Tuple.Tuple(a, a);
};
var downFrom = function (dictEnum) {
    return function (dictUnfoldable) {
        return Data_Unfoldable.unfoldr(dictUnfoldable)((function () {
            var $107 = Data_Functor.map(Data_Maybe.functorMaybe)(diag);
            var $108 = pred(dictEnum);
            return function ($109) {
                return $107($108($109));
            };
        })());
    };
};
var upFrom = function (dictEnum) {
    return function (dictUnfoldable) {
        return Data_Unfoldable.unfoldr(dictUnfoldable)((function () {
            var $110 = Data_Functor.map(Data_Maybe.functorMaybe)(diag);
            var $111 = succ(dictEnum);
            return function ($112) {
                return $110($111($112));
            };
        })());
    };
};
var defaultToEnum = function (dictBounded) {
    return function (dictEnum) {
        return function (i$prime) {
            var go = function ($copy_i) {
                return function ($copy_x) {
                    var $tco_var_i = $copy_i;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(i, x) {
                        var $82 = i === 0;
                        if ($82) {
                            $tco_done = true;
                            return new Data_Maybe.Just(x);
                        };
                        var v = succ(dictEnum)(x);
                        if (v instanceof Data_Maybe.Just) {
                            $tco_var_i = i - 1 | 0;
                            $copy_x = v.value0;
                            return;
                        };
                        if (v instanceof Data_Maybe.Nothing) {
                            $tco_done = true;
                            return Data_Maybe.Nothing.value;
                        };
                        throw new Error("Failed pattern match at Data.Enum (line 293, column 12 - line 295, column 33): " + [ v.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_i, $copy_x);
                    };
                    return $tco_result;
                };
            };
            var $85 = i$prime < 0;
            if ($85) {
                return Data_Maybe.Nothing.value;
            };
            return go(i$prime)(Data_Bounded.bottom(dictBounded));
        };
    };
};
var defaultSucc = function (toEnum$prime) {
    return function (fromEnum$prime) {
        return function (a) {
            return toEnum$prime(fromEnum$prime(a) + 1 | 0);
        };
    };
};
var defaultPred = function (toEnum$prime) {
    return function (fromEnum$prime) {
        return function (a) {
            return toEnum$prime(fromEnum$prime(a) - 1 | 0);
        };
    };
};
var defaultFromEnum = function (dictEnum) {
    var go = function ($copy_i) {
        return function ($copy_x) {
            var $tco_var_i = $copy_i;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(i, x) {
                var v = pred(dictEnum)(x);
                if (v instanceof Data_Maybe.Just) {
                    $tco_var_i = i + 1 | 0;
                    $copy_x = v.value0;
                    return;
                };
                if (v instanceof Data_Maybe.Nothing) {
                    $tco_done = true;
                    return i;
                };
                throw new Error("Failed pattern match at Data.Enum (line 306, column 5 - line 308, column 19): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_i, $copy_x);
            };
            return $tco_result;
        };
    };
    return go(0);
};
var defaultCardinality = function (dictBounded) {
    return function (dictEnum) {
        var go = function ($copy_i) {
            return function ($copy_x) {
                var $tco_var_i = $copy_i;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(i, x) {
                    var v = succ(dictEnum)(x);
                    if (v instanceof Data_Maybe.Just) {
                        $tco_var_i = i + 1 | 0;
                        $copy_x = v.value0;
                        return;
                    };
                    if (v instanceof Data_Maybe.Nothing) {
                        $tco_done = true;
                        return i;
                    };
                    throw new Error("Failed pattern match at Data.Enum (line 273, column 5 - line 275, column 19): " + [ v.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_i, $copy_x);
                };
                return $tco_result;
            };
        };
        return Cardinality(go(1)(Data_Bounded.bottom(dictBounded)));
    };
};
var charToEnum = function (v) {
    if (v >= Data_Bounded.bottom(Data_Bounded.boundedInt) && v <= Data_Bounded.top(Data_Bounded.boundedInt)) {
        return new Data_Maybe.Just($foreign.fromCharCode(v));
    };
    return Data_Maybe.Nothing.value;
};
var enumChar = new Enum(function () {
    return Data_Ord.ordChar;
}, defaultPred(charToEnum)($foreign.toCharCode), defaultSucc(charToEnum)($foreign.toCharCode));
var cardinality = function (dict) {
    return dict.cardinality;
};
var boundedEnumUnit = new BoundedEnum(function () {
    return Data_Bounded.boundedUnit;
}, function () {
    return enumUnit;
}, 1, Data_Function["const"](0), function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(Data_Unit.unit);
    };
    return Data_Maybe.Nothing.value;
});
var boundedEnumOrdering = new BoundedEnum(function () {
    return Data_Bounded.boundedOrdering;
}, function () {
    return enumOrdering;
}, 3, function (v) {
    if (v instanceof Data_Ordering.LT) {
        return 0;
    };
    if (v instanceof Data_Ordering.EQ) {
        return 1;
    };
    if (v instanceof Data_Ordering.GT) {
        return 2;
    };
    throw new Error("Failed pattern match at Data.Enum (line 137, column 1 - line 145, column 18): " + [ v.constructor.name ]);
}, function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(Data_Ordering.LT.value);
    };
    if (v === 1) {
        return new Data_Maybe.Just(Data_Ordering.EQ.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(Data_Ordering.GT.value);
    };
    return Data_Maybe.Nothing.value;
});
var boundedEnumChar = new BoundedEnum(function () {
    return Data_Bounded.boundedChar;
}, function () {
    return enumChar;
}, $foreign.toCharCode(Data_Bounded.top(Data_Bounded.boundedChar)) - $foreign.toCharCode(Data_Bounded.bottom(Data_Bounded.boundedChar)) | 0, $foreign.toCharCode, charToEnum);
var boundedEnumBoolean = new BoundedEnum(function () {
    return Data_Bounded.boundedBoolean;
}, function () {
    return enumBoolean;
}, 2, function (v) {
    if (!v) {
        return 0;
    };
    if (v) {
        return 1;
    };
    throw new Error("Failed pattern match at Data.Enum (line 118, column 1 - line 124, column 20): " + [ v.constructor.name ]);
}, function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(false);
    };
    if (v === 1) {
        return new Data_Maybe.Just(true);
    };
    return Data_Maybe.Nothing.value;
});
module.exports = {
    Enum: Enum,
    succ: succ,
    pred: pred,
    BoundedEnum: BoundedEnum,
    cardinality: cardinality,
    toEnum: toEnum,
    fromEnum: fromEnum,
    toEnumWithDefaults: toEnumWithDefaults,
    Cardinality: Cardinality,
    enumFromTo: enumFromTo,
    enumFromThenTo: enumFromThenTo,
    upFrom: upFrom,
    upFromIncluding: upFromIncluding,
    downFrom: downFrom,
    downFromIncluding: downFromIncluding,
    defaultSucc: defaultSucc,
    defaultPred: defaultPred,
    defaultCardinality: defaultCardinality,
    defaultToEnum: defaultToEnum,
    defaultFromEnum: defaultFromEnum,
    enumBoolean: enumBoolean,
    enumInt: enumInt,
    enumChar: enumChar,
    enumUnit: enumUnit,
    enumOrdering: enumOrdering,
    enumMaybe: enumMaybe,
    enumEither: enumEither,
    enumTuple: enumTuple,
    boundedEnumBoolean: boundedEnumBoolean,
    boundedEnumChar: boundedEnumChar,
    boundedEnumUnit: boundedEnumUnit,
    boundedEnumOrdering: boundedEnumOrdering,
    newtypeCardinality: newtypeCardinality,
    eqCardinality: eqCardinality,
    ordCardinality: ordCardinality,
    showCardinality: showCardinality
};

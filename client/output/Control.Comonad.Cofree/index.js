// Generated by purs version 0.13.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Comonad = require("../Control.Comonad/index.js");
var Control_Extend = require("../Control.Extend/index.js");
var Control_Lazy = require("../Control.Lazy/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Monad_Free = require("../Control.Monad.Free/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Control_Monad_State = require("../Control.Monad.State/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Lazy = require("../Data.Lazy/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Cofree = function (x) {
    return x;
};
var tail = function (v) {
    return Data_Tuple.snd(Data_Lazy.force(v));
};
var mkCofree = function (a) {
    return function (t) {
        return Data_Lazy.defer(function (v) {
            return new Data_Tuple.Tuple(a, t);
        });
    };
};
var lazyCofree = new Control_Lazy.Lazy(function (k) {
    return Data_Lazy.defer(function (v) {
        var v1 = k(Data_Unit.unit);
        return Data_Lazy.force(v1);
    });
});
var hoistCofree = function (dictFunctor) {
    return function (nat) {
        return function (v) {
            return Data_Functor.map(Data_Lazy.functorLazy)(Data_Functor.map(Data_Tuple.functorTuple)((function () {
                var $64 = Data_Functor.map(dictFunctor)(hoistCofree(dictFunctor)(nat));
                return function ($65) {
                    return nat($64($65));
                };
            })()))(v);
        };
    };
};
var head = function (v) {
    return Data_Tuple.fst(Data_Lazy.force(v));
};
var functorCofree = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        var loop = function (v) {
            return Data_Functor.map(Data_Lazy.functorLazy)(function (v1) {
                return new Data_Tuple.Tuple(f(v1.value0), Data_Functor.map(dictFunctor)(loop)(v1.value1));
            })(v);
        };
        return loop;
    });
};
var foldableCofree = function (dictFoldable) {
    return new Data_Foldable.Foldable(function (dictMonoid) {
        return function (f) {
            var go = function (fa) {
                return Data_Semigroup.append(dictMonoid.Semigroup0())(f(head(fa)))(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(go)(tail(fa)));
            };
            return go;
        };
    }, function (f) {
        var go = function (b) {
            return function (fa) {
                return Data_Foldable.foldl(dictFoldable)(go)(f(b)(head(fa)))(tail(fa));
            };
        };
        return go;
    }, function (f) {
        var go = function (fa) {
            return function (b) {
                return f(head(fa))(Data_Foldable.foldr(dictFoldable)(go)(b)(tail(fa)));
            };
        };
        return Data_Function.flip(go);
    });
};
var traversableCofree = function (dictTraversable) {
    return new Data_Traversable.Traversable(function () {
        return foldableCofree(dictTraversable.Foldable1());
    }, function () {
        return functorCofree(dictTraversable.Functor0());
    }, function (dictApplicative) {
        return Data_Traversable.traverse(traversableCofree(dictTraversable))(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
    }, function (dictApplicative) {
        return function (f) {
            var loop = function (ta) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(mkCofree)(f(head(ta))))(Data_Traversable.traverse(dictTraversable)(dictApplicative)(loop)(tail(ta)));
            };
            return loop;
        };
    });
};
var extendCofree = function (dictFunctor) {
    return new Control_Extend.Extend(function () {
        return functorCofree(dictFunctor);
    }, function (f) {
        var loop = function (v) {
            return Data_Functor.map(Data_Lazy.functorLazy)(function (v1) {
                return new Data_Tuple.Tuple(f(v), Data_Functor.map(dictFunctor)(loop)(v1.value1));
            })(v);
        };
        return loop;
    });
};
var eqCofree = function (dictEq1) {
    return function (dictEq) {
        return new Data_Eq.Eq(function (x) {
            return function (y) {
                return Data_Eq.eq(dictEq)(head(x))(head(y)) && Data_Eq.eq1(dictEq1)(eqCofree(dictEq1)(dictEq))(tail(x))(tail(y));
            };
        });
    };
};
var ordCofree = function (dictOrd1) {
    return function (dictOrd) {
        return new Data_Ord.Ord(function () {
            return eqCofree(dictOrd1.Eq10())(dictOrd.Eq0());
        }, function (x) {
            return function (y) {
                var v = Data_Ord.compare(dictOrd)(head(x))(head(y));
                if (v instanceof Data_Ordering.EQ) {
                    return Data_Ord.compare1(dictOrd1)(ordCofree(dictOrd1)(dictOrd))(tail(x))(tail(y));
                };
                return v;
            };
        });
    };
};
var eq1Cofree = function (dictEq1) {
    return new Data_Eq.Eq1(function (dictEq) {
        return Data_Eq.eq(eqCofree(dictEq1)(dictEq));
    });
};
var ord1Cofree = function (dictOrd1) {
    return new Data_Ord.Ord1(function () {
        return eq1Cofree(dictOrd1.Eq10());
    }, function (dictOrd) {
        return Data_Ord.compare(ordCofree(dictOrd1)(dictOrd));
    });
};
var deferCofree = function ($66) {
    return Cofree(Data_Lazy.defer($66));
};
var comonadCofree = function (dictFunctor) {
    return new Control_Comonad.Comonad(function () {
        return extendCofree(dictFunctor);
    }, head);
};
var explore = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (pair) {
            return function (m) {
                return function (w) {
                    var step = function (ff) {
                        return Control_Monad_State_Class.state(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (cof) {
                            return pair(Data_Functor.map(dictFunctor)(Data_Tuple.Tuple.create)(ff))(tail(cof));
                        });
                    };
                    var v = Control_Monad_State.runState(Control_Monad_Free.runFreeM(dictFunctor)(Control_Monad_State_Trans.monadRecStateT(Control_Monad_Rec_Class.monadRecIdentity))(step)(m))(w);
                    return v.value0(Control_Comonad.extract(comonadCofree(dictFunctor1))(v.value1));
                };
            };
        };
    };
};
var exploreM = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictMonadRec) {
            return function (pair) {
                return function (m) {
                    return function (w) {
                        var step = function (ff) {
                            return function (cof) {
                                return pair(Data_Functor.map(dictFunctor)(Data_Tuple.Tuple.create)(ff))(tail(cof));
                            };
                        };
                        var $$eval = function (v) {
                            return v.value0(Control_Comonad.extract(comonadCofree(dictFunctor1))(v.value1));
                        };
                        return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())($$eval)(Control_Monad_State_Trans.runStateT(Control_Monad_Free.runFreeM(dictFunctor)(Control_Monad_State_Trans.monadRecStateT(dictMonadRec))(step)(m))(w));
                    };
                };
            };
        };
    };
};
var buildCofree = function (dictFunctor) {
    return function (k) {
        return function (s) {
            return Data_Lazy.defer(function (v) {
                return Data_Functor.map(Data_Tuple.functorTuple)(Data_Functor.map(dictFunctor)(buildCofree(dictFunctor)(k)))(k(s));
            });
        };
    };
};
var unfoldCofree = function (dictFunctor) {
    return function (e) {
        return function (n) {
            return buildCofree(dictFunctor)(function (s) {
                return new Data_Tuple.Tuple(e(s), n(s));
            });
        };
    };
};
var monadCofree = function (dictAlternative) {
    return new Control_Monad.Monad(function () {
        return applicativeCofree(dictAlternative);
    }, function () {
        return bindCofree(dictAlternative);
    });
};
var bindCofree = function (dictAlternative) {
    return new Control_Bind.Bind(function () {
        return applyCofree(dictAlternative);
    }, function (fa) {
        return function (f) {
            var loop = function (fa$prime) {
                var fh = f(head(fa$prime));
                return mkCofree(head(fh))(Control_Alt.alt((dictAlternative.Plus1()).Alt0())(tail(fh))(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(loop)(tail(fa$prime))));
            };
            return loop(fa);
        };
    });
};
var applyCofree = function (dictAlternative) {
    return new Control_Apply.Apply(function () {
        return functorCofree(((dictAlternative.Plus1()).Alt0()).Functor0());
    }, Control_Monad.ap(monadCofree(dictAlternative)));
};
var applicativeCofree = function (dictAlternative) {
    return new Control_Applicative.Applicative(function () {
        return applyCofree(dictAlternative);
    }, function (a) {
        return mkCofree(a)(Control_Plus.empty(dictAlternative.Plus1()));
    });
};
module.exports = {
    deferCofree: deferCofree,
    mkCofree: mkCofree,
    head: head,
    tail: tail,
    hoistCofree: hoistCofree,
    unfoldCofree: unfoldCofree,
    buildCofree: buildCofree,
    explore: explore,
    exploreM: exploreM,
    eqCofree: eqCofree,
    eq1Cofree: eq1Cofree,
    ordCofree: ordCofree,
    ord1Cofree: ord1Cofree,
    functorCofree: functorCofree,
    foldableCofree: foldableCofree,
    traversableCofree: traversableCofree,
    extendCofree: extendCofree,
    comonadCofree: comonadCofree,
    applyCofree: applyCofree,
    applicativeCofree: applicativeCofree,
    bindCofree: bindCofree,
    monadCofree: monadCofree,
    lazyCofree: lazyCofree
};

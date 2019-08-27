// Generated by purs version 0.13.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class/index.js");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class/index.js");
var Control_MonadPlus = require("../Control.MonadPlus/index.js");
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var WriterT = function (x) {
    return x;
};
var runWriterT = function (v) {
    return v;
};
var newtypeWriterT = new Data_Newtype.Newtype(function (n) {
    return n;
}, WriterT);
var monadTransWriterT = function (dictMonoid) {
    return new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
        return function (m) {
            return Control_Bind.bind(dictMonad.Bind1())(m)(function (v) {
                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v, Data_Monoid.mempty(dictMonoid)));
            });
        };
    });
};
var mapWriterT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorWriterT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return mapWriterT(Data_Functor.map(dictFunctor)(function (v) {
            return new Data_Tuple.Tuple(f(v.value0), v.value1);
        }));
    });
};
var execWriterT = function (dictFunctor) {
    return function (v) {
        return Data_Functor.map(dictFunctor)(Data_Tuple.snd)(v);
    };
};
var applyWriterT = function (dictSemigroup) {
    return function (dictApply) {
        return new Control_Apply.Apply(function () {
            return functorWriterT(dictApply.Functor0());
        }, function (v) {
            return function (v1) {
                var k = function (v3) {
                    return function (v4) {
                        return new Data_Tuple.Tuple(v3.value0(v4.value0), Data_Semigroup.append(dictSemigroup)(v3.value1)(v4.value1));
                    };
                };
                return Control_Apply.apply(dictApply)(Data_Functor.map(dictApply.Functor0())(k)(v))(v1);
            };
        });
    };
};
var bindWriterT = function (dictSemigroup) {
    return function (dictBind) {
        return new Control_Bind.Bind(function () {
            return applyWriterT(dictSemigroup)(dictBind.Apply0());
        }, function (v) {
            return function (k) {
                return WriterT(Control_Bind.bind(dictBind)(v)(function (v1) {
                    var v2 = k(v1.value0);
                    return Data_Functor.map((dictBind.Apply0()).Functor0())(function (v3) {
                        return new Data_Tuple.Tuple(v3.value0, Data_Semigroup.append(dictSemigroup)(v1.value1)(v3.value1));
                    })(v2);
                }));
            };
        });
    };
};
var applicativeWriterT = function (dictMonoid) {
    return function (dictApplicative) {
        return new Control_Applicative.Applicative(function () {
            return applyWriterT(dictMonoid.Semigroup0())(dictApplicative.Apply0());
        }, function (a) {
            return WriterT(Control_Applicative.pure(dictApplicative)(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
        });
    };
};
var monadWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Monad.Monad(function () {
            return applicativeWriterT(dictMonoid)(dictMonad.Applicative0());
        }, function () {
            return bindWriterT(dictMonoid.Semigroup0())(dictMonad.Bind1());
        });
    };
};
var monadAskWriterT = function (dictMonoid) {
    return function (dictMonadAsk) {
        return new Control_Monad_Reader_Class.MonadAsk(function () {
            return monadWriterT(dictMonoid)(dictMonadAsk.Monad0());
        }, Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid))(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)));
    };
};
var monadReaderWriterT = function (dictMonoid) {
    return function (dictMonadReader) {
        return new Control_Monad_Reader_Class.MonadReader(function () {
            return monadAskWriterT(dictMonoid)(dictMonadReader.MonadAsk0());
        }, function (f) {
            return mapWriterT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
        });
    };
};
var monadContWriterT = function (dictMonoid) {
    return function (dictMonadCont) {
        return new Control_Monad_Cont_Class.MonadCont(function () {
            return monadWriterT(dictMonoid)(dictMonadCont.Monad0());
        }, function (f) {
            return WriterT(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                var v = f(function (a) {
                    return WriterT(c(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
                });
                return v;
            }));
        });
    };
};
var monadEffectWriter = function (dictMonoid) {
    return function (dictMonadEffect) {
        return new Effect_Class.MonadEffect(function () {
            return monadWriterT(dictMonoid)(dictMonadEffect.Monad0());
        }, (function () {
            var $123 = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid))(dictMonadEffect.Monad0());
            var $124 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($125) {
                return $123($124($125));
            };
        })());
    };
};
var monadRecWriterT = function (dictMonoid) {
    return function (dictMonadRec) {
        return new Control_Monad_Rec_Class.MonadRec(function () {
            return monadWriterT(dictMonoid)(dictMonadRec.Monad0());
        }, function (f) {
            return function (a) {
                var f$prime = function (v) {
                    var v1 = f(v.value0);
                    return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(v1)(function (v2) {
                        return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())((function () {
                            if (v2.value0 instanceof Control_Monad_Rec_Class.Loop) {
                                return new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(v2.value0.value0, Data_Semigroup.append(dictMonoid.Semigroup0())(v.value1)(v2.value1)));
                            };
                            if (v2.value0 instanceof Control_Monad_Rec_Class.Done) {
                                return new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v2.value0.value0, Data_Semigroup.append(dictMonoid.Semigroup0())(v.value1)(v2.value1)));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Writer.Trans (line 83, column 16 - line 85, column 47): " + [ v2.value0.constructor.name ]);
                        })());
                    });
                };
                return WriterT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(f$prime)(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
            };
        });
    };
};
var monadStateWriterT = function (dictMonoid) {
    return function (dictMonadState) {
        return new Control_Monad_State_Class.MonadState(function () {
            return monadWriterT(dictMonoid)(dictMonadState.Monad0());
        }, function (f) {
            return Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid))(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)(f));
        });
    };
};
var monadTellWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Monad_Writer_Class.MonadTell(function () {
            return monadWriterT(dictMonoid)(dictMonad);
        }, (function () {
            var $126 = Control_Applicative.pure(dictMonad.Applicative0());
            var $127 = Data_Tuple.Tuple.create(Data_Unit.unit);
            return function ($128) {
                return WriterT($126($127($128)));
            };
        })());
    };
};
var monadWriterWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadTellWriterT(dictMonoid)(dictMonad);
        }, function (v) {
            return Control_Bind.bind(dictMonad.Bind1())(v)(function (v1) {
                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v1.value0, v1.value1), v1.value1));
            });
        }, function (v) {
            return Control_Bind.bind(dictMonad.Bind1())(v)(function (v1) {
                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0.value0, v1.value0.value1(v1.value1)));
            });
        });
    };
};
var monadThrowWriterT = function (dictMonoid) {
    return function (dictMonadThrow) {
        return new Control_Monad_Error_Class.MonadThrow(function () {
            return monadWriterT(dictMonoid)(dictMonadThrow.Monad0());
        }, function (e) {
            return Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid))(dictMonadThrow.Monad0())(Control_Monad_Error_Class.throwError(dictMonadThrow)(e));
        });
    };
};
var monadErrorWriterT = function (dictMonoid) {
    return function (dictMonadError) {
        return new Control_Monad_Error_Class.MonadError(function () {
            return monadThrowWriterT(dictMonoid)(dictMonadError.MonadThrow0());
        }, function (v) {
            return function (h) {
                return WriterT(Control_Monad_Error_Class.catchError(dictMonadError)(v)(function (e) {
                    var v1 = h(e);
                    return v1;
                }));
            };
        });
    };
};
var altWriterT = function (dictAlt) {
    return new Control_Alt.Alt(function () {
        return functorWriterT(dictAlt.Functor0());
    }, function (v) {
        return function (v1) {
            return Control_Alt.alt(dictAlt)(v)(v1);
        };
    });
};
var plusWriterT = function (dictPlus) {
    return new Control_Plus.Plus(function () {
        return altWriterT(dictPlus.Alt0());
    }, Control_Plus.empty(dictPlus));
};
var alternativeWriterT = function (dictMonoid) {
    return function (dictAlternative) {
        return new Control_Alternative.Alternative(function () {
            return applicativeWriterT(dictMonoid)(dictAlternative.Applicative0());
        }, function () {
            return plusWriterT(dictAlternative.Plus1());
        });
    };
};
var monadZeroWriterT = function (dictMonoid) {
    return function (dictMonadZero) {
        return new Control_MonadZero.MonadZero(function () {
            return alternativeWriterT(dictMonoid)(dictMonadZero.Alternative1());
        }, function () {
            return monadWriterT(dictMonoid)(dictMonadZero.Monad0());
        });
    };
};
var monadPlusWriterT = function (dictMonoid) {
    return function (dictMonadPlus) {
        return new Control_MonadPlus.MonadPlus(function () {
            return monadZeroWriterT(dictMonoid)(dictMonadPlus.MonadZero0());
        });
    };
};
module.exports = {
    WriterT: WriterT,
    runWriterT: runWriterT,
    execWriterT: execWriterT,
    mapWriterT: mapWriterT,
    newtypeWriterT: newtypeWriterT,
    functorWriterT: functorWriterT,
    applyWriterT: applyWriterT,
    applicativeWriterT: applicativeWriterT,
    altWriterT: altWriterT,
    plusWriterT: plusWriterT,
    alternativeWriterT: alternativeWriterT,
    bindWriterT: bindWriterT,
    monadWriterT: monadWriterT,
    monadRecWriterT: monadRecWriterT,
    monadZeroWriterT: monadZeroWriterT,
    monadPlusWriterT: monadPlusWriterT,
    monadTransWriterT: monadTransWriterT,
    monadEffectWriter: monadEffectWriter,
    monadContWriterT: monadContWriterT,
    monadThrowWriterT: monadThrowWriterT,
    monadErrorWriterT: monadErrorWriterT,
    monadAskWriterT: monadAskWriterT,
    monadReaderWriterT: monadReaderWriterT,
    monadStateWriterT: monadStateWriterT,
    monadTellWriterT: monadTellWriterT,
    monadWriterWriterT: monadWriterWriterT
};

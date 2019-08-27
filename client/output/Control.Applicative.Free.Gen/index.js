// Generated by purs version 0.13.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Applicative_Free = require("../Control.Applicative.Free/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Gen = require("../Control.Monad.Gen/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_NonEmpty = require("../Data.NonEmpty/index.js");
var genFree = function (dictMonadGen) {
    return function (dictMonadRec) {
        return function (genF) {
            return function (genA) {
                return function (genA2A) {
                    return Control_Monad_Gen.oneOf(dictMonadGen)(Data_NonEmpty.foldable1NonEmpty(Data_Foldable.foldableArray))(new Data_NonEmpty.NonEmpty(Data_Functor.mapFlipped((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(genA)(Control_Applicative.pure(Control_Applicative_Free.applicativeFreeAp)), [ Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genF)(function (v) {
                        return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genA)(function (v1) {
                            return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(Control_Apply.apply(Control_Applicative_Free.applyFreeAp)(Control_Applicative.pure(Control_Applicative_Free.applicativeFreeAp)(Data_Function["const"](v1)))(Control_Applicative_Free.liftFreeAp(v)));
                        });
                    }), Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genF)(function (v) {
                        return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genA)(function (v1) {
                            return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genA2A)(function (v2) {
                                return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(Data_Functor.mapFlipped(Control_Applicative_Free.functorFreeAp)(Control_Apply.apply(Control_Applicative_Free.applyFreeAp)(Control_Applicative.pure(Control_Applicative_Free.applicativeFreeAp)(Data_Function["const"](v1)))(Control_Applicative_Free.liftFreeAp(v)))(v2));
                            });
                        });
                    }), Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genF)(function (v) {
                        return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genA)(function (v1) {
                            return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genA2A)(function (v2) {
                                return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(Data_Functor.mapFlipped(Control_Applicative_Free.functorFreeAp)(Data_Functor.mapFlipped(Control_Applicative_Free.functorFreeAp)(Control_Applicative_Free.liftFreeAp(v))(Data_Function["const"](v1)))(v2));
                            });
                        });
                    }), Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genA)(function (v) {
                        return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genA2A)(function (v1) {
                            return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(Data_Functor.mapFlipped(Control_Applicative_Free.functorFreeAp)(Control_Applicative.pure(Control_Applicative_Free.applicativeFreeAp)(v))(v1));
                        });
                    }) ]));
                };
            };
        };
    };
};
module.exports = {
    genFree: genFree
};

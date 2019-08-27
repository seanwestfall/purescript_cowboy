// Generated by purs version 0.13.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Show = require("../Data.Show/index.js");
var toNullable = Data_Maybe.maybe($foreign["null"])($foreign.notNull);
var toMaybe = function (n) {
    return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
};
var showNullable = function (dictShow) {
    return new Data_Show.Show((function () {
        var $5 = Data_Maybe.maybe("null")(Data_Show.show(dictShow));
        return function ($6) {
            return $5(toMaybe($6));
        };
    })());
};
var eqNullable = function (dictEq) {
    return new Data_Eq.Eq(Data_Function.on(Data_Eq.eq(Data_Maybe.eqMaybe(dictEq)))(toMaybe));
};
var ordNullable = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqNullable(dictOrd.Eq0());
    }, Data_Function.on(Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd)))(toMaybe));
};
var eq1Nullable = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqNullable(dictEq));
});
var ord1Nullable = new Data_Ord.Ord1(function () {
    return eq1Nullable;
}, function (dictOrd) {
    return Data_Ord.compare(ordNullable(dictOrd));
});
module.exports = {
    toMaybe: toMaybe,
    toNullable: toNullable,
    showNullable: showNullable,
    eqNullable: eqNullable,
    eq1Nullable: eq1Nullable,
    ordNullable: ordNullable,
    ord1Nullable: ord1Nullable,
    "null": $foreign["null"],
    notNull: $foreign.notNull
};

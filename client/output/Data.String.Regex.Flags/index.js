// Generated by purs version 0.13.2
"use strict";
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_String_Common = require("../Data.String.Common/index.js");
var RegexFlags = (function () {
    function RegexFlags(value0) {
        this.value0 = value0;
    };
    RegexFlags.create = function (value0) {
        return new RegexFlags(value0);
    };
    return RegexFlags;
})();
var unicode = new RegexFlags({
    global: false,
    ignoreCase: false,
    multiline: false,
    sticky: false,
    unicode: true
});
var sticky = new RegexFlags({
    global: false,
    ignoreCase: false,
    multiline: false,
    sticky: true,
    unicode: false
});
var showRegexFlags = new Data_Show.Show(function (v) {
    var usedFlags = Data_Semigroup.append(Data_Semigroup.semigroupArray)([  ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.global))("global"))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.ignoreCase))("ignoreCase"))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.multiline))("multiline"))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.sticky))("sticky"))(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.unicode))("unicode"))))));
    var $6 = Data_Eq.eq(Data_Eq.eqArray(Data_Eq.eqString))(usedFlags)([  ]);
    if ($6) {
        return "noFlags";
    };
    return "(" + (Data_String_Common.joinWith(" <> ")(usedFlags) + ")");
});
var semigroupRegexFlags = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return new RegexFlags({
            global: v.value0.global || v1.value0.global,
            ignoreCase: v.value0.ignoreCase || v1.value0.ignoreCase,
            multiline: v.value0.multiline || v1.value0.multiline,
            sticky: v.value0.sticky || v1.value0.sticky,
            unicode: v.value0.unicode || v1.value0.unicode
        });
    };
});
var noFlags = new RegexFlags({
    global: false,
    ignoreCase: false,
    multiline: false,
    sticky: false,
    unicode: false
});
var multiline = new RegexFlags({
    global: false,
    ignoreCase: false,
    multiline: true,
    sticky: false,
    unicode: false
});
var monoidRegexFlags = new Data_Monoid.Monoid(function () {
    return semigroupRegexFlags;
}, noFlags);
var ignoreCase = new RegexFlags({
    global: false,
    ignoreCase: true,
    multiline: false,
    sticky: false,
    unicode: false
});
var global = new RegexFlags({
    global: true,
    ignoreCase: false,
    multiline: false,
    sticky: false,
    unicode: false
});
var eqRegexFlags = new Data_Eq.Eq(function (v) {
    return function (v1) {
        return v.value0.global === v1.value0.global && (v.value0.ignoreCase === v1.value0.ignoreCase && (v.value0.multiline === v1.value0.multiline && (v.value0.sticky === v1.value0.sticky && v.value0.unicode === v1.value0.unicode)));
    };
});
module.exports = {
    RegexFlags: RegexFlags,
    noFlags: noFlags,
    global: global,
    ignoreCase: ignoreCase,
    multiline: multiline,
    sticky: sticky,
    unicode: unicode,
    semigroupRegexFlags: semigroupRegexFlags,
    monoidRegexFlags: monoidRegexFlags,
    eqRegexFlags: eqRegexFlags,
    showRegexFlags: showRegexFlags
};

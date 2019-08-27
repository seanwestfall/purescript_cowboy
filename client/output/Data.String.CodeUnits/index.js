// Generated by purs version 0.13.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_String_Unsafe = require("../Data.String.Unsafe/index.js");
var uncons = function (v) {
    if (v === "") {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just({
        head: Data_String_Unsafe.charAt(0)(v),
        tail: $foreign.drop(1)(v)
    });
};
var toChar = $foreign["_toChar"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var takeWhile = function (p) {
    return function (s) {
        return $foreign.take($foreign.countPrefix(p)(s))(s);
    };
};
var takeRight = function (i) {
    return function (s) {
        return $foreign.drop($foreign.length(s) - i | 0)(s);
    };
};
var slice = function (b) {
    return function (e) {
        return function (s) {
            var l = $foreign.length(s);
            var norm = function (x) {
                if (x < 0) {
                    return l + x | 0;
                };
                if (Data_Boolean.otherwise) {
                    return x;
                };
                throw new Error("Failed pattern match at Data.String.CodeUnits (line 314, column 5 - line 315, column 27): " + [ x.constructor.name ]);
            };
            var e$prime = norm(e);
            var b$prime = norm(b);
            var $7 = b$prime < 0 || (b$prime >= l || (e$prime < 0 || (e$prime > l || b$prime > e$prime)));
            if ($7) {
                return Data_Maybe.Nothing.value;
            };
            return new Data_Maybe.Just($foreign["_slice"](b)(e)(s));
        };
    };
};
var lastIndexOf$prime = $foreign["_lastIndexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var lastIndexOf = $foreign["_lastIndexOf"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripSuffix = function (v) {
    return function (str) {
        var v1 = lastIndexOf(v)(str);
        if (v1 instanceof Data_Maybe.Just && v1.value0 === ($foreign.length(str) - $foreign.length(v) | 0)) {
            return Data_Maybe.Just.create($foreign.take(v1.value0)(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var indexOf$prime = $foreign["_indexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var indexOf = $foreign["_indexOf"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripPrefix = function (v) {
    return function (str) {
        var v1 = indexOf(v)(str);
        if (v1 instanceof Data_Maybe.Just && v1.value0 === 0) {
            return Data_Maybe.Just.create($foreign.drop($foreign.length(v))(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var dropWhile = function (p) {
    return function (s) {
        return $foreign.drop($foreign.countPrefix(p)(s))(s);
    };
};
var dropRight = function (i) {
    return function (s) {
        return $foreign.take($foreign.length(s) - i | 0)(s);
    };
};
var contains = function (pat) {
    var $16 = indexOf(pat);
    return function ($17) {
        return Data_Maybe.isJust($16($17));
    };
};
var charAt = $foreign["_charAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
module.exports = {
    stripPrefix: stripPrefix,
    stripSuffix: stripSuffix,
    contains: contains,
    charAt: charAt,
    toChar: toChar,
    uncons: uncons,
    indexOf: indexOf,
    "indexOf'": indexOf$prime,
    lastIndexOf: lastIndexOf,
    "lastIndexOf'": lastIndexOf$prime,
    takeRight: takeRight,
    takeWhile: takeWhile,
    dropRight: dropRight,
    dropWhile: dropWhile,
    slice: slice,
    singleton: $foreign.singleton,
    fromCharArray: $foreign.fromCharArray,
    toCharArray: $foreign.toCharArray,
    length: $foreign.length,
    countPrefix: $foreign.countPrefix,
    take: $foreign.take,
    drop: $foreign.drop,
    splitAt: $foreign.splitAt
};
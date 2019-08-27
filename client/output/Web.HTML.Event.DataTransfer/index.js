// Generated by purs version 0.13.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Effect = require("../Effect/index.js");
var Copy = (function () {
    function Copy() {

    };
    Copy.value = new Copy();
    return Copy;
})();
var Link = (function () {
    function Link() {

    };
    Link.value = new Link();
    return Link;
})();
var Move = (function () {
    function Move() {

    };
    Move.value = new Move();
    return Move;
})();
var None = (function () {
    function None() {

    };
    None.value = new None();
    return None;
})();
var setDropEffect = function (de) {
    return $foreign["_setDropEffect"]((function () {
        if (de instanceof Copy) {
            return "copy";
        };
        if (de instanceof Link) {
            return "link";
        };
        if (de instanceof Move) {
            return "move";
        };
        if (de instanceof None) {
            return "none";
        };
        throw new Error("Failed pattern match at Web.HTML.Event.DataTransfer (line 81, column 35 - line 85, column 17): " + [ de.constructor.name ]);
    })());
};
var setData = function (v) {
    return function (dat) {
        return function (dt) {
            return $foreign["_setData"](v)(dat)(dt);
        };
    };
};
var getData = function (v) {
    return function (dt) {
        return $foreign["_getData"](v)(dt);
    };
};
var files = Data_Functor.map(Data_Functor.functorFn)(Data_Nullable.toMaybe)($foreign["_files"]);
var eqDropEffect = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Copy && y instanceof Copy) {
            return true;
        };
        if (x instanceof Link && y instanceof Link) {
            return true;
        };
        if (x instanceof Move && y instanceof Move) {
            return true;
        };
        if (x instanceof None && y instanceof None) {
            return true;
        };
        return false;
    };
});
var ordDropEffect = new Data_Ord.Ord(function () {
    return eqDropEffect;
}, function (x) {
    return function (y) {
        if (x instanceof Copy && y instanceof Copy) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Copy) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Copy) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Link && y instanceof Link) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Link) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Link) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Move && y instanceof Move) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Move) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Move) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof None && y instanceof None) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at Web.HTML.Event.DataTransfer (line 65, column 1 - line 65, column 48): " + [ x.constructor.name, y.constructor.name ]);
    };
});
var dropEffect = function (dt) {
    return Data_Functor.mapFlipped(Effect.functorEffect)($foreign["_dropEffect"](dt))(function (v) {
        if (v === "copy") {
            return Copy.value;
        };
        if (v === "link") {
            return Link.value;
        };
        if (v === "move") {
            return Move.value;
        };
        if (v === "none") {
            return None.value;
        };
        return None.value;
    });
};
module.exports = {
    files: files,
    getData: getData,
    setData: setData,
    Copy: Copy,
    Link: Link,
    Move: Move,
    None: None,
    dropEffect: dropEffect,
    setDropEffect: setDropEffect,
    eqDropEffect: eqDropEffect,
    ordDropEffect: ordDropEffect,
    types: $foreign.types
};

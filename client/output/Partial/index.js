// Generated by purs version 0.13.2
"use strict";
var $foreign = require("./foreign.js");
var crash = function (dictPartial) {
    return $foreign.crashWith(dictPartial)("Partial.crash: partial function");
};
module.exports = {
    crash: crash,
    crashWith: $foreign.crashWith
};

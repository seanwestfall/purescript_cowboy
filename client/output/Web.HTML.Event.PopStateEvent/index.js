// Generated by purs version 0.13.2
"use strict";
var $foreign = require("./foreign.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toEvent = Unsafe_Coerce.unsafeCoerce;
var fromEvent = Web_Internal_FFI.unsafeReadProtoTagged("PopStateEvent");
module.exports = {
    fromEvent: fromEvent,
    toEvent: toEvent,
    state: $foreign.state
};
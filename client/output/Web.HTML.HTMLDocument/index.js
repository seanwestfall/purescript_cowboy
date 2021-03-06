// Generated by purs version 0.13.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Effect = require("../Effect/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_HTML_HTMLDocument_ReadyState = require("../Web.HTML.HTMLDocument.ReadyState/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toParentNode = Unsafe_Coerce.unsafeCoerce;
var toNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var toNode = Unsafe_Coerce.unsafeCoerce;
var toEventTarget = Unsafe_Coerce.unsafeCoerce;
var toDocument = Unsafe_Coerce.unsafeCoerce;
var readyState = (function () {
    var $0 = Data_Functor.map(Effect.functorEffect)((function () {
        var $2 = Data_Maybe.fromMaybe(Web_HTML_HTMLDocument_ReadyState.Loading.value);
        return function ($3) {
            return $2(Web_HTML_HTMLDocument_ReadyState.parse($3));
        };
    })());
    return function ($1) {
        return $0($foreign["_readyState"]($1));
    };
})();
var fromParentNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLDocument");
var fromNonElementParentNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLDocument");
var fromNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLDocument");
var fromEventTarget = Web_Internal_FFI.unsafeReadProtoTagged("HTMLDocument");
var fromDocument = Web_Internal_FFI.unsafeReadProtoTagged("HTMLDocument");
var currentScript = (function () {
    var $4 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($5) {
        return $4($foreign["_currentScript"]($5));
    };
})();
var body = (function () {
    var $6 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($7) {
        return $6($foreign["_body"]($7));
    };
})();
var activeElement = (function () {
    var $8 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($9) {
        return $8($foreign["_activeElement"]($9));
    };
})();
module.exports = {
    fromDocument: fromDocument,
    fromNode: fromNode,
    fromParentNode: fromParentNode,
    fromNonElementParentNode: fromNonElementParentNode,
    fromEventTarget: fromEventTarget,
    toDocument: toDocument,
    toNode: toNode,
    toParentNode: toParentNode,
    toNonElementParentNode: toNonElementParentNode,
    toEventTarget: toEventTarget,
    body: body,
    readyState: readyState,
    activeElement: activeElement,
    currentScript: currentScript,
    referrer: $foreign.referrer,
    title: $foreign.title,
    setTitle: $foreign.setTitle
};

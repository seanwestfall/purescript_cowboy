// Generated by purs version 0.13.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Effect = require("../Effect/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toParentNode = Unsafe_Coerce.unsafeCoerce;
var toNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var toNode = Unsafe_Coerce.unsafeCoerce;
var toHTMLElement = Unsafe_Coerce.unsafeCoerce;
var toEventTarget = Unsafe_Coerce.unsafeCoerce;
var toElement = Unsafe_Coerce.unsafeCoerce;
var toChildNode = Unsafe_Coerce.unsafeCoerce;
var fromParentNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLButtonElement");
var fromNonDocumentTypeChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLButtonElement");
var fromNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLButtonElement");
var fromHTMLElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLButtonElement");
var fromEventTarget = Web_Internal_FFI.unsafeReadProtoTagged("HTMLButtonElement");
var fromElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLButtonElement");
var fromChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLButtonElement");
var form = (function () {
    var $0 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($1) {
        return $0($foreign["_form"]($1));
    };
})();
module.exports = {
    fromHTMLElement: fromHTMLElement,
    fromElement: fromElement,
    fromNode: fromNode,
    fromChildNode: fromChildNode,
    fromNonDocumentTypeChildNode: fromNonDocumentTypeChildNode,
    fromParentNode: fromParentNode,
    fromEventTarget: fromEventTarget,
    toHTMLElement: toHTMLElement,
    toElement: toElement,
    toNode: toNode,
    toChildNode: toChildNode,
    toNonDocumentTypeChildNode: toNonDocumentTypeChildNode,
    toParentNode: toParentNode,
    toEventTarget: toEventTarget,
    form: form,
    autofocus: $foreign.autofocus,
    setAutofocus: $foreign.setAutofocus,
    disabled: $foreign.disabled,
    setDisabled: $foreign.setDisabled,
    formAction: $foreign.formAction,
    setFormAction: $foreign.setFormAction,
    formEnctype: $foreign.formEnctype,
    setFormEnctype: $foreign.setFormEnctype,
    formMethod: $foreign.formMethod,
    setFormMethod: $foreign.setFormMethod,
    formNoValidate: $foreign.formNoValidate,
    setFormNoValidate: $foreign.setFormNoValidate,
    formTarget: $foreign.formTarget,
    setFormTarget: $foreign.setFormTarget,
    name: $foreign.name,
    setName: $foreign.setName,
    type_: $foreign.type_,
    setType: $foreign.setType,
    value: $foreign.value,
    setValue: $foreign.setValue,
    willValidate: $foreign.willValidate,
    validity: $foreign.validity,
    validationMessage: $foreign.validationMessage,
    checkValidity: $foreign.checkValidity,
    reportValidity: $foreign.reportValidity,
    setCustomValidity: $foreign.setCustomValidity,
    labels: $foreign.labels
};

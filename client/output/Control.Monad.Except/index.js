// Generated by purs version 0.13.2
"use strict";
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var withExcept = Control_Monad_Except_Trans.withExceptT(Data_Identity.functorIdentity);
var runExcept = (function () {
    var $0 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
    return function ($1) {
        return $0(Control_Monad_Except_Trans.runExceptT($1));
    };
})();
var mapExcept = function (f) {
    return Control_Monad_Except_Trans.mapExceptT((function () {
        var $2 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
        return function ($3) {
            return Data_Identity.Identity(f($2($3)));
        };
    })());
};
module.exports = {
    runExcept: runExcept,
    mapExcept: mapExcept,
    withExcept: withExcept
};

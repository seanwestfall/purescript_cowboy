// Generated by purs version 0.13.2
"use strict";
var Costrong = function (Profunctor0, unfirst, unsecond) {
    this.Profunctor0 = Profunctor0;
    this.unfirst = unfirst;
    this.unsecond = unsecond;
};
var unsecond = function (dict) {
    return dict.unsecond;
};
var unfirst = function (dict) {
    return dict.unfirst;
};
module.exports = {
    unfirst: unfirst,
    unsecond: unsecond,
    Costrong: Costrong
};

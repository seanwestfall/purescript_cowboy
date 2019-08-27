// Generated by purs version 0.13.2
"use strict";
var MenuList = (function () {
    function MenuList() {

    };
    MenuList.value = new MenuList();
    return MenuList;
})();
var MenuContext = (function () {
    function MenuContext() {

    };
    MenuContext.value = new MenuContext();
    return MenuContext;
})();
var MenuToolbar = (function () {
    function MenuToolbar() {

    };
    MenuToolbar.value = new MenuToolbar();
    return MenuToolbar;
})();
var renderMenuType = function (v) {
    if (v instanceof MenuList) {
        return "list";
    };
    if (v instanceof MenuContext) {
        return "context";
    };
    if (v instanceof MenuToolbar) {
        return "toolbar";
    };
    throw new Error("Failed pattern match at DOM.HTML.Indexed.MenuType (line 9, column 18 - line 12, column 27): " + [ v.constructor.name ]);
};
module.exports = {
    MenuList: MenuList,
    MenuContext: MenuContext,
    MenuToolbar: MenuToolbar,
    renderMenuType: renderMenuType
};

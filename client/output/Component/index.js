// Generated by purs version 0.13.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Aff_Class = require("../Effect.Aff.Class/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Console = require("../Effect.Console/index.js");
var Halogen_Component = require("../Halogen.Component/index.js");
var Halogen_HTML_Core = require("../Halogen.HTML.Core/index.js");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements/index.js");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties/index.js");
var Halogen_Query_EventSource = require("../Halogen.Query.EventSource/index.js");
var Halogen_Query_HalogenM = require("../Halogen.Query.HalogenM/index.js");
var Web_Event_Event = require("../Web.Event.Event/index.js");
var Web_HTML = require("../Web.HTML/index.js");
var Web_HTML_HTMLDocument = require("../Web.HTML.HTMLDocument/index.js");
var Web_HTML_Window = require("../Web.HTML.Window/index.js");
var Web_UIEvent_KeyboardEvent = require("../Web.UIEvent.KeyboardEvent/index.js");
var Web_UIEvent_KeyboardEvent_EventTypes = require("../Web.UIEvent.KeyboardEvent.EventTypes/index.js");
var Init = (function () {
    function Init() {

    };
    Init.value = new Init();
    return Init;
})();
var HandleKey = (function () {
    function HandleKey(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    HandleKey.create = function (value0) {
        return function (value1) {
            return new HandleKey(value0, value1);
        };
    };
    return HandleKey;
})();
var ConsoleLog = (function () {
    function ConsoleLog() {

    };
    ConsoleLog.value = new ConsoleLog();
    return ConsoleLog;
})();
var initialState = {
    on: false,
    chars: "",
    buttonClassName: "button-not-changed",
    spritePosition: 0,
    spriteStyle: "position: absolute; left: 0;",
    innerStyle: "display: block; position: relative; margin-left: 20px; margin-right: 20px;",
    spriteHero: "sprite-hero-0",
    spriteReverse: false,
    spriteIndex: 0
};
var handleAction = function (v) {
    if (v instanceof Init) {
        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(Effect_Aff.monadEffectAff))(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.document)(Web_HTML.window)))(function (v1) {
            return Halogen_Query_HalogenM["subscribe'"](function (sid) {
                return Halogen_Query_EventSource.eventListenerEventSource(Effect_Aff_Class.monadAffAff)(Web_UIEvent_KeyboardEvent_EventTypes.keyup)(Web_HTML_HTMLDocument.toEventTarget(v1))((function () {
                    var $14 = Data_Functor.map(Data_Maybe.functorMaybe)(HandleKey.create(sid));
                    return function ($15) {
                        return $14(Web_UIEvent_KeyboardEvent.fromEvent($15));
                    };
                })());
            });
        });
    };
    if (v instanceof HandleKey) {
        if (Web_UIEvent_KeyboardEvent.shiftKey(v.value1)) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(Effect_Aff.monadEffectAff))(Web_Event_Event.preventDefault(Web_UIEvent_KeyboardEvent.toEvent(v.value1))))(function () {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (state) {
                    return {
                        on: !state.on,
                        chars: state.chars,
                        buttonClassName: "button-changed",
                        spriteStyle: state.spriteStyle,
                        spritePosition: state.spritePosition,
                        innerStyle: state.innerStyle,
                        spriteIndex: state.spriteIndex,
                        spriteReverse: state.spriteReverse,
                        spriteHero: "sprite-hero-gun" + (function () {
                            if (state.spriteReverse) {
                                return "r";
                            };
                            return "";
                        })()
                    };
                }))(function (v1) {
                    return Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(Effect_Aff.monadEffectAff))(Effect_Console.log("shiftKey pressed!"));
                });
            });
        };
        if (Web_UIEvent_KeyboardEvent.code(v.value1) === "ArrowLeft") {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (state) {
                return {
                    on: !state.on,
                    chars: state.chars,
                    buttonClassName: "button-changed",
                    spriteStyle: "position: absolute; left: " + (Data_Show.show(Data_Show.showInt)(state.spritePosition) + "px;"),
                    spritePosition: state.spritePosition - 10 | 0,
                    innerStyle: state.innerStyle,
                    spriteIndex: (function () {
                        var $10 = state.spriteIndex !== 3;
                        if ($10) {
                            return state.spriteIndex + 1 | 0;
                        };
                        return 0;
                    })(),
                    spriteReverse: true,
                    spriteHero: "sprite-hero-" + (Data_Show.show(Data_Show.showInt)(state.spriteIndex) + "r")
                };
            }))(function (v1) {
                return Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(Effect_Aff.monadEffectAff))(Effect_Console.log("Arrow Left Button Clicked!"));
            });
        };
        if (Web_UIEvent_KeyboardEvent.code(v.value1) === "ArrowRight") {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (state) {
                return {
                    on: !state.on,
                    chars: state.chars,
                    buttonClassName: "button-changed",
                    spriteStyle: "position: absolute; left: " + (Data_Show.show(Data_Show.showInt)(state.spritePosition) + "px;"),
                    spritePosition: state.spritePosition + 10 | 0,
                    innerStyle: state.innerStyle,
                    spriteIndex: (function () {
                        var $11 = state.spriteIndex !== 3;
                        if ($11) {
                            return state.spriteIndex + 1 | 0;
                        };
                        return 0;
                    })(),
                    spriteReverse: false,
                    spriteHero: "sprite-hero-" + Data_Show.show(Data_Show.showInt)(state.spriteIndex)
                };
            }))(function (v1) {
                return Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(Effect_Aff.monadEffectAff))(Effect_Console.log("Arrow Right Button Clicked!"));
            });
        };
        if (Data_Boolean.otherwise) {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
        };
    };
    if (v instanceof ConsoleLog) {
        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (state) {
            return {
                on: !state.on,
                chars: state.chars,
                buttonClassName: "button-changed",
                spriteStyle: "position: absolute; left: 0;",
                spritePosition: 0,
                innerStyle: state.innerStyle,
                spriteIndex: state.spriteIndex,
                spriteReverse: state.spriteReverse,
                spriteHero: state.spriteHero
            };
        }))(function (v1) {
            return Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(Effect_Aff.monadEffectAff))(Effect_Console.log("Button Clicked!"));
        });
    };
    throw new Error("Failed pattern match at Component (line 86, column 16 - line 145, column 41): " + [ v.constructor.name ]);
};
var component = (function () {
    var render = function (state) {
        return Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.h1_([ Halogen_HTML_Core.text("Hello Cowboy") ]), Halogen_HTML_Elements.div([ Halogen_HTML_Properties.class_("inner"), Halogen_HTML_Properties.attr("style")(state.innerStyle) ])([ Halogen_HTML_Elements.div([ Halogen_HTML_Properties.class_(state.spriteHero), Halogen_HTML_Properties.attr("style")(state.spriteStyle) ])([  ]) ]) ]);
    };
    return Halogen_Component.mkComponent({
        initialState: Data_Function["const"](initialState),
        render: render,
        "eval": Halogen_Component.mkEval({
            handleAction: handleAction,
            handleQuery: Halogen_Component.defaultEval.handleQuery,
            receive: Halogen_Component.defaultEval.receive,
            initialize: new Data_Maybe.Just(Init.value),
            finalize: Halogen_Component.defaultEval.finalize
        })
    });
})();
module.exports = {
    Init: Init,
    HandleKey: HandleKey,
    ConsoleLog: ConsoleLog,
    initialState: initialState,
    component: component,
    handleAction: handleAction
};
// Generated by purs version 0.13.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var Halogen_Aff_Driver = require("../Halogen.Aff.Driver/index.js");
var Halogen_Aff_Driver_State = require("../Halogen.Aff.Driver.State/index.js");
var Halogen_Component = require("../Halogen.Component/index.js");
var Halogen_HTML_Core = require("../Halogen.HTML.Core/index.js");
var Halogen_VDom_DOM = require("../Halogen.VDom.DOM/index.js");
var Halogen_VDom_DOM_Prop = require("../Halogen.VDom.DOM.Prop/index.js");
var Halogen_VDom_Machine = require("../Halogen.VDom.Machine/index.js");
var Halogen_VDom_Thunk = require("../Halogen.VDom.Thunk/index.js");
var Unsafe_Reference = require("../Unsafe.Reference/index.js");
var Web_DOM_Node = require("../Web.DOM.Node/index.js");
var Web_HTML = require("../Web.HTML/index.js");
var Web_HTML_HTMLDocument = require("../Web.HTML.HTMLDocument/index.js");
var Web_HTML_HTMLElement = require("../Web.HTML.HTMLElement/index.js");
var Web_HTML_Window = require("../Web.HTML.Window/index.js");
var RenderState = function (x) {
    return x;
};
var substInParent = function (v) {
    return function (v1) {
        return function (v2) {
            if (v1 instanceof Data_Maybe.Just && v2 instanceof Data_Maybe.Just) {
                return Data_Functor["void"](Effect.functorEffect)(Web_DOM_Node.insertBefore(v)(v1.value0)(v2.value0));
            };
            if (v1 instanceof Data_Maybe.Nothing && v2 instanceof Data_Maybe.Just) {
                return Data_Functor["void"](Effect.functorEffect)(Web_DOM_Node.appendChild(v)(v2.value0));
            };
            return Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit);
        };
    };
};
var removeChild = function (v) {
    return function __do() {
        var v1 = Web_DOM_Node.parentNode(v.node)();
        return Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(function (pn) {
            return Web_DOM_Node.removeChild(v.node)(pn);
        })(v1)();
    };
};
var mkSpec = function (handler) {
    return function (renderChildRef) {
        return function (document) {
            var getNode = Halogen_Aff_Driver_State.unRenderStateX(function (v) {
                return v.node;
            });
            var done = function (st) {
                if (st instanceof Data_Maybe.Just) {
                    return Halogen_VDom_Machine.halt(st.value0);
                };
                return Data_Unit.unit;
            };
            var buildWidget = function (spec) {
                var buildThunk = Halogen_VDom_Thunk.buildThunk(Data_Newtype.unwrap(Halogen_HTML_Core.newtypeHTML))(spec);
                var renderComponentSlot = function (cs) {
                    var v = Effect_Ref.read(renderChildRef)();
                    var v1 = v(cs)();
                    var node = getNode(v1);
                    return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(node, Data_Maybe.Nothing.value, patch, done));
                };
                var render = function (slot) {
                    if (slot instanceof Halogen_Component.ComponentSlot) {
                        return renderComponentSlot(slot.value0);
                    };
                    if (slot instanceof Halogen_Component.ThunkSlot) {
                        var v = buildThunk(slot.value0);
                        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(v), new Data_Maybe.Just(v), patch, done));
                    };
                    throw new Error("Failed pattern match at Halogen.VDom.Driver (line 85, column 7 - line 90, column 75): " + [ slot.constructor.name ]);
                };
                var patch = function (st, slot) {
                    if (st instanceof Data_Maybe.Just) {
                        if (slot instanceof Halogen_Component.ComponentSlot) {
                            Halogen_VDom_Machine.halt(st.value0);
                            return renderComponentSlot(slot.value0);
                        };
                        if (slot instanceof Halogen_Component.ThunkSlot) {
                            var v = Halogen_VDom_Machine.step(st.value0, slot.value0);
                            return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(v), new Data_Maybe.Just(v), patch, done));
                        };
                        throw new Error("Failed pattern match at Halogen.VDom.Driver (line 98, column 22 - line 104, column 79): " + [ slot.constructor.name ]);
                    };
                    return render(slot);
                };
                return render;
            };
            var buildAttributes = Halogen_VDom_DOM_Prop.buildProp(handler);
            return {
                buildWidget: buildWidget,
                buildAttributes: buildAttributes,
                document: document
            };
        };
    };
};
var renderSpec = function (document) {
    return function (container) {
        var render = function (handler) {
            return function (child) {
                return function (v) {
                    return function (v1) {
                        if (v1 instanceof Data_Maybe.Nothing) {
                            return function __do() {
                                var v2 = Effect_Ref["new"](child)();
                                var spec = mkSpec(handler)(v2)(document);
                                var v3 = Halogen_VDom_DOM.buildVDom(spec)(v);
                                var node = Halogen_VDom_Machine.extract(v3);
                                Data_Functor["void"](Effect.functorEffect)(Web_DOM_Node.appendChild(node)(Web_HTML_HTMLElement.toNode(container)))();
                                return {
                                    machine: v3,
                                    node: node,
                                    renderChildRef: v2
                                };
                            };
                        };
                        if (v1 instanceof Data_Maybe.Just) {
                            return function __do() {
                                Effect_Ref.write(child)(v1.value0.renderChildRef)();
                                var v2 = Web_DOM_Node.parentNode(v1.value0.node)();
                                var v3 = Web_DOM_Node.nextSibling(v1.value0.node)();
                                var v4 = Halogen_VDom_Machine.step(v1.value0.machine, v);
                                var newNode = Halogen_VDom_Machine.extract(v4);
                                Control_Applicative.when(Effect.applicativeEffect)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraBoolean)))(Unsafe_Reference.unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(v3)(v2))();
                                return {
                                    machine: v4,
                                    node: newNode,
                                    renderChildRef: v1.value0.renderChildRef
                                };
                            };
                        };
                        throw new Error("Failed pattern match at Halogen.VDom.Driver (line 159, column 5 - line 175, column 80): " + [ v1.constructor.name ]);
                    };
                };
            };
        };
        return {
            render: render,
            renderChild: Control_Category.identity(Control_Category.categoryFn),
            removeChild: removeChild,
            dispose: removeChild
        };
    };
};
var runUI = function (component) {
    return function (i) {
        return function (element) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Functor.map(Effect.functorEffect)(Web_HTML_HTMLDocument.toDocument)(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.document)(Web_HTML.window))))(function (v) {
                return Halogen_Aff_Driver.runUI(renderSpec(v)(element))(component)(i);
            });
        };
    };
};
module.exports = {
    runUI: runUI
};
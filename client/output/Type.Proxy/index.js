// Generated by purs version 0.13.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_CommutativeRing = require("../Data.CommutativeRing/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Proxy3 = (function () {
    function Proxy3() {

    };
    Proxy3.value = new Proxy3();
    return Proxy3;
})();
var Proxy2 = (function () {
    function Proxy2() {

    };
    Proxy2.value = new Proxy2();
    return Proxy2;
})();
var $$Proxy = (function () {
    function $$Proxy() {

    };
    $$Proxy.value = new $$Proxy();
    return $$Proxy;
})();
var showProxy3 = new Data_Show.Show(function (v) {
    return "Proxy3";
});
var showProxy2 = new Data_Show.Show(function (v) {
    return "Proxy2";
});
var showProxy = new Data_Show.Show(function (v) {
    return "Proxy";
});
var semiringProxy3 = new Data_Semiring.Semiring(function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, Proxy3.value, Proxy3.value);
var semiringProxy2 = new Data_Semiring.Semiring(function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, Proxy2.value, Proxy2.value);
var semiringProxy = new Data_Semiring.Semiring(function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, $$Proxy.value, $$Proxy.value);
var semigroupProxy3 = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return Proxy3.value;
    };
});
var semigroupProxy2 = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return Proxy2.value;
    };
});
var semigroupProxy = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
});
var ringProxy3 = new Data_Ring.Ring(function () {
    return semiringProxy3;
}, function (v) {
    return function (v1) {
        return Proxy3.value;
    };
});
var ringProxy2 = new Data_Ring.Ring(function () {
    return semiringProxy2;
}, function (v) {
    return function (v1) {
        return Proxy2.value;
    };
});
var ringProxy = new Data_Ring.Ring(function () {
    return semiringProxy;
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
});
var heytingAlgebraProxy3 = new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, Proxy3.value, function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, function (v) {
    return Proxy3.value;
}, Proxy3.value);
var heytingAlgebraProxy2 = new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, Proxy2.value, function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, function (v) {
    return Proxy2.value;
}, Proxy2.value);
var heytingAlgebraProxy = new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, $$Proxy.value, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, function (v) {
    return $$Proxy.value;
}, $$Proxy.value);
var functorProxy = new Data_Functor.Functor(function (f) {
    return function (m) {
        return $$Proxy.value;
    };
});
var eqProxy3 = new Data_Eq.Eq(function (x) {
    return function (y) {
        return true;
    };
});
var ordProxy3 = new Data_Ord.Ord(function () {
    return eqProxy3;
}, function (x) {
    return function (y) {
        return Data_Ordering.EQ.value;
    };
});
var eqProxy2 = new Data_Eq.Eq(function (x) {
    return function (y) {
        return true;
    };
});
var ordProxy2 = new Data_Ord.Ord(function () {
    return eqProxy2;
}, function (x) {
    return function (y) {
        return Data_Ordering.EQ.value;
    };
});
var eqProxy = new Data_Eq.Eq(function (x) {
    return function (y) {
        return true;
    };
});
var ordProxy = new Data_Ord.Ord(function () {
    return eqProxy;
}, function (x) {
    return function (y) {
        return Data_Ordering.EQ.value;
    };
});
var discardProxy3 = new Control_Bind.Discard(function (dictBind) {
    return Control_Bind.bind(dictBind);
});
var discardProxy2 = new Control_Bind.Discard(function (dictBind) {
    return Control_Bind.bind(dictBind);
});
var discardProxy = new Control_Bind.Discard(function (dictBind) {
    return Control_Bind.bind(dictBind);
});
var commutativeRingProxy3 = new Data_CommutativeRing.CommutativeRing(function () {
    return ringProxy3;
});
var commutativeRingProxy2 = new Data_CommutativeRing.CommutativeRing(function () {
    return ringProxy2;
});
var commutativeRingProxy = new Data_CommutativeRing.CommutativeRing(function () {
    return ringProxy;
});
var boundedProxy3 = new Data_Bounded.Bounded(function () {
    return ordProxy3;
}, Proxy3.value, Proxy3.value);
var boundedProxy2 = new Data_Bounded.Bounded(function () {
    return ordProxy2;
}, Proxy2.value, Proxy2.value);
var boundedProxy = new Data_Bounded.Bounded(function () {
    return ordProxy;
}, $$Proxy.value, $$Proxy.value);
var booleanAlgebraProxy3 = new Data_BooleanAlgebra.BooleanAlgebra(function () {
    return heytingAlgebraProxy3;
});
var booleanAlgebraProxy2 = new Data_BooleanAlgebra.BooleanAlgebra(function () {
    return heytingAlgebraProxy2;
});
var booleanAlgebraProxy = new Data_BooleanAlgebra.BooleanAlgebra(function () {
    return heytingAlgebraProxy;
});
var applyProxy = new Control_Apply.Apply(function () {
    return functorProxy;
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
});
var bindProxy = new Control_Bind.Bind(function () {
    return applyProxy;
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
});
var applicativeProxy = new Control_Applicative.Applicative(function () {
    return applyProxy;
}, function (v) {
    return $$Proxy.value;
});
var monadProxy = new Control_Monad.Monad(function () {
    return applicativeProxy;
}, function () {
    return bindProxy;
});
module.exports = {
    "Proxy": $$Proxy,
    Proxy2: Proxy2,
    Proxy3: Proxy3,
    eqProxy: eqProxy,
    functorProxy: functorProxy,
    ordProxy: ordProxy,
    applicativeProxy: applicativeProxy,
    applyProxy: applyProxy,
    bindProxy: bindProxy,
    booleanAlgebraProxy: booleanAlgebraProxy,
    boundedProxy: boundedProxy,
    commutativeRingProxy: commutativeRingProxy,
    discardProxy: discardProxy,
    heytingAlgebraProxy: heytingAlgebraProxy,
    monadProxy: monadProxy,
    ringProxy: ringProxy,
    semigroupProxy: semigroupProxy,
    semiringProxy: semiringProxy,
    showProxy: showProxy,
    eqProxy2: eqProxy2,
    ordProxy2: ordProxy2,
    booleanAlgebraProxy2: booleanAlgebraProxy2,
    boundedProxy2: boundedProxy2,
    commutativeRingProxy2: commutativeRingProxy2,
    discardProxy2: discardProxy2,
    heytingAlgebraProxy2: heytingAlgebraProxy2,
    ringProxy2: ringProxy2,
    semigroupProxy2: semigroupProxy2,
    semiringProxy2: semiringProxy2,
    showProxy2: showProxy2,
    eqProxy3: eqProxy3,
    ordProxy3: ordProxy3,
    booleanAlgebraProxy3: booleanAlgebraProxy3,
    boundedProxy3: boundedProxy3,
    commutativeRingProxy3: commutativeRingProxy3,
    discardProxy3: discardProxy3,
    heytingAlgebraProxy3: heytingAlgebraProxy3,
    ringProxy3: ringProxy3,
    semigroupProxy3: semigroupProxy3,
    semiringProxy3: semiringProxy3,
    showProxy3: showProxy3
};

// Generated by purs version 0.13.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Data_Row = require("../Type.Data.Row/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var HeytingAlgebraRecord = function (conjRecord, disjRecord, ffRecord, impliesRecord, notRecord, ttRecord) {
    this.conjRecord = conjRecord;
    this.disjRecord = disjRecord;
    this.ffRecord = ffRecord;
    this.impliesRecord = impliesRecord;
    this.notRecord = notRecord;
    this.ttRecord = ttRecord;
};
var HeytingAlgebra = function (conj, disj, ff, implies, not, tt) {
    this.conj = conj;
    this.disj = disj;
    this.ff = ff;
    this.implies = implies;
    this.not = not;
    this.tt = tt;
};
var ttRecord = function (dict) {
    return dict.ttRecord;
};
var tt = function (dict) {
    return dict.tt;
};
var notRecord = function (dict) {
    return dict.notRecord;
};
var not = function (dict) {
    return dict.not;
};
var impliesRecord = function (dict) {
    return dict.impliesRecord;
};
var implies = function (dict) {
    return dict.implies;
};
var heytingAlgebraUnit = new HeytingAlgebra(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, Data_Unit.unit, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return Data_Unit.unit;
}, Data_Unit.unit);
var heytingAlgebraRecordNil = new HeytingAlgebraRecord(function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
}, function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
}, function (v) {
    return function (v1) {
        return {};
    };
}, function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
}, function (v) {
    return function (v1) {
        return {};
    };
}, function (v) {
    return function (v1) {
        return {};
    };
});
var ffRecord = function (dict) {
    return dict.ffRecord;
};
var ff = function (dict) {
    return dict.ff;
};
var disjRecord = function (dict) {
    return dict.disjRecord;
};
var disj = function (dict) {
    return dict.disj;
};
var heytingAlgebraBoolean = new HeytingAlgebra($foreign.boolConj, $foreign.boolDisj, false, function (a) {
    return function (b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
}, $foreign.boolNot, true);
var conjRecord = function (dict) {
    return dict.conjRecord;
};
var heytingAlgebraRecord = function (dictRowToList) {
    return function (dictHeytingAlgebraRecord) {
        return new HeytingAlgebra(conjRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value), disjRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value), ffRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(Type_Data_Row.RProxy.value), impliesRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value), notRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value), ttRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(Type_Data_Row.RProxy.value));
    };
};
var conj = function (dict) {
    return dict.conj;
};
var heytingAlgebraFunction = function (dictHeytingAlgebra) {
    return new HeytingAlgebra(function (f) {
        return function (g) {
            return function (a) {
                return conj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (f) {
        return function (g) {
            return function (a) {
                return disj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (v) {
        return ff(dictHeytingAlgebra);
    }, function (f) {
        return function (g) {
            return function (a) {
                return implies(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (f) {
        return function (a) {
            return not(dictHeytingAlgebra)(f(a));
        };
    }, function (v) {
        return tt(dictHeytingAlgebra);
    });
};
var heytingAlgebraRecordCons = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictHeytingAlgebraRecord) {
            return function (dictHeytingAlgebra) {
                return new HeytingAlgebraRecord(function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = conjRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(conj(dictHeytingAlgebra)(get(ra))(get(rb)))(tail);
                        };
                    };
                }, function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = disjRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(disj(dictHeytingAlgebra)(get(ra))(get(rb)))(tail);
                        };
                    };
                }, function (v) {
                    return function (row) {
                        var tail = ffRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(row);
                        var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                        var insert = Record_Unsafe.unsafeSet(key);
                        return insert(ff(dictHeytingAlgebra))(tail);
                    };
                }, function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = impliesRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(implies(dictHeytingAlgebra)(get(ra))(get(rb)))(tail);
                        };
                    };
                }, function (v) {
                    return function (row) {
                        var tail = notRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(row);
                        var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                        var insert = Record_Unsafe.unsafeSet(key);
                        var get = Record_Unsafe.unsafeGet(key);
                        return insert(not(dictHeytingAlgebra)(get(row)))(tail);
                    };
                }, function (v) {
                    return function (row) {
                        var tail = ttRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(row);
                        var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                        var insert = Record_Unsafe.unsafeSet(key);
                        return insert(tt(dictHeytingAlgebra))(tail);
                    };
                });
            };
        };
    };
};
module.exports = {
    HeytingAlgebra: HeytingAlgebra,
    tt: tt,
    ff: ff,
    implies: implies,
    conj: conj,
    disj: disj,
    not: not,
    HeytingAlgebraRecord: HeytingAlgebraRecord,
    ffRecord: ffRecord,
    ttRecord: ttRecord,
    impliesRecord: impliesRecord,
    conjRecord: conjRecord,
    disjRecord: disjRecord,
    notRecord: notRecord,
    heytingAlgebraBoolean: heytingAlgebraBoolean,
    heytingAlgebraUnit: heytingAlgebraUnit,
    heytingAlgebraFunction: heytingAlgebraFunction,
    heytingAlgebraRecord: heytingAlgebraRecord,
    heytingAlgebraRecordNil: heytingAlgebraRecordNil,
    heytingAlgebraRecordCons: heytingAlgebraRecordCons
};

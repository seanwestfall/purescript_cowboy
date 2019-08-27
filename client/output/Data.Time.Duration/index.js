// Generated by purs version 0.13.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Seconds = function (x) {
    return x;
};
var Minutes = function (x) {
    return x;
};
var Milliseconds = function (x) {
    return x;
};
var Hours = function (x) {
    return x;
};
var Days = function (x) {
    return x;
};
var Duration = function (fromDuration, toDuration) {
    this.fromDuration = fromDuration;
    this.toDuration = toDuration;
};
var toDuration = function (dict) {
    return dict.toDuration;
};
var showSeconds = new Data_Show.Show(function (v) {
    return "(Seconds " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var showMinutes = new Data_Show.Show(function (v) {
    return "(Minutes " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var showMilliseconds = new Data_Show.Show(function (v) {
    return "(Milliseconds " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var showHours = new Data_Show.Show(function (v) {
    return "(Hours " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var showDays = new Data_Show.Show(function (v) {
    return "(Days " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var semigroupSeconds = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return v + v1;
    };
});
var semigroupMinutes = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return v + v1;
    };
});
var semigroupMilliseconds = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return v + v1;
    };
});
var semigroupHours = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return v + v1;
    };
});
var semigroupDays = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return v + v1;
    };
});
var ordSeconds = Data_Ord.ordNumber;
var ordMinutes = Data_Ord.ordNumber;
var ordMilliseconds = Data_Ord.ordNumber;
var ordHours = Data_Ord.ordNumber;
var ordDays = Data_Ord.ordNumber;
var newtypeSeconds = new Data_Newtype.Newtype(function (n) {
    return n;
}, Seconds);
var newtypeMinutes = new Data_Newtype.Newtype(function (n) {
    return n;
}, Minutes);
var newtypeMilliseconds = new Data_Newtype.Newtype(function (n) {
    return n;
}, Milliseconds);
var newtypeHours = new Data_Newtype.Newtype(function (n) {
    return n;
}, Hours);
var newtypeDays = new Data_Newtype.Newtype(function (n) {
    return n;
}, Days);
var monoidSeconds = new Data_Monoid.Monoid(function () {
    return semigroupSeconds;
}, 0.0);
var monoidMinutes = new Data_Monoid.Monoid(function () {
    return semigroupMinutes;
}, 0.0);
var monoidMilliseconds = new Data_Monoid.Monoid(function () {
    return semigroupMilliseconds;
}, 0.0);
var monoidHours = new Data_Monoid.Monoid(function () {
    return semigroupHours;
}, 0.0);
var monoidDays = new Data_Monoid.Monoid(function () {
    return semigroupDays;
}, 0.0);
var fromDuration = function (dict) {
    return dict.fromDuration;
};
var negateDuration = function (dictDuration) {
    var $56 = toDuration(dictDuration);
    var $57 = Data_Newtype.over(newtypeMilliseconds)(newtypeMilliseconds)(Milliseconds)(Data_Ring.negate(Data_Ring.ringNumber));
    var $58 = fromDuration(dictDuration);
    return function ($59) {
        return $56($57($58($59)));
    };
};
var eqSeconds = Data_Eq.eqNumber;
var eqMinutes = Data_Eq.eqNumber;
var eqMilliseconds = Data_Eq.eqNumber;
var eqHours = Data_Eq.eqNumber;
var eqDays = Data_Eq.eqNumber;
var durationSeconds = new Duration(Data_Newtype.over(newtypeSeconds)(newtypeMilliseconds)(Seconds)(function (v) {
    return v * 1000.0;
}), Data_Newtype.over(newtypeMilliseconds)(newtypeSeconds)(Milliseconds)(function (v) {
    return v / 1000.0;
}));
var durationMinutes = new Duration(Data_Newtype.over(newtypeMinutes)(newtypeMilliseconds)(Minutes)(function (v) {
    return v * 60000.0;
}), Data_Newtype.over(newtypeMilliseconds)(newtypeMinutes)(Milliseconds)(function (v) {
    return v / 60000.0;
}));
var durationMilliseconds = new Duration(Control_Category.identity(Control_Category.categoryFn), Control_Category.identity(Control_Category.categoryFn));
var durationHours = new Duration(Data_Newtype.over(newtypeHours)(newtypeMilliseconds)(Hours)(function (v) {
    return v * 3600000.0;
}), Data_Newtype.over(newtypeMilliseconds)(newtypeHours)(Milliseconds)(function (v) {
    return v / 3600000.0;
}));
var durationDays = new Duration(Data_Newtype.over(newtypeDays)(newtypeMilliseconds)(Days)(function (v) {
    return v * 8.64e7;
}), Data_Newtype.over(newtypeMilliseconds)(newtypeDays)(Milliseconds)(function (v) {
    return v / 8.64e7;
}));
var convertDuration = function (dictDuration) {
    return function (dictDuration1) {
        var $60 = toDuration(dictDuration1);
        var $61 = fromDuration(dictDuration);
        return function ($62) {
            return $60($61($62));
        };
    };
};
module.exports = {
    fromDuration: fromDuration,
    toDuration: toDuration,
    Milliseconds: Milliseconds,
    Seconds: Seconds,
    Minutes: Minutes,
    Hours: Hours,
    Days: Days,
    Duration: Duration,
    convertDuration: convertDuration,
    negateDuration: negateDuration,
    newtypeMilliseconds: newtypeMilliseconds,
    eqMilliseconds: eqMilliseconds,
    ordMilliseconds: ordMilliseconds,
    semigroupMilliseconds: semigroupMilliseconds,
    monoidMilliseconds: monoidMilliseconds,
    showMilliseconds: showMilliseconds,
    newtypeSeconds: newtypeSeconds,
    eqSeconds: eqSeconds,
    ordSeconds: ordSeconds,
    semigroupSeconds: semigroupSeconds,
    monoidSeconds: monoidSeconds,
    showSeconds: showSeconds,
    newtypeMinutes: newtypeMinutes,
    eqMinutes: eqMinutes,
    ordMinutes: ordMinutes,
    semigroupMinutes: semigroupMinutes,
    monoidMinutes: monoidMinutes,
    showMinutes: showMinutes,
    newtypeHours: newtypeHours,
    eqHours: eqHours,
    ordHours: ordHours,
    semigroupHours: semigroupHours,
    monoidHours: monoidHours,
    showHours: showHours,
    newtypeDays: newtypeDays,
    eqDays: eqDays,
    ordDays: ordDays,
    semigroupDays: semigroupDays,
    monoidDays: monoidDays,
    showDays: showDays,
    durationMilliseconds: durationMilliseconds,
    durationSeconds: durationSeconds,
    durationMinutes: durationMinutes,
    durationHours: durationHours,
    durationDays: durationDays
};

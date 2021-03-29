#!/usr/local/bin/node

// <xbar.title>Moon Phase</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Volker Wieban</xbar.author>
// <xbar.author.github>hpcodecraft</xbar.author.github>
// <xbar.desc>Shows the current moon phase</xbar.desc>
// <xbar.image>https://cloud.githubusercontent.com/assets/1476865/24201253/ce0d8c5e-0f0f-11e7-8e44-503654407850.png</xbar.image>
// <xbar.dependencies>node</xbar.dependencies>

// Moon phase calculations taken from https://github.com/tingletech/moon-phase
Date.prototype.getJulian = function() {
    return ((this / 86400000) - (this.getTimezoneOffset() / 1440) + 2440587.5);
};

function moon_day(today) {
    var GetFrac = function(fr) {
        return (fr - Math.floor(fr));
    };
    var thisJD = today.getJulian();
    var year = today.getFullYear();
    var degToRad = 3.14159265 / 180;
    var K0, T, T2, T3, J0, F0, M0, M1, B1, oldJ;
    K0 = Math.floor((year - 1900) * 12.3685);
    T = (year - 1899.5) / 100;
    T2 = T * T;
    T3 = T * T * T;
    J0 = 2415020 + 29 * K0;
    F0 = 0.0001178 * T2 - 0.000000155 * T3 + (0.75933 + 0.53058868 * K0) - (0.000837 * T + 0.000335 * T2);
    M0 = 360 * (GetFrac(K0 * 0.08084821133)) + 359.2242 - 0.0000333 * T2 - 0.00000347 * T3;
    M1 = 360 * (GetFrac(K0 * 0.07171366128)) + 306.0253 + 0.0107306 * T2 + 0.00001236 * T3;
    B1 = 360 * (GetFrac(K0 * 0.08519585128)) + 21.2964 - (0.0016528 * T2) - (0.00000239 * T3);
    var phase = 0;
    var jday = 0;
    while (jday < thisJD) {
        var F = F0 + 1.530588 * phase;
        var M5 = (M0 + phase * 29.10535608) * degToRad;
        var M6 = (M1 + phase * 385.81691806) * degToRad;
        var B6 = (B1 + phase * 390.67050646) * degToRad;
        F -= 0.4068 * Math.sin(M6) + (0.1734 - 0.000393 * T) * Math.sin(M5);
        F += 0.0161 * Math.sin(2 * M6) + 0.0104 * Math.sin(2 * B6);
        F -= 0.0074 * Math.sin(M5 - M6) - 0.0051 * Math.sin(M5 + M6);
        F += 0.0021 * Math.sin(2 * M5) + 0.0010 * Math.sin(2 * B6 - M6);
        F += 0.5 / 1440;
        oldJ = jday;
        jday = J0 + 28 * phase + Math.floor(F);
        phase++;
    }

    // 29.53059 days per lunar month
    return (((thisJD - oldJ) / 29.53059));
}

function phase_text(phase) {
    var txt_phase;
    if (phase <= 0.0625 || phase > 0.9375) {
        txt_phase = "new_moon";
    } else if (phase <= 0.1875) {
        txt_phase = "waxing_crescent_moon";
    } else if (phase <= 0.3125) {
        txt_phase = "first_quarter_moon";
    } else if (phase <= 0.4375) {
        txt_phase = "waxing_gibbous_moon";
    } else if (phase <= 0.5625) {
        txt_phase = "full_moon";
    } else if (phase <= 0.6875) {
        txt_phase = "waning_gibbous_moon";
    } else if (phase <= 0.8125) {
        txt_phase = "last_quarter_moon";
    } else if (phase <= 0.9375) {
        txt_phase = "waning_crescent_moon";
    }

    return txt_phase;
}

var phaseLabel = {
  "new_moon"              : "New moon",
  "waxing_crescent_moon"  : "Waxing crescent moon",
  "first_quarter_moon"    : "First quarter moon",
  "waxing_gibbous_moon"   : "Waxing gibbous moon",
  "full_moon"             : "Full moon",
  "waning_gibbous_moon"   : "Waning gibbous moon",
  "last_quarter_moon"     : "Third quarter moon",
  "waning_crescent_moon"  : "Waning crescent moon",
};

var phase     = moon_day(new Date()),
    phase_str = phase_text(phase),
    output    = ':' + phase_str + ':|dropdown=false \n' +
              '---\n' +
              phaseLabel[phase_str] + '\n' +
              "Completed " + Math.round(phase*100) + "% of lunar cycle";

console.log(output);

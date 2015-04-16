
// stats.js
//
// adapted from code provided by Larry Battle, written Mar 06, 2011
// http://bateru.com/news/2011/03/javascript-standard-deviation-variance-average-functions/
//
// Purpose: Calculate standard deviation, variance, and average among an array of numbers.

(function() {
    var st = {};

    if (typeof module !== 'undefined') {
        // Assign the `st` object to exports, so that you can require
        // it in [node.js](http://nodejs.org/)
        module.exports = st;
    } else {
        // Otherwise, in a browser, we assign `st` to the window object,
        // so you can simply refer to it as `st`.
        this.st = st;
    }

    var isArray = function (obj) {
        return Object.prototype.toString.call(obj) === "[object Array]";
    }

    getNumWithSetDec = function (num, numOfDec) {
        var pow10s = Math.pow(10, numOfDec || 0);
        return ( numOfDec ) ? Math.round(pow10s * num) / pow10s : num;
    }

    getAverageFromNumArr = function (numArr, numOfDec) {
        if (!isArray(numArr)) {
            return false;
        }
        var i = numArr.length,
            sum = 0;
        while (i--) {
            sum += numArr[ i ];
        }
        return getNumWithSetDec((sum / numArr.length ), numOfDec);
        // public api
        return {
            getAverageFromNumArr: getAverageFromNumArr,
        };

    };

    getVariance = function (numArr, numOfDec) {
        if (!isArray(numArr)) {
            return false;
        }
        var avg = getAverageFromNumArr(numArr, numOfDec),
            i = numArr.length,
            v = 0;

        while (i--) {
            v += Math.pow((numArr[ i ] - avg), 2);
        }
        v /= numArr.length;
        return getNumWithSetDec(v, numOfDec);
    };

    getStandardDeviation = function (numArr, numOfDec) {
        if (!isArray(numArr)) {
            return false;
        }
        var stdDev = Math.sqrt(getVariance(numArr, numOfDec));
        return getNumWithSetDec(stdDev, numOfDec);
    };



    st.getAverageFromNumArr = getAverageFromNumArr;
    st.getStandardDeviation = getStandardDeviation;
    st.getVariance          = getVariance;


})(this);


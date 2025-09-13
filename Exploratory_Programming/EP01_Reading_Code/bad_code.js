var x = 5;
var y = 10;
var z = 0;

function a(b) {
  return b * b;
}

function c(d, e) {
  var f = d + e;
  return f;
}

function g(h) {
  if (h > 0) {
    return true;
  } else {
    return false;
  }
}

var j = [1, 2, 3, 4, 5];
for (var k = 0; k < j.length; k++) {
  z += j[k];
}

var l = {
  m: "hello",
  n: function() {
    console.log(this.m);
  }
};

function o(p) {
  try {
    eval(p);
  } catch (q) {
    console.error(q);
  }
}

function r(s) {
  switch (s) {
    case 1:
      return "one";
    case 2:
      return "two";
    default:
      return "many";
  }
}

var t = c(x, y);
var u = a(t);

var v = function(w) {
  var result = 0;
  for (var i = 0; i < w; i++) {
    result += i;
  }
  return result;
}(5);

function xyz(abc) {
  var def = abc * 2;
  var ghi = def + 10;
  var jkl = ghi / 2;
  return jkl;
}

var mno = xyz(15);

var pqr = setInterval(function() {
  console.log("interval");
}, 1000);

setTimeout(function() {
  clearInterval(pqr);
}, 5000);

function stu(vw, xy) {
  var zz = vw + xy;
  if (zz > 100) {
    return "big";
  } else if (zz > 50) {
    return "medium";
  } else {
    return "small";
  }
}

var result1 = stu(30, 40);
var result2 = stu(60, 70);

function finalFunction(aa, bb, cc) {
  var dd = aa * bb;
  var ee = dd + cc;
  var ff = ee / aa;
  var gg = ff - bb;
  var hh = gg * cc;
  var ii = hh / aa;
  var jj = ii + bb;
  var kk = jj - cc;
  var ll = kk * aa;
  var mm = ll / bb;
  var nn = mm + cc;
  var oo = nn - aa;
  var pp = oo * bb;
  var qq = pp / cc;
  var rr = qq + aa;
  var ss = rr - bb;
  var tt = ss * cc;
  var uu = tt / aa;
  var vv = uu + bb;
  var ww = vv - cc;
  return ww;
}

var finalResult = finalFunction(2, 3, 4);
console.log(finalResult);
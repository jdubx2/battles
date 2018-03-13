// this code is a lot more messy than I usually like, but meh it works.

// color index to temperature in kelvin
function bvToT(bv) {
  let t;

  // make sure bv is within its bounds [-0.4, 2] otherwise the math doesnt work
  if (bv < -0.4) {
    bv = -0.4;
  } else if (bv > 2) {
    bv = 2;
  }

  // found it online at http://www.wikiwand.com/en/Color_index
  t = 4600 * ((1 / ((0.92 * bv) + 1.7)) + (1 / ((0.92 * bv) + 0.62)));

  // console.log('t: ' + t);

  return t;
}

// temperature to CIE xyY Colorspace, assume Y is 1
function tToXyy(t) {
  let x, y, Y = 1; // Y is the luminance, I just assume full luminance for sanity

  // approximation of CIE xyY (http://www.wikiwand.com/en/CIE_1931_color_space) using https://en.wikipedia.org/wiki/Planckian_locus
  if (t >= 1667 && t <= 4000) {
    x = (-0.2661239 * (Math.pow(10, 9) / Math.pow(t, 3))) -
        (-0.2343580 * (Math.pow(10, 6) / Math.pow(t, 2))) +
        (0.8776956 * (Math.pow(10, 3) / t)) + 0.179910;
  } else if (t >= 4000 && t <= 25000) {
    x = (-3.0258469 * (Math.pow(10, 9) / Math.pow(t, 3))) +
        (2.1070379 * (Math.pow(10, 6) / Math.pow(t, 2))) +
        (0.2226347 * (Math.pow(10, 3) / t)) + 0.240390;
  }

  if (t >= 1667 && t <= 2222) {
    y = (-1.1063814 * Math.pow(x, 3)) -
        (1.34811020 * Math.pow(x, 2)) +
        (2.18555832 * x) -
         0.20219683;
  } else if (t >= 2222 && t <= 4000) {
    y = (-0.9549476 * Math.pow(x, 3)) -
        (1.37418593 * Math.pow(x, 2)) +
        (2.09137015 * x) -
         0.16748867;
  } else if (t >= 4000 && t <= 25000) {
    y = (3.0817580 * Math.pow(x, 3)) -
        (5.87338670 * Math.pow(x, 2)) +
        (3.75112997 * x) -
         0.37001483;
  }

  // console.log('xyY: ' + [x, y, Y]);

  return [x, y, Y];
}

// xyY Color space to XYZ, prepping for conversion to linear RGB
function xyYToXyz(xyY) {
  let X, Y, Z,
      x = xyY[0],
      y = xyY[1];

  // X and Z tristimulus values calculated using https://en.wikipedia.org/wiki/CIE_1931_color_space?oldformat=true#CIE_xy_chromaticity_diagram_and_the_CIE_xyY_color_space
  Y = xyY[2];
  X = (y === 0) ? 0 : (x * Y) / y;
  Z = (y === 0) ? 0 : ((1 - x - y) * Y) / y;

  // console.log('XYZ: ' + [X, Y, Z]);

  return [X, Y, Z];
}

//XYZ color space to linear RGB, finally a format I recognize
function xyzToRgb(xyz) {
  let r, g, b,
      x = xyz[0],
      y = xyz[1],
      z = xyz[2];

  // using matrix from https://www.cs.rit.edu/~ncs/color/t_convert.html#RGB%20to%20XYZ%20&%20XYZ%20to%20RGB
  r = (3.2406 * x) +
      (-1.5372 * y) +
      (-0.4986 * z);

  g = (-0.9689 * x) +
      (1.8758 * y) +
      (0.0415 * z);

  b = (0.0557 * x) +
      (-0.2040 * y) +
      (1.0570 * z);

  // make sure the values didnt overflow
  r = (r > 1) ? 1 : r;
  g = (g > 1) ? 1 : g;
  b = (b > 1) ? 1 : b;

  // console.log('rgb: ' + [r, g, b]);

  return [r, g, b];
}

// Im supposed to gamma correct and convert to sRGB but right now it breaks things so TODO: fix this..
function gammaCorrect(rgb) {
  let a = 0.055,
      gamma = 2.2,
      R, G, B,
      r = rgb[0],
      g = rgb[1],
      b = rgb[2];

  // using https://en.wikipedia.org/wiki/SRGB?oldformat=true#The_forward_transformation_.28CIE_xyY_or_CIE_XYZ_to_sRGB.29
  /*R = (r <= 0.0031308) ? 12.92 * r : ((1 + r) * Math.pow(r, 1 / 2.2)) - a;
  G = (g <= 0.0031308) ? 12.92 * g : ((1 + g) * Math.pow(g, 1 / 2.2)) - a;
  B = (b <= 0.0031308) ? 12.92 * b : ((1 + b) * Math.pow(b, 1 / 2.2)) - a;*/

  /*R = Math.pow(r, 1 / gamma);
  G = Math.pow(g, 1 / gamma);
  B = Math.pow(b, 1 / gamma);*/

  R = r;
  G = g / 1.05; // idk but i messed up somewhere and this makes it look better
  B = b;

  R = (R > 1) ? 1 : R;
  G = (G > 1) ? 1 : G;
  B = (B > 1) ? 1 : B;

  // turn the 0-1 rgb value to 0-255
  return [Math.round(R * 255), Math.round(G * 255), Math.round(B * 255)];
}

// rgb to hex is cake
function rgbToHex(rgb) {
  return '#' + rgb[0].toString(16) + rgb[1].toString(16) + rgb[2].toString(16);
}

// now put it all together!
function bvToRgb(bv) {
  let t, xyY, xyz, rgb, crgb;

  t = bvToT(bv);

  xyY = tToXyy(t);

  xyz = xyYToXyz(xyY);

  rgb = xyzToRgb(xyz);

  crgb = gammaCorrect(rgb);

  return crgb;
}

// loop though and generate an array in a range
function generateGradient(min, max, interval) {
  let i = min,
      ii = max,
      gradient = [];

  for (; i <= ii; i = i + interval) {
    gradient.push({'hex': rgbToHex(bvToRgb(i)), 'bv': i});
  }

  return gradient;
}

// now make the gradient array into an array of dom elements
function generateElements(gradient) {
  let i = 0,
      ii = gradient.length,
      elements = [];

  for (; i < ii; i++) {
    let tmp = document.createElement('div');

    tmp.className = 'color';
    tmp.setAttribute('style', 'background: ' + gradient[i].hex + ';');
    tmp.setAttribute('title', 'B-V: ' + gradient[i].bv.toFixed(2) + ' Hex: ' + gradient[i].hex);

    elements.push(tmp);
  }

  return elements;
}

// append all the elements to the page
function appendElements(elements) {
  let i = 0,
      ii = elements.length;

  for (; i < ii; i++) {
    document.getElementsByClassName('gradient')[0].appendChild(elements[i]);
  }
}

let rgb = bvToRgb(2);

// console.log('Final rgb: ' + rgb);

let hex = rgbToHex(rgb);

// console.log('Hex: ' + hex);

let gradient = generateGradient(-0.4, 2, 0.01);

console.log(gradient);

let elements = generateElements(gradient);

console.log(elements);

appendElements(elements);

var arr1 = [0.442,0.484,0.031,0.66,1.22,0.49,1.439,0.687,1.005,1.029,1.068,0.524,0.499,0.355,1.565,1.128,1.5,0.952,1.571,0.437,0.965,0.469,0.486,0.644,0.56,0.906,0.955,0.833,0.716,0.41,1.301,1.072,0.595,1.011,0.974,1.166,0.62,0.542,0.964,0.139,1.014,0.456,0.251,1.031,0.345,0.476,0.539,0.293,0.371,1.077,1.603,1.042,1.156,0.7,0.594,0.409,0.524,0.526,1.032,0.356,1.104,-0.053,0.411,0.437,1.12,0.423,0.511,0.578,0.508,0.473,1.042,0.528,1.528,0.503,0.471,1.487,0.491,0.144,1.037,0.563,0.077,0.574,1.165,0.674]

// for(x in arr1){
//   console.log(rgbToHex(bvToRgb(arr1[x])));
// }

// $.getJSON( "data/cl.json", function(data) {
//   // var items = [];
//   // $.each( data, function( key, val ) {
//   //   items.push(val);
//   // });
//  console.log(data);
// });

var dataStr = "data:text/json;charset=utf-8," + encodeURIComponent(JSON.stringify(gradient));
var dlAnchorElem = document.getElementById('downloadAnchorElem');
dlAnchorElem.setAttribute("href",     dataStr     );
dlAnchorElem.setAttribute("download", "gradient.json");
dlAnchorElem.click();
// listen for click on the button to convert a value
// document.getElementById('convert').addEventListener('click', function(e) {
//   e.preventDefault();
//
//   let input = document.getElementById('input').value;
//
//   document.getElementsByClassName('output')[0].innerHTML = rgbToHex(bvToRgb(input));
// });

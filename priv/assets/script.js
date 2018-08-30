/* Copyright 2018, Daniel Oltmanns (https://github.com/oltdaniel) */

// General helper functions
function e(id) {
  return document.getElementById(id);
}

var el_history = e('message-history'),
    inp_message = e('message-content'),
    btn_send = e('message-send'),
    uid = '',
    partnerUid = '',
    keys = {};

// Chat functions
function newMessage(type, content) {
  el_history.innerHTML += "<div class=\"message " + type + "\">" + content + "</div>";
}

// From https://en.wikipedia.org/w/index.php?title=Euclidean_algorithm&oldid=855705350
function gcd(a, b) {
  while(b > 0) {
    var t = b;
    b = a % b;
    a = t;
  }
  return a;
}

// Optimized with https://en.wikipedia.org/w/index.php?title=Modular_exponentiation&oldid=857101936
// Is equal to `Math.pow(b, e) % m`
function powermod(b, e, m){
  var r = 1,
      b = b % m;
  while(e > 0) {
    if(e % 2 == 1) {
      r = (r * b) % m;
    }
    b = (b * b) % m;
    e = Math.floor(e / 2);
  }
  return r;
}

// Optimized with extended Euclidean algorithm https://en.wikipedia.org/w/index.php?title=Extended_Euclidean_algorithm&oldid=853933038
function modinverse(a, n) {
  var t = 0, newt = 1, r = n, newr = a;
  while(newr > 0) {
    var q = Math.floor(r / newr);
    newtt = newt;
    newt = t - q * newt;
    t = newtt;
    newrr = newr;
    newr = r - q * newr;
    r = newr;
  }
  if(t < 0) t+= n;
  return t;
}

// Optimized with https://en.wikipedia.org/w/index.php?title=Extended_Euclidean_algorithm&oldid=853933038
function isPrime(num) {
  return powermod(3, num - 1, num) === 1;
}

function generatePrime(len) {
  var num =  Math.floor(Math.pow(10, len) * Math.random());
  while(!isPrime(num)) num++;
  return num;
}

// RSA toolbox
function RSA(len) {
  var p = generatePrime(len),
      q = generatePrime(len),
      n = p * q,
      phi = (p - 1) * (q - 1),
      e = generatePrime(len),
      d = modinverse(e, phi);

  if(gcd(d, phi) != 1) {
    location.reload();
  }

  this.private = d;
  this.public = e;
  this.n = n;
}

RSA.prototype.encrypt = function(msg) {
  var r = this;
  return msg.split('').map(function(c) {
    return String.fromCharCode(powermod(c.charCodeAt(0), r.public, r.n));
  }).join('');
}

RSA.prototype.decrypt = function(msg) {
  var r = this;
  return msg.split('').map(function(c) {
    return String.fromCharCode(powermod(c.charCodeAt(0), r.private, r.n));
  }).join('');
}

function uidEncrypt(key, msg) {
  return msg.split('').map(function(c) {
    return String.fromCharCode(powermod(c.charCodeAt(0), key['k'], key['n']));
  }).join('');
}

// Initialize rsa key
var r = new RSA(2);

// Initialize websocket connection
var s = new WebSocket('ws://' + window.location.hostname + ':' + window.location.port + '/ws');

// Message functions
function parseMessage(msg) {
  var cmd = msg.substr(0, 2);
  switch (cmd) {
    case '/p':
      s.send('/p ' + (uidEncrypt(keys[partnerUid], msg.substring(2).trim())));
      break;
    case '/o':
      partnerUid = msg.substring(2).trim();
      s.send(msg);
      if(keys[partnerUid] === undefined) {
        s.send('/kr ' + partnerUid);
      }
      break;
    default:
      newMessage('server', 'unknown command');
      break;
  }
}

s.onopen = function() {
  newMessage('server', 'connected to server');
  s.send('/k ' + r.n + ' ' + r.public);
  var ping = function() {
    s.send('ping');
    setTimeout(ping, 5000);
  };
  ping();
};

s.onclose = function() {
  newMessage('server error', 'disconnected from server');
};

s.onmessage = function(m) {
  console.log(m);
  var sender = m.data.substr(0, m.data.indexOf(':'));
  if(sender == 'server') {
    newMessage('server', m.data.substring(sender.length + 1));
  } else if(sender == 'uid') {
    uid = m.data.substring(4);
    newMessage('server', 'your id is: ' + uid);
  } else if(sender == 'key') {
    var v = m.data.split(' ');
    var partnerUid = v[1],
        n = parseInt(v[2]),
        key = parseInt(v[3]);
    keys[partnerUid] = {'k': key, 'n': n};
    newMessage('server', 'received key from ' + partnerUid);
  } else {
    var v = m.data.split(' ');
    newMessage('partner', v[0] + ' ' + r.decrypt(v[1]));
  }
  el_history.scrollTo(0, el_history.scrollHeight);
};

// Listen for click event
btn_send.onclick = function() {
  var m = inp_message.value;
  m = m.replace(/&/g, "&amp;")
       .replace(/</g, "&lt;")
       .replace(/>/g, "&gt;");
  parseMessage(m);
  newMessage('me', m);
  inp_message.value = '';
};

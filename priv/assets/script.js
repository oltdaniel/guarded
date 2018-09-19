/* Copyright 2018, Daniel Oltmanns (https://github.com/oltdaniel) */

/**
 * Handle explanation boxes
 */

// Get modal template from body base on type
function getExplanation(type) {
  return document.getElementById('m-' + type).innerHTML;
}

// Construct an modal and display it to the screen
function explain(type, values) {
  var modalContent = getExplanation(type);
  for(var i = 0; i < values.length; i++) {
    modalContent = modalContent.replace(new RegExp('\{' + values[i] + '\}', 'g'), values[++i]);
  }
  winExplanation.innerHTML = modalContent;
}

/**
 * Extend default prototypes
 */

// Add hashCode function to String to generate
// an unique hash for a specific string
// NOTE: High collision due to modular step at the end
//       to keep number low for less computation
String.prototype.hashCode = function(n) {
  var hash = 0, i, chr;
  if (this.length === 0) return hash;
  for (i = 0; i < this.length; i++) {
    chr   = this.charCodeAt(i);
    hash  = ((hash << 5) - hash) + chr;
    hash |= 0;
  }
  return hash % n;
}

// String replace all shortcut
// Based on https://stackoverflow.com/a/17606289
String.prototype.replaceAll = function(search, replacement) {
  var target = this;
  return target.replace(new RegExp(search, 'g'), replacement);
};

// Construct an ASCII uint8array from a string
// NOTE: Use this to prevent the use of utf8
function toAsciiArray(s) {
  var bytes = s.split('').map(function(c) {
    return c.charCodeAt(0);
  });
  return new Uint8Array(bytes);
}

/**
 * Mathematical functions
 */

// Euclidean algorithm
// Based on https://en.wikipedia.org/w/index.php?title=Euclidean_algorithm&oldid=855705350
function gcd(a, b) {
  while(b > 0) {
    var t = b;
    b = a % b;
    a = t;
  }
  return a;
}

// `Math.pow(b, e) % m` optimized for large numbers
// Based on https://en.wikipedia.org/w/index.php?title=Modular_exponentiation&oldid=857101936
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

// Extended Euclidean Algorithm to calculate
// `a^-q mod n`
// Based on https://rosettacode.org/wiki/Modular_inverse#C
// Based on https://en.wikipedia.org/w/index.php?title=Extended_Euclidean_algorithm&oldid=853933038
function modinverse(a, n) {
  var n0 = n, t, q;
  var x0 = 0, x1 = 1;
  if(n == 1) return 1;
  while(a > 1) {
    q = Math.floor(a / n);
    t = n;
    n = a % n;
    a = t;
    t = x0;
    x0 = x1 - q * x0;
    x1 = t;
  }
  while(x1 < 0) x1 += n0;
  return x1;
}

// Prime check with Extended Euclidean Algorithm
// Based on https://en.wikipedia.org/w/index.php?title=Extended_Euclidean_algorithm&oldid=853933038
function isPrime(num) {
  return powermod(3, num - 1, num) == 1;
}

// Generate a number with x digits
function randomNumber(len) {
  return Math.floor(Math.pow(10, len) * Math.random());
}

// Generate a prime with at least x digits
function generatePrime(len) {
  var num =  randomNumber(len);
  while(!isPrime(num)) num++;
  return num;
}

// Generate a random alphanumeric string with x characters
function randomString(len) {
  var s = "0123456789abcdefghijklmnopqrstuvwxyz";
  return Array(len).join().split(',').map(function() { return s.charAt(Math.floor(Math.random() * s.length)); }).join('');
}

/**
 * Implement RSA
 */

// Specify main constructor for new RSA instance
function RSA(len, c = 0) {
  var p = generatePrime(len),
      q = generatePrime(len),
      n = p * q,
      phi = (p - 1) * (q - 1),
      e = generatePrime(len);

  while(e > phi) e = generatePrime(len);
  if(gcd(e, phi) != 1) {
    if(c > 5) alert('Cannot find any number');
    return new RSA(len, c + 1);
  }
  while(gcd(e * modinverse(e, phi), phi) != 1) {
    e = generatePrime(len);
  }
  this.private = d = modinverse(e, phi);
  this.public = e;
  this.n = n;

  newMessage('info', "generated RSA key " +
    "<i class=\"info-btn\" onclick=\"" +
      "explain(\'rsa-init\', " +
      "[\'p\', " + p + ", \'q\'," + q + ", \'n\'," + n +", \'phi\'," + phi + ", \'e\'," + e + ", \'d\'," + d +"]" +
      ")\">" +
    "(info)</i>");
}

// Extend RSA prototype with encrypt function
// NOTE: Not used
RSA.prototype.encrypt = function(msg) {
  var r = this;
  msg = window.btoa(msg);
  return msg.split('').map(function(c) {
    return (powermod(c.charCodeAt(0), r.public, r.n));
  }).join('.');
}

// Extend RSA prototype with decrypt function
RSA.prototype.decrypt = function(msg) {
  var r = this;
  var d = msg.split('.').map(function(c) {
    return String.fromCharCode(powermod(parseInt(c), r.private, r.n));
  }).join('');
  newMessage('info', "decrypt message " +
  "<i class=\"info-btn\" onclick=\"" +
    "explain(\'rsa-decrypt\', " +
    "[\'priv\', " + this.private + ", \'n\', " + this.n + ", \'m\', \'" + msg.substr(0, 5) + "...\', \'d\', \'" + window.atob(d) + "\']" +
    ")\">" +
  "(info)</i>");
  return window.atob(d);
}

// Extend RSA prototype with sign function
RSA.prototype.sign = function(msg) {
  var r = this,
      signature = powermod(msg.hashCode(r.n), this.private, this.n);
  newMessage('info', "sign message " +
  "<i class=\"info-btn\" onclick=\"" +
    "explain(\'rsa-sign\', " +
    "[\'priv\', " + this.private + ", \'n\', " + this.n + ", \'m\', \'" + msg.hashCode(r.n) + "\', \'s\', " + signature + "]" +
    ")\">" +
  "(info)</i>");
  return signature;
};

/**
 * Global variables
 */

// Global element variables
var elHistory = document.getElementById('message-history'),
    inpMessage = document.getElementById('message-content'),
    btnSend = document.getElementById('message-send'),
    winExplanation = document.getElementById('explanation');

// Global protocol variables
var uid = '',
    partnerUid = '',
    keys = {},
    sharedKey = null;

// Global encryption varliables
var r = new RSA(4);

// Global connection varliables
var s = new WebSocket('ws://' + window.location.hostname + ':' + window.location.port + '/ws');

/**
 * Extend RSA with external encryption functions
 */

// Encrypt a message for another user
function uidEncrypt(key, msg) {
  msg = window.btoa(msg);
  var msgEncrypted = msg.split('').map(function(c) {
    return (powermod(c.charCodeAt(0), key['k'], key['n']));
  }).join('.');
  newMessage('info', "encrypt with partner key " +
  "<i class=\"info-btn\" onclick=\"" +
    "explain(\'rsa-encrypt\', " +
    "[\'pub\', " + key['k'] + ", \'n\', " + key['n'] + ", \'m\', \'" + msg.substr(0, 5) + "...\', \'e\', \'" + msgEncrypted + "\']" +
    ")\">" +
  "(info)</i>");
  return msgEncrypted;
}

// Verify signature of another user
function uidVerify(key, msg, signature) {
  newMessage('info', "verify partner message signature " +
  "<i class=\"info-btn\" onclick=\"" +
    "explain(\'rsa-verify\', " +
    "[\'pub\', " + key['k'] + ", \'n\', " + key['n'] + ", \'s\', " + signature + ", \'m\', \'" + msg.hashCode(key['n']) + "\']" +
    ")\">" +
  "(info)</i>");
  return (powermod(parseInt(signature), key['k'], key['n']) === msg.hashCode(key['n']));
}

/**
 * Simplify client functions
 */

// Add new message to the history
function newMessage(type, content) {
  elHistory.innerHTML += '<div class="message ' + type + '">' +
    content +
    '</div>';
  elHistory.scrollTo(0, elHistory.scrollHeight);
}

// Simplify send message function
function sendMessage(msg) {
  var ivec = toAsciiArray(randomString(16)),
      aesCounter = new aesjs.Counter(ivec),
      aes = new aesjs.ModeOfOperation.ctr(aesjs.utils.hex.toBytes(sharedKey), aesCounter),
      msgBytes = toAsciiArray(msg),
      encryptedMsgBytes = aes.encrypt(msgBytes),
      encryptedMsg = String.fromCharCode.apply(null, encryptedMsgBytes),
      encodedEncryptedMessage = window.btoa(String.fromCharCode.apply(null, ivec) + encryptedMsg);
  newMessage('info', "encrypt message for server " +
  "<i class=\"info-btn\" onclick=\"" +
    "explain(\'diffie-encrypt\', " +
    "[\'m\', \'" + msg.substr(0, 5) + "...\', \'k\', \'" + sharedKey.substr(0, 5) + "...\', \'e\', \'" + encodedEncryptedMessage + "\']" +
    ")\">" +
  "(info)</i>");
  s.send(encodedEncryptedMessage);
}

// Simplify client command parsing
function parseCommand(msg) {
  var command = msg.substr(0, 2);
  switch (command) {
    case '/p':
      msg = uid + ': ' + msg.substring(2).trim();
      var signedMessage = r.sign(msg) + '#' + msg,
          partnerKey = keys[partnerUid],
          encryptedMsg = uidEncrypt(partnerKey, signedMessage);
      sendMessage('/p ' + encryptedMsg);
      break;
    case '/o':
      partnerUid = msg.substring(2).trim();
      sendMessage('/o ' + partnerUid);
      if(keys[partnerUid] === undefined) {
        newMessage('server', 'requesting key from partner');
        sendMessage('/kr ' + partnerUid);
      }
      break;
    case '/s':
      sendMessage('/s');
      break;
    case '/h':
      explain('help', []);
      return;
    default:
      newMessage('server', 'unknown command');
      break;
  }
}

// Extract sender from message
function deriveSender(msg) {
  return msg.substr(0, msg.indexOf(':'));
}

/**
 * Handle client events
 */

// Handle connect event to server
s.onopen = function() {
  newMessage('server', 'connected to server');
  newMessage('server', 'use /h for help');
  var ping = function() {
    s.send('ping');
    setTimeout(ping, 5000);
  };
  ping();
};

// Handle disconnect event
s.onclose = function() {
  newMessage('server error', 'disconnected from server');
};

// Handle new message from the server
s.onmessage = function onmessage(m) {
  if(m.data && deriveSender(m.data) === 'shared') {
    var messageParts = m.data.substring('shared'.length + 1)
                             .split(' ')
                             .map(function(n) {
                               return parseInt(n);
                             }),
        serverPart = messageParts[0],
        publicPrime = messageParts[1],
        publicGenerator = messageParts[2],
        secret = randomNumber(5),
        sharedKeyMyPart = powermod(publicGenerator, secret, publicPrime),
        sharedKeyTemp = powermod(serverPart, secret, publicPrime);
    sharedKey = sha256(sharedKeyTemp.toString(16).toUpperCase());
    s.send('/d ' + sharedKeyMyPart);
    newMessage('info', "shared key parameters received " +
    "<i class=\"info-btn\" onclick=\"" +
      "explain(\'diffie-init\', " +
      "[\'sp\', " + serverPart + ", \'p\', " + publicPrime + ", \'g\', " + publicGenerator + "]" +
      ")\">" +
    "(info)</i>");
    newMessage('info', "shared key initialized " +
    "<i class=\"info-btn\" onclick=\"" +
      "explain(\'diffie-finish\', " +
      "[\'sk\', \'" + sharedKey + "\', \'sp\', " + serverPart + ", \'p\', " + publicPrime + ", \'g\', " + publicGenerator + ", \'s\', " + secret + ", \'mp\', " + sharedKeyMyPart + ",  \'k\', \'" + sharedKeyTemp + "\']" +
      ")\">" +
    "(info)</i>");
    sendMessage('/k ' + r.n + ' ' + r.public);
    newMessage('server', 'public key published');
    return;
  } else if(m.data) {
    var decodedMessage = window.atob(m.data),
        decodedMessageBytes = toAsciiArray(decodedMessage.substring(16)),
        ivec = toAsciiArray(decodedMessage.substr(0, 16)),
        aesCounter = new aesjs.Counter(ivec),
        aes = new aesjs.ModeOfOperation.ctr(aesjs.utils.hex.toBytes(sharedKey), aesCounter),
        decryptedMessageBytes = aes.decrypt(decodedMessageBytes),
        decryptedMessage = aesjs.utils.utf8.fromBytes(decryptedMessageBytes);
    newMessage('info', "decrypt message from server " +
    "<i class=\"info-btn\" onclick=\"" +
      "explain(\'diffie-decrypt\', " +
      "[\'m\', \'" + m.data.substr(0, 5) + "...\', \'k\', \'" + sharedKey.substr(0, 5) + "...\', \'d\', \'" + decryptedMessage + "\']" +
      ")\">" +
    "(info)</i>");
    return onmessage(decryptedMessage);
  }
  var sender = deriveSender(m),
      message = m.substring(sender.length + 1);
  switch (sender) {
    case 'server':
      newMessage('server', message);
      break;
    case 'uid':
      uid = message;
      newMessage('server', 'your id is: ' + message);
      break;
    case 'key':
      var messageParts = message.substring(1).split(' '),
          keyUser = messageParts[0],
          keyN = parseInt(messageParts[1]),
          keyKey = parseInt(messageParts[2]);
      keys[keyUser] = {
        'k': keyKey,
        'n': keyN
      };
      newMessage('server', 'received key from ' + keyUser);
      break;
    default:
      var messageParts = message.split(' '),
          messageDecrypt = function() {
            var decryptedMessage = r.decrypt(messageParts[1]),
                messageSignature = decryptedMessage.substr(0, decryptedMessage.indexOf('#')),
                decryptedMessageContent = decryptedMessage.substring(messageSignature.length + 1);
            if(uidVerify(keys[sender], decryptedMessageContent, messageSignature)) {
              newMessage('partner', decryptedMessageContent);
            } else {
              newMessage('server error', 'received incorrect signature');
            }
          },
          waitForKey = function() {
            if(keys[sender] === undefined) {
              setTimeout(waitForKey, 500);
            } else {
              messageDecrypt();
            }
          };
      if(keys[sender] == undefined) {
        sendMessage('/kr ' + sender);
        waitForKey();
      } else {
        messageDecrypt();
      }
      break;
  }
};

/**
 * Handle interface events
 */

// Handle enter key for inpMessage field
inpMessage.onkeyup = function(e) {
  e.preventDefault();
  if(e.keyCode === 13) {
    btnSend.click();
  }
};

// Handle send button click
btnSend.onclick = function() {
  var message = inpMessage.value;
  inpMessage.value = '';
  message = message.replace(/&/g, "&amp;")
                   .replace(/</g, "&lt;")
                   .replace(/>/g, "&gt;");
  message = message.trim();
  newMessage('me', message);
  parseCommand(message);
};

parseCommand('/h')

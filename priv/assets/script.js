/* Copyright 2018, Daniel Oltmanns (https://github.com/oltdaniel) */

// General helper functions
function e(id) {
  return document.getElementById(id);
}

var el_history = e('message-history'),
    inp_message = e('message-content'),
    btn_send = e('message-send');

// Chat functions
function newMessage(type, content) {
  console.log(content);
  el_history.innerHTML += "<div class=\"message " + type + "\">" + content + "</div>";
}

// Initialize websocket connection
var s = new WebSocket('ws://' + window.location.hostname + ':' + window.location.port + '/ws');

s.onopen = function() {
  newMessage('server', 'connected to server');
};

s.onclose = function() {
  newMessage('server error', 'disconnected from server');
};

s.onmessage = function(m) {
  var sender = m.data.substr(0, m.data.indexOf(':'));
  if(sender == 'server') {
    newMessage('server', m.data.substring(sender.length + 1));
  } else if(sender == 'uid') {
    var uid = m.data.substring(4);
    newMessage('server', 'your id is: ' + uid);
  } else {
    newMessage('partner', m.data);
  }
  el_history.scrollTo(0, el_history.scrollHeight);
};

// Listen for click event
btn_send.onclick = function() {
  var m = inp_message.value;
  m = m.replace(/&/g, "&amp;")
       .replace(/</g, "&lt;")
       .replace(/>/g, "&gt;");
  s.send(m);
  newMessage('me', m);
  inp_message.value = '';
};

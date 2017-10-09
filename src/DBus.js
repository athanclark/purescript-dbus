"use strict";

var dbus = require('dbus-native');

exports.connectSessionImpl = function connectSessionImpl () {
  return dbus.sessionBus();
};

exports.getServiceImpl = function getServiceImpl (c,b) {
  var service = c.getService(b);
  if (service.hasOwnProperty("getInterface")) {
    return service;
  } else {
    return null;
  }
};

exports.getInterfaceImpl = function getInterfaceImpl (s,o,i,f) {
  s.getInterface(o,i,f);
};

exports.callImpl = function callImpl (c,b,o,i,m,s,xs,f) {
  c.invoke({
    path: o,
    destination: b,
    interface: i,
    member: m,
    signature: s,
    body: xs
  }, function (me,x) {
    if (me) console.log('call error!',me);
    f(me,x);
  });
};

exports.onImpl = function onImpl (i,m,f) {
  i.on(m,f);
};

exports.signalDescImpl = function signalDescImpl (s,sd) {
  return [s,sd];
};

exports.exportInterfaceImpl = function exportInterfaceImpl (s,o,id) {
  var desc = {};
  var impl = {};
  for (var k in id) {
    if (id.hasOwnProperty(k)) {
      var x = id[k];
      desc[k] = x.desc;
      impl[k] = x.func;
    }
  }
  s.exportInterface(impl,o,desc);
};

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

exports.callImpl = function callImpl (i,m,xs,f) {
  if (i.hasOwnProperty(m)) {
    var args = xs.slice();
    args.push(f);
    i[m].apply(this,args);
    return true;
  } else {
    return false;
  }
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

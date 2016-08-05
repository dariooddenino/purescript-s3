'use strict';

exports._init = function _init(options) {
  var aws = require('aws-sdk');
  return new aws.S3(options);
};

exports.traceAny = function traceAny(x) {
  return function() {
    console.log(x);
    return {};
  };
};

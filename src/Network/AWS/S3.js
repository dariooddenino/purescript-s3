'use strict';

exports._init = function _init(options) {
  var aws = require('aws-sdk');
  return new aws.S3(options);
};

exports._listBuckets = function _listBuckets(s3, onError, onSuccess) {
  return function() {
    s3.listBuckets(function(err, data) {
      if (err) {
        onError(err)();
      } else {
        onSuccess(data)();
      }
    });
  };
};

exports.traceAny = function traceAny(x) {
  return function() {
    console.log(x);
    return {};
  };
};

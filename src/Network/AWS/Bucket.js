'use strict';

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

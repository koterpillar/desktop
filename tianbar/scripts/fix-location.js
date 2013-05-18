define(['jquery'], function ($) {
  var api_key = 'a54ebf597a8e77198d9a47c798d42c51';
  var my_geolocation;

  navigator.geolocation = {};
  navigator.geolocation.getCurrentPosition = function (cb) {
    if (my_geolocation) {
      cb(my_geolocation);
    } else {
      $.getJSON('http://freegeoip.net/json/').success(function (loc) {
        my_geolocation = { coords: loc };
        $.getJSON('http://query.yahooapis.com/v1/public/yql?q=' +
          'select%20place.woeid%20from%20flickr.places%20where%20' +
          'lat%3D' + loc.latitude + '%20and%20' +
          'lon%3D' + loc.longitude + '%20and%20' +
          'api_key=%22' + api_key + '%22&format=json')
        .success(function (yloc) {
          my_geolocation.woeid = yloc.query.results.places.place.woeid;
          cb(my_geolocation);
        });
      });
    }
  };
});

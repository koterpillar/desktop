require.config({
  paths: {
    "jquery": "../components/jquery/jquery.min"
  }
});
require(['jquery', 'widgets/time'], function ($) {
  function unescapeHTML(html) {
    return $('<span/>').html(html).text();
  }

  function updateWeather() {
    $.ajax('http://weather.yahooapis.com/forecastrss?' +
      'w=' + window.my_geolocation.woeid + '&u=c')
    .success(function (weather) {
      var temp = $('condition', weather).attr('temp');
      var units = $('units', weather).attr('temperature');
      units = " &deg;" + units;
      $('#weather').html(temp + units);
      var forecast = $('forecast', weather).map(function (k, el) {
        el = $(el);
        return el.attr('day') + ": " + el.attr('text') + ", " +
          el.attr('low') + "&ndash;" + el.attr('high') +
          units;
      });
      forecast = Array.prototype.join.call(forecast, "; ");
      $('#weather').attr('title', unescapeHTML(forecast));
    });
  }

  function setupWeather() {
    updateWeather();
    setInterval(updateWeather, 5 * 60 * 1000);
  }

  function setupLocation() {
    var api_key = 'a54ebf597a8e77198d9a47c798d42c51';
    $.getJSON('http://freegeoip.net/json/')
    .success(function (loc) {
      window.my_geolocation = loc;
      $.getJSON('http://query.yahooapis.com/v1/public/yql?q=' +
        'select%20place.woeid%20from%20flickr.places%20where%20' +
        'lat%3D' + loc.latitude + '%20and%20' +
        'lon%3D' + loc.longitude + '%20and%20' +
        'api_key=%22' + api_key + '%22&format=json')
      .success(function (yloc) {
        window.my_geolocation.woeid = yloc.query.results.places.place.woeid;
        locationReady();
      });
    });
  }

  function setupBackground() {
    $.ajax('gsettings:org.gnome.desktop.background/picture-uri')
    .success(function (background) {
      $('body').css('background-image',
        "url('" + background + "')");
      $('body').css('background-size',
        screen.width + "px " + screen.height + "px");
    });
    $('html').css('height', $(document).height());
  }

  function locationReady() {
    setupWeather();
  };

  window.setXMonadStatus = function (status) {
    $('#status').html(status);
  }

  $(document).ready(function () {
    setupLocation();
    setupBackground();
    if (window.XMonadStatus) {
      window.setXMonadStatus(window.XMonadStatus);
    }
  });
});

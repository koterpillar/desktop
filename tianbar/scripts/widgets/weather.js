define(['jquery', 'fix-location'], function ($) {
  var position;

  function unescapeHTML(html) {
    return $('<span/>').html(html).text();
  }

  function updateWeather() {
    $.ajax('http://weather.yahooapis.com/forecastrss?' +
      'w=' + position.woeid + '&u=c')
    .success(function (weather) {
      var temp = $('condition', weather).attr('temp');
      var units = $('units', weather).attr('temperature');
      units = " &deg;" + units;
      var forecast = $('forecast', weather).map(function (k, el) {
        el = $(el);
        return el.attr('day') + ": " + el.attr('text') + ", " +
          el.attr('low') + "&ndash;" + el.attr('high') +
          units;
      });
      forecast = Array.prototype.join.call(forecast, "; ");
      $('.widget-weather').html(temp + units);
      $('.widget-weather').attr('title', unescapeHTML(forecast));
    });
  }

  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(function (pos) {
      position = pos;
      updateWeather();
      setInterval(updateWeather, 5 * 60 * 1000);
    });
  });
});

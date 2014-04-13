require.config({
  paths: {
    'jquery': '../bower_components/jquery/dist/jquery.min',
    'moment': '../bower_components/moment/min/moment-with-langs.min',
    'tianbar': 'tianbar:///data/scripts'
  }
});
require(
  [
    'jquery',
    'moment'
  ],
  function ($, moment) {
    // Force 24 hour time
    var lang = navigator.language;
    if (!moment.langData(lang)) {
      lang = lang.replace(/-.+/, '');
    }
    moment.langData(lang)._longDateFormat.LT = 'HH:mm';

    require([
      'tianbar/power',
      'tianbar/time',
      'tianbar/volume',
      'tianbar/weather',
      'tianbar/xmonad'
    ], function (power, time, volume, weather, xmonad) {
      function adjustWidth() {
        var rightWidth = 0;
        $('.widget-right').each(function (_, w) {
          rightWidth += $(w).width();
        });
        $('.widget-xmonad').width($(document).width() - rightWidth - 60);
        $('.title').width(
          $('.widget-xmonad').width() -
          $('.workspaces').width() -
          $('.layout').width() -
          20
        );
      }

      $(document).ready(function () {
        $('html').css('height', $(document).height());
      });

      xmonad.change.add(adjustWidth);
      power.updated.add(adjustWidth);
    });
  }
);

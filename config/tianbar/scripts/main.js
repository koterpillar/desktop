require.config({
  paths: {
    'tianbar': 'tianbar:///data/scripts',
    'jquery': 'tianbar:///data/scripts/vendor/jquery',
    'moment': 'tianbar:///data/scripts/vendor/moment'
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
      'tianbar/time',
      'tianbar/ibus',
      'tianbar/power',
      'tianbar/volume',
      'tianbar/weather',
      'tianbar/xmonad'
    ], function (time, ibus, power, volume, weather, xmonad) {
      function adjustWidth() {
        var rightWidth = 0;
        $('.widget-right').each(function (_, w) {
          rightWidth += $(w).width() + 15;
        });
        $('.widget-xmonad').width($(document).width() - rightWidth - 20);
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
      ibus.updated.add(adjustWidth);
    });
  }
);

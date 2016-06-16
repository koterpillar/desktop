/* jshint esversion: 6 */
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
    if (!moment.localeData(lang)) {
      lang = lang.replace(/-.+/, '');
    }
    const to24hour = (fmt) =>
      fmt.replace('hh:mm A', 'HH:mm').replace('h:mm A', 'H:mm');
    const dateFormats = moment.localeData(lang)._longDateFormat;
    for (const fmt in dateFormats) {
      if (dateFormats.hasOwnProperty(fmt)) {
        dateFormats[fmt] = to24hour(dateFormats[fmt]);
      }
    }

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

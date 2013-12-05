require.config({
  paths: {
    'jquery': '../bower_components/jquery/jquery.min',
    'moment': '../bower_components/moment/min/moment-with-langs.min',
    'tianbar': 'tianbar:scripts'
  }
});
require(
  [
    'jquery',
    'tianbar/location_shim',
    'moment'
  ],
  function ($, location_shim, moment) {
    // Force 24 hour time
    var lang = navigator.language;
    if (!moment.langData(lang)) {
      lang = lang.replace(/-.+/, '');
    }
    moment.langData(lang)._longDateFormat.LT = 'HH:mm';

    require([
      'tianbar/time',
      'tianbar/weather',
      'tianbar/xmonad'
    ]);

    $(document).ready(function () {
      $('html').css('height', $(document).height());
    });
  }
);

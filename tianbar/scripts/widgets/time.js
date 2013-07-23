define(['jquery', 'moment', 'moment/lang'], function ($, moment) {
  var config = {
    format: 'llll'
  };

  function updateClock() {
    var dt = moment();
    var clockText = dt.format(config.format);
    $('.widget-time').text(clockText);
  }

  $(document).ready(function () {
    var lang = navigator.language;
    if (moment.langData(lang)) {
      moment.lang(lang);
    } else {
      moment.lang(navigator.language.replace(/-.+/, ''));
    }
    // Force 24 hour time
    moment.langData("en")._longDateFormat.LT = 'HH:mm';
    updateClock();
    setInterval(updateClock, 1000);
  });

  return config;
});

define(['jquery'], function ($) {
  timeLocaleSupport = false;
  try {
    new Date().toLocaleTimeString("i");
  } catch (e) {
    timeLocaleSupport = e.name === 'RangeError';
  }

  function timeString(date) {
    if (timeLocaleSupport) {
      return date.toLocaleTimeString(undefined, {
        hour12: false,
        hour: "numeric",
        minute: "numeric",
        second: undefined
      });
    } else {
      function pad(s) {
        return ((''+s).length < 2 ? '0' : '') + s;
      }
      return pad(date.getHours()) + ":" + pad(date.getMinutes());
    }
  }

  function updateClock() {
    var dt = new Date();
    var clockText = dt.toLocaleDateString() + " " + timeString(dt);
    $('.widget-time').text(clockText);
  }

  $(document).ready(function () {
    updateClock();
    setInterval(updateClock, 1000);
  });
});

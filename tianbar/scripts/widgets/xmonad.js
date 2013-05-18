define(['jquery'], function ($) {
  window.setXMonadStatus = function (status) {
    window.XMonadStatus = undefined;
    $('.widget-xmonad').html(status);
  }

  $(document).ready(function () {
    window.setTimeout(function () {
      if (window.XMonadStatus) {
        window.setXMonadStatus(window.XMonadStatus);
      }
    }, 1000);
  });
});

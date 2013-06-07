define(['jquery'], function ($) {
  function setStatus(st) {
    $('.widget-xmonad').html(st);
  }

  window.setXMonadStatus = function (st) {
    window.XMonadStatus = undefined;
    setStatus(st);
  }

  $(document).ready(function () {
    window.setTimeout(function () {
      if (window.XMonadStatus) {
        window.setXMonadStatus(window.XMonadStatus);
      }
    }, 1000);
  });
});

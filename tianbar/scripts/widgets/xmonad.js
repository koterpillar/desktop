define(['handlebars', 'jquery'], function (Handlebars, $) {
  var template;
  function setStatus(status) {
    $('.widget-xmonad').html(template(status));
  }

  window.setXMonadStatus = function (status) {
    window.XMonadStatus = undefined;
    if (typeof status === "string") {
      status = JSON.parse(status);
    }
    setStatus(status);
  }

  $(document).ready(function () {
    template = Handlebars.compile($('#xmonad-layout').html());
    window.setTimeout(function () {
      if (window.XMonadStatus) {
        window.setXMonadStatus(window.XMonadStatus);
      }
    }, 1000);
  });
});

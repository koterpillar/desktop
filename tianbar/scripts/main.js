require.config({
  paths: {
    "jquery": "../components/jquery/jquery.min"
  }
});
require(['jquery', 'widgets/time', 'widgets/weather'], function ($) {
  function setupBackground() {
    $.ajax('gsettings:org.gnome.desktop.background/picture-uri')
    .success(function (background) {
      $('body').css('background-image',
        "url('" + background + "')");
      $('body').css('background-size',
        screen.width + "px " + screen.height + "px");
    });
    $('html').css('height', $(document).height());
  }

  window.setXMonadStatus = function (status) {
    $('#status').html(status);
  }

  $(document).ready(function () {
    setupBackground();
    if (window.XMonadStatus) {
      window.setXMonadStatus(window.XMonadStatus);
    }
  });
});

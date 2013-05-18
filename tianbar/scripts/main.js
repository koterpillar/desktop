require.config({
  paths: {
    "jquery": "../components/jquery/jquery.min"
  }
});
require(
  [
    'jquery',
    'widgets/time',
    'widgets/weather',
    'widgets/xmonad'
  ],
  function ($) {
    $(document).ready(function () {
      $.ajax('gsettings:org.gnome.desktop.background/picture-uri')
      .success(function (background) {
        $('body').css('background-image',
          "url('" + background + "')");
        $('body').css('background-size',
          screen.width + "px " + screen.height + "px");
      });
      $('html').css('height', $(document).height());
    });
  }
);

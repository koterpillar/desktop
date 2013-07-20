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
      $('html').css('height', $(document).height());
    });
  }
);

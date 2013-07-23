require.config({
  paths: {
    'jquery': '../components/jquery/jquery.min',
    'moment': '../components/moment/min/moment.min',
    'moment/lang': '../components/moment/min/langs.min'
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

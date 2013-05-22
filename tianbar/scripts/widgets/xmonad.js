define(['handlebars', 'jquery'], function (Handlebars, $) {
  var template;
  function setStatus(st) {
    $('.widget-xmonad').html(template(st));
  }

  window.setXMonadStatus = function (st) {
    window.XMonadStatus = undefined;
    if (typeof st === "string") {
      st = JSON.parse(st);
    }
    for (var i = 0; i < st.workspaces.length; i++) {
      wksp = st.workspaces[i];
      if (wksp.tag === "4") {
        wksp.icon = "code-fork";
      } else if (wksp.tag === "6") {
        wksp.icon = "envelope";
      } else if (wksp.tag === "7") {
        wksp.icon = "comment-alt";
      }
    }
    setStatus(st);
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

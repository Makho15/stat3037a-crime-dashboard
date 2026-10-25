// Replays the page-banner animation each time a tab is opened.
(function () {
  function restart(el) {
    var nodes = [el].concat(Array.prototype.slice.call(el.querySelectorAll('*')));
    nodes.forEach(function (n) { n.style.animation = 'none'; });
    void el.offsetWidth;
    nodes.forEach(function (n) { n.style.animation = ''; });
  }
  document.addEventListener('shown.bs.tab', function () {
    var b = document.querySelector('.tab-pane.active .pbanner');
    if (b) restart(b);
  });
})();

/* plugins/motion.js (Motion Highcharts Plugin v1.0.9) calls Highcharts.each,
   which current Highcharts versions no longer define, so every chart with
   motion enabled threw "Highcharts.each is not a function". Define the helper
   with Array.prototype.forEach only when Highcharts lacks it. */
(function (H) {
  if (H && typeof H.each !== "function") {
    H.each = function (arr, fn, ctx) {
      return Array.prototype.forEach.call(arr, fn, ctx);
    };
  }
}(window.Highcharts));

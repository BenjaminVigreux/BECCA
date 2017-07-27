<script type="text/javascript">
var wasBusy = false;
var elapsedTimer = null;
var startTime = null;
function updateBusy() {
  var isBusy = $('html').hasClass('shiny-busy');
  if (isBusy && !wasBusy) {
    startTime = new Date().getTime();
    setTimeout(function() {
    elapsedTimer = setInterval(function() {
      var millisElapsed = new Date().getTime() - startTime;
      $('#progress').text(Math.floor(Math.round(millisElapsed/1000)/60) + ' minutes and ' + Math.round(millisElapsed/1000) - (Math.floor(Math.round(millisElapsed/1000)/60) * 60) + ' seconds elapsed');
    }, 1000)}, 1000);
  }
  else if (!isBusy && wasBusy) {
    clearInterval(elapsedTimer);
  }
  wasBusy = isBusy;
}
</script>
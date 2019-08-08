function copyUrl() {
  var copyText = document.getElementById("Url");
  copyText.select();
  document.execCommand("copy");
} 

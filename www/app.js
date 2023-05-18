$(document).ready(function(){
  $(".logo").attr("data-toggle",'offcanvas');
  $(".logo").attr("role",'button');
});

function getCookies(){
  var res = Cookies.get();
  Shiny.setInputValue('cookies', res);
}

$(document).on('shiny:connected', function(ev){
  getCookies();
})

Shiny.addCustomMessageHandler('cookie-set', function(msg){
  Cookies.set(msg.name, msg.value, { expires: 7 });
  getCookies();
})

Shiny.addCustomMessageHandler('cookie-remove', function(msg){
  Cookies.remove(msg.name);
  getCookies();
})
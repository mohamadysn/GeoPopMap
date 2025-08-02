console.log("custom.js chargé !");

$( document ).ready(function() {
 $("#sidebarPanel").on("shown", function(){
      $(this).css({"overflow-y": "visible"});
    }).on("hidden", function(){
      $(this).css({"overflow-y": "auto"});
    });

});

$(document).on('click', '.toggle-box-btn', function () {
                     var target = $(this).data('target');
                     $('#' + target).slideToggle(200);
                   });

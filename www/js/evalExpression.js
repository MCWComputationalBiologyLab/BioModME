Shiny.addCustomMessageHandler('executeJS', function(message) {
    eval(message.js);
  });
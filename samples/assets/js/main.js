(function (config) {
  new OktaSignIn(config).renderEl({ el: '#sign-in-widget' }, function(){});
})(oktaWidgetConfig);
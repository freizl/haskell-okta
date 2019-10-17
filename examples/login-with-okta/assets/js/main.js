(function (config) {
  const widgetConfig = Object.assign(
    {
      i18n: {
        en: {
          'primaryauth.title': 'Sign in to Sample App'
        }
      },
    },
    config,
  );
  new OktaSignIn(widgetConfig).renderEl({ el: '#sign-in-widget' }, function(){});
})(oktaWidgetConfig);

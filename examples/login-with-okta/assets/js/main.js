(function (config) {
  const widgetConfig = Object.assign(
    {
      i18n: {
        en: {
          'primaryauth.title': 'Sign in to Sample App'
        }
      },
      idps: [
        { type: 'APPLE', id: '0oaz2emOZGUKjuZwX0g3'}
      ],
      idpDisplay: 'PRIMARY',
    },
    config,
  );
  widgetConfig.authParams.pkce = false;
  new OktaSignIn(widgetConfig).renderEl({ el: '#sign-in-widget' }, function(){});
})(oktaWidgetConfig);

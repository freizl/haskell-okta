(function (widgetRenderType, config) {

  const authParams = {
    ...config.authParams,
  };

  let redirectUri = config.redirectUri;

  if (widgetRenderType === 'code') {
    Object.assign(authParams, {
      responseType: 'code',
      pkce: false,
    });
  } else if (widgetRenderType === 'implicit') {
    Object.assign(authParams, {
      responseType: ['id_token', 'token'],
      pkce: false,
      // display: 'page',
    });
  } else if (widgetRenderType === 'pkce') {
    Object.assign(authParams, {
      responseType: 'code',
    });

    redirectUri = redirectUri.replace('authorization-code/callback', 'authorization-code/pkce');
  }

  const widgetConfig = {
    ...config,
    authParams,
    redirectUri,
    i18n: {
      en: {
        'primaryauth.title': 'Sign in to Sample App'
      }
    },
    Xidps: [
      { type: 'APPLE', id: '0oaz2emOZGUKjuZwX0g3'}
    ],
    idpDisplay: 'PRIMARY',
  };

  console.table(widgetConfig);
  
  const osi = new OktaSignIn(widgetConfig);
  osi.renderEl({ el: '#sign-in-widget' }, function(){});

  window.osi = osi;
})(widgetRenderType, oktaWidgetConfig);

(function(widgetRenderType, config) {

  const reverseEngAuthTokenForSharing = (user, idToken, accessToken) => {
    const claims = idToken.claims;
    const rawTokenResp = {
      scope: idToken.scopes.join(' '),
      id_token: idToken.idToken,
      access_token: accessToken.accessToken,
      token_type: accessToken.tokenType,
      expires_in: accessToken.expiresAt - Math.floor(Date.now() / 1000),
    };
    const cookieValue = JSON.stringify([claims, user, rawTokenResp]);
    document.cookie = `login-with-okta-sample-cookie-user=${cookieValue}; path=/`;
  };

  const handleResp = (resp) => {
    console.log(resp);
    if (resp.tokens) {
      const idToken = resp.tokens.idToken;
      const accessToken = resp.tokens.accessToken;
      osi.authClient.tokenManager.add('idToken', idToken);
      osi.authClient.tokenManager.add('accessToken', accessToken);
      osi.authClient.token.getUserInfo()
        .then(function(user) {  

          // HACK!!
          // 'decode' response to cookie so that shared with server side.
          reverseEngAuthTokenForSharing(user, idToken, accessToken);
          // window.location.href = '/profile';
          //
          window.osi.remove();

          const title = document.createElement('h1');
          title.innerText = `Hello, ${user.name}`;

          const pre = document.createElement('pre');
          pre.innerText = JSON.stringify(user, null, 2);

          [title, pre].forEach(c => {
            document.getElementsByTagName('body')[0].appendChild(c);
          });

        })
        .catch(function(err) {
          // handle OAuthError or AuthSdkError (AuthSdkError will be thrown if app is in OAuthCallback state)
        });
    }
  }

  const hasTokenInUrl = () => {
    const u = location.href;
    return u.indexOf('code=') > 0 || u.indexOf('access_token=') > 0 || u.indexOf('id_token=') > 0;
  }

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
      //display: 'page',
    });
    // HACK !!!
    redirectUri = redirectUri.replace('authorization-code/callback', 'authorization-code/implicit');
  } else if (widgetRenderType === 'pkce') {
    Object.assign(authParams, {
      responseType: 'code',
    });

    redirectUri = redirectUri.replace('authorization-code/callback', 'authorization-code/implicit');
  }
  authParams.scope.push('offline_access');
  const widgetConfig = {
    ...config,
    authParams,
    redirectUri,

    colors: {
      brand: '#8d6e97',
    },

    features: {
      rememberMe: false,
    },

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
  const ac = osi.authClient;

  if (hasTokenInUrl()) {
    ac.token.parseFromUrl().then(handleResp);
  } else {
    osi.renderEl({ el: '#sign-in-widget' }, handleResp);
  }

  window.osi = osi;
  window.ac = ac;
 
})(widgetRenderType, oktaWidgetConfig);

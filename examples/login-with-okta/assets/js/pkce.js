(function(config) {
  const widgetConfig = {
    ...config,

    i18n: {
      en: {
        'primaryauth.title': 'Hacking SPA App'
      }
    },
    colors: {
      brand: '#8d6e97',
    },
    features: {
      rememberMe: false,
    },
    logo: null,
  };

  const reverseEngAuthTokenForSharing = (user, idToken, accessToken) => {
    const claims = idToken.claims;
    const rawTokenResp = {
      scope: idToken.scopes.join(' '),
      id_token: idToken.idToken,
      access_token: accessToken.accessToken,
      token_type: accessToken.tokenType,
      expires_in: accessToken.expiresAt - Math.floor(Date.now()/1000),
    }
    const cookieValue = JSON.stringify([claims, user, rawTokenResp]);
    document.cookie = `login-with-okta-sample-cookie-user=${cookieValue}; path=/`;
  };

  const osi = new OktaSignIn(widgetConfig);

  osi.authClient.token.parseFromUrl()
     .then(resp => {
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
           window.location.href = '/profile';

           // const title = document.createElement('h1');
           // title.innerText = `Hello, ${user.name}`;

           // const pre = document.createElement('pre');
           // pre.innerText = JSON.stringify(user, null, 2);

           // [title, pre].forEach(c => {
           //   document.getElementsByTagName('body')[0].appendChild(c);
           // });
         })
         .catch(function(err) {
           // handle OAuthError or AuthSdkError (AuthSdkError will be thrown if app is in OAuthCallback state)
         });
       }
     })

  window.osi = osi;

})(oktaWidgetConfig);

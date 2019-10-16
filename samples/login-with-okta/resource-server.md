
The key part of implementing resource server is to verify the access token which has been demostrated in the `login with okta` sample application already.

Here are some obsolete code

```haskell

resourceServerHomeH :: ActionM ()
resourceServerHomeH = json $ WelcomeMessage "Hello!  There's not much to see here :) Please grab one of our front-end samples for use with this sample resource server"

messageOptionH :: ActionM ()
messageOptionH = do
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Headers" "authorization"
  setHeader "Access-Control-Allow-Methods" "GET,HEAD,PUT,PATCH,POST,DELETE"
  setHeader "Connection" "keep-alive"
  setHeader "Vary" "Access-Control-Request-Headers"
  status status204

isAuthH :: ActionM Bool
isAuthH = do
  authHeader <- header "Authorization"
  when (isNothing authHeader) (return False)
  -- TODO: verify access token
  -- let accossToken = drop (length "Bearer ") authHeader
  return (isJust authHeader)

unAuthHandler :: ActionM ()
unAuthHandler = status unauthorized401

messageGetH :: Config -> ActionM ()
messageGetH _ = do
  isauth <- isAuthH
  setHeader "Access-Control-Allow-Origin" "*"
  unless isauth unAuthHandler
  json $ MessageResponse
    [ Message "I am a robot."
    , Message "Hello, world!"
    ]

```

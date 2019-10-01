

{-# LANGUAGE OverloadedStrings #-}

module Specs.Token
  ( spec
  , sampleConfig
  ) where

import           Crypto.JOSE.Compact
import           Crypto.JOSE.Error
import           Crypto.JWT

import           Data.ByteString.Lazy (ByteString)
import           Data.Either
import           Test.Hspec

import           Okta.Samples.Token

import           Specs.Internal

successResp :: ByteString
successResp = "{\n  \"access_token\": \"SOME_TOKEN\",\n  \"token_type\": \"Bearer\",\n  \"expires_in\": 3600,\n  \"scope\": \"openid email profile\",\n  \"id_token\": \"eyJhbGciOiJSUzI1NiIsImtpZCI6IktJRF81M2I0MWVlZTQ4MzdhYTJhNDFiMTExODc0YzE5YzNhYyJ9.eyJzdWIiOiIwMHVrejZFMDZ2dHJHRFZuOTBnMyIsIm5hbWUiOiJKb2huIEFkYW1zIiwiZW1haWwiOiJqb2huQGFjbWUuY29tIiwidmVyIjoxLCJpc3MiOiJodHRwOi8vMTI3LjAuMC4xOjc3NzciLCJhdWQiOiJ6WVZOb05JZVN3dWwzMnZwTmlPeiIsImlhdCI6MTQ3ODM4ODIzMiwiZXhwIjoxNDgxNjc0NzMyLCJqdGkiOiJJRC5YYVI2dFA3b0hLa3c4MWxRYWFwMENJQ3l0R1B2eGZTTkgwZjR6SnkyQzFnIiwiYW1yIjoicHdkIiwiaWRwIjoiMDBva29zYVZKUFlKa1N3VmswZzMiLCJub25jZSI6IlNPTUVfTk9OQ0UiLCJwcmVmZXJyZWRfdXNlcm5hbWUiOiJqb2huQGFjbWUuY29tIiwiYXV0aF90aW1lIjoxNDc4Mzg4MjMyLCJhdF9oYXNoIjoibi1IazZLYmFndGNEZGFyS09WeUFLUSJ9.YQuS6RwAgcOI2BX-X38kxoUDLZ3Ye5-jETAzPni5YvOZ06csR4aTQ0tpo9ad7Z9JBLCAxeEj6LiCKNLSIHwCBSsKFd6ctX9Wo4OUhCk2pg3nuFgGydCPY_uYpxkHvsYNjxdPibNF3zalp5L9rDdW-nPBfz2VOZR0M8Cq_mQlXBo9ZiiymIsUdDFl160qMPIZWwjj_1ejA6CmEIokJt4TsVPow8IHqanZIGCAzakEEm1rmMyWDkky-lR9QSVvJLpOnYcMxhnsiMJCSdysscEFkQA07TTxs3ZhGxpTsL0g1AW6iKga2UBMeL17tdaCkKXGboeDSBePd8pg4NzSd9_Nuw\"\n}"

validIdToken :: ByteString
validIdToken = "eyJhbGciOiJSUzI1NiIsImtpZCI6IktJRF81M2I0MWVlZTQ4MzdhYTJhNDFiMTExODc0YzE5YzNhYyJ9.eyJzdWIiOiIwMHVrejZFMDZ2dHJHRFZuOTBnMyIsIm5hbWUiOiJKb2huIEFkYW1zIiwiZW1haWwiOiJqb2huQGFjbWUuY29tIiwidmVyIjoxLCJpc3MiOiJodHRwOi8vMTI3LjAuMC4xOjc3NzciLCJhdWQiOiJ6WVZOb05JZVN3dWwzMnZwTmlPeiIsImlhdCI6MTQ3ODM4ODIzMiwiZXhwIjoxNDgxNjc0NzMyLCJqdGkiOiJJRC5YYVI2dFA3b0hLa3c4MWxRYWFwMENJQ3l0R1B2eGZTTkgwZjR6SnkyQzFnIiwiYW1yIjoicHdkIiwiaWRwIjoiMDBva29zYVZKUFlKa1N3VmswZzMiLCJub25jZSI6IlNPTUVfTk9OQ0UiLCJwcmVmZXJyZWRfdXNlcm5hbWUiOiJqb2huQGFjbWUuY29tIiwiYXV0aF90aW1lIjoxNDc4Mzg4MjMyLCJhdF9oYXNoIjoibi1IazZLYmFndGNEZGFyS09WeUFLUSJ9.YQuS6RwAgcOI2BX-X38kxoUDLZ3Ye5-jETAzPni5YvOZ06csR4aTQ0tpo9ad7Z9JBLCAxeEj6LiCKNLSIHwCBSsKFd6ctX9Wo4OUhCk2pg3nuFgGydCPY_uYpxkHvsYNjxdPibNF3zalp5L9rDdW-nPBfz2VOZR0M8Cq_mQlXBo9ZiiymIsUdDFl160qMPIZWwjj_1ejA6CmEIokJt4TsVPow8IHqanZIGCAzakEEm1rmMyWDkky-lR9QSVvJLpOnYcMxhnsiMJCSdysscEFkQA07TTxs3ZhGxpTsL0g1AW6iKga2UBMeL17tdaCkKXGboeDSBePd8pg4NzSd9_Nuw"

spec :: Spec
spec = describe "Token" $
  it "parse success response" $ do
    let jwt = decodeCompact validIdToken :: Either Error JWT
    jwt `shouldSatisfy` isRight

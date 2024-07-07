{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}

module Main (main) where

import Data.Aeson
import Data.UUID (UUID)
import Formatting
import Prosumma
import Prosumma.Servant
import Network.Wai.Handler.Warp
import RIO
import RIO.ByteString (toStrict)
import RIO.Partial
import Servant
import Servant.Auth.Server

import qualified Data.UUID as UUID

data UserRole = UserRoleOwner | UserRoleTherapist deriving (Eq, Ord, Enum, Show)

instance ToJSON UserRole where
  toJSON UserRoleOwner = String "owner"
  toJSON UserRoleTherapist = String "therapist"

instance FromJSON UserRole where
  parseJSON = withText "UserRole" $ \text -> do
    case text of
      "owner" -> return UserRoleOwner
      "therapist" -> return UserRoleTherapist
      _ -> fail $ "Invalid role " ++ show text

data User = User {
  sub :: UUID,
  role :: UserRole,
  spa :: UUID
} deriving (Eq, Show, Generic)

instance ToJSON User where
instance FromJSON User where

instance ToJWT User
instance FromJWT User

newtype JWTResponse = JWTResponse {
  jwt :: Text 
} deriving (Generic, Show)

instance ToJSON JWTResponse

type Login = "login" :> Get '[JSON] JWTResponse

login :: JWTSettings -> RIO LogFunc JWTResponse
login jwtSettings = do
  let sub = fromJust $ UUID.fromText "92B12BD5-CD8D-426D-AD11-F84E35E25B2C"
  let spa = fromJust $ UUID.fromText "6C3B2274-69D0-42B3-BCAB-37266B3BE6CE"
  let user = User sub UserRoleOwner spa
  logDebug $ uformat shown user 
  token <- liftIO $ makeJWT user jwtSettings Nothing
  case token of
    Left err -> do
      logError $ uformat shown err
      throwIO err500
    Right token -> case decodeUtf8' (toStrict token) of 
      Left err -> do
        logError $ uformat shown err
        throwIO err500
      Right jwt -> return $ JWTResponse jwt

type Health = "health" :> Get '[PlainText] Text 

health :: Monad m => m Text
health = return "health"

type Bong = "bong" :> Get '[PlainText] Text
type Bang = "bang" :> Get '[PlainText] Text
type Protected = Bong :<|> Bang
type Unprotected = Login :<|> Health

bong :: User -> RIO LogFunc Text 
bong user = do 
  logDebug "Bong!!!"
  return "bong"

bang :: User -> RIO LogFunc Text
bang user = do
  logDebug "Bang!!!"
  return "bang"

type API = (Auth '[JWT] User :> Protected) :<|> Unprotected 

authenticated :: User -> ServerT Protected (RIO LogFunc)
authenticated user = bong user :<|> bang user

protected :: AuthResult User -> ServerT Protected (RIO LogFunc) 
protected (Authenticated user) = authenticated user 
protected _ = unauthenticated :<|> unauthenticated 
  where
    unauthenticated :: RIO LogFunc a
    unauthenticated = throwIO err401

unprotected :: JWTSettings -> ServerT Unprotected (RIO LogFunc)
unprotected jwtSettings = login jwtSettings :<|> health

proxy :: Proxy API
proxy = Proxy

api :: JWTSettings -> ServerT API (RIO LogFunc)
api jwtSettings = protected :<|> unprotected jwtSettings

main :: IO ()
main = do 
  logOptions <- logOptionsHandle stderr True
  withLogFunc logOptions $ \lf -> do
    key <- liftIO generateKey
    let jwtSettings = defaultJWTSettings key
    let context = jwtSettings :. defaultCookieSettings :. EmptyContext
    liftIO $ run 8080 $ runApplicationWithContext context loggingExceptionHandler id proxy (api jwtSettings) lf 

{-# LANGUAGE OverloadedStrings #-}

module Email where

import Network.Mail.Mime
import Network.Mail.SMTP
import Data.Text
import Data.Text.Lazy (fromStrict)

-- TODO: Use type safe route
domain :: Text
domain = "library.allthings.red"

from :: Address
from = Address (Just "Hedgiegon") ("hedgiegon@" <> domain)
to :: [Address]
to = [Address Nothing "neallred@gmail.com"]

passwordResetMessage :: Text
passwordResetMessage = Data.Text.unlines
  [ "Looks like you forgot your password! "
  , "You can reset it by following this link: "
  , "library.allthings.red/some-reset-token"
  ]

emptyText :: Data.Text.Text
emptyText = ""


signature :: Text
signature = Prelude.foldr (\x y -> x <> ("\n" :: Data.Text.Text) <> y) emptyText
  ([ ""
  , ""
  , "----------------------------------------"
  , ""
  , "-- The Hedgiegon"
  , ""
  , "                      ^ ^"
  , "                  <<,-----"
  , "                 <<,      \\_   o"
  , "      _____      <<,    O   \\_/    ("
  , "    _/     \\    <<,        / ``   ) \\"
  , "   /        \\   <<,       |<==EE|)   (" 
  , "  /     !    \\ <<,        |\\^^   \\(_)/"
  , " !   !  !     '<,         |"
  , " !   !   \\____',          |  ::"
  , " !   .   \\ __!\\,          |)::|"
  , "  \\   \\_!     <,           \\"
  , "   \\ !       <<,            _"
  , "    \\!        <<,            \\_  ::"
  , "              <<_             |)::|"
  , "               <<\\_         /"
  , "                 <<\\       /"
  , "                  <<._,__,/"
  , ""
  ] :: [Data.Text.Text])

lazyPlainText :: Text -> Part
lazyPlainText =  plainTextPart . fromStrict

cc :: [Address]
cc = []
bcc :: [Address]
bcc = []
subject :: Text
subject = "Email reset " <> domain
body :: Network.Mail.Mime.Part
body = lazyPlainText (passwordResetMessage <> signature)

mail :: Network.Mail.Mime.Mail
mail = Network.Mail.SMTP.simpleMail from to cc bcc subject [body]

sendingPasswordResetEmail :: IO ()
sendingPasswordResetEmail = do
  putStrLn "Sending password reset email"
  sendMail "localhost" mail
  putStrLn "Email sent"


{-# LANGUAGE OverloadedStrings #-}

import Network.Mail.SMTP
import Data.Text
import Data.Text.Lazy (fromStrict)

-- TODO: Use type safe route
domain = "library.allthings.red"

from = Address (Just "Hedgiegon") ("hedgiegon@" <> domain)
to = [Address Nothing "neallred@gmail.com"]

passwordResetMessage = Data.Text.unlines
  [ "Looks like you forgot your password! "
  , "You can reset it by following this link: "
  , "library.allthings.red/some-reset-token"
  ]

emptyText =  ("" :: Data.Text.Text)


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

lazyPlainText =  plainTextPart . fromStrict

cc = []
bcc = []
subject = "Email reset " <> domain
body = lazyPlainText (passwordResetMessage <> signature)

mail = simpleMail from to cc bcc subject [body]

sendingPasswordResetEmail = do
  putStrLn "Sending password reset email"
  sendMail "localhost" mail
  putStrLn "Email sent"


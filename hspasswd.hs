{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Control.Monad.Trans
import Data.Monoid

import Network.Wai

import Data.Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Environment
import System.Process
import System.Exit

main :: IO ()
main = scotty 3000 $ do

	get "/" $
		html $ mconcat ["<form method=POST action=\"cpw\">"
			,"<p>Old password:</p>"
			,"<input type=\"password\" name=password>"
			,"<p>New password:</p>"
			,"<input type=\"password\" name=newPassword>"
			,"<input type=submit>"
			,"</form>"
		]

	post "/cpw" $ do
		password <- param "password"
		newPassword <- param "newPassword"
		username <-  liftIO $ getEnv "REMOTE_USER"
		(exitcode, stdout, stderr) <- liftIO $ changePassword username password newPassword
		text $ pack $ outputText exitcode username stderr
	
changePassword username password newPassword = do
	(exitcode, stdout, stderr) <- readProcessWithExitCode "/usr/bin/su" ["-l", username, "-c", "passwd"] (password++"\n"++password++"\n"++newPassword++"\n"++newPassword++"\n")
	return (exitcode, stdout, stderr)

outputText exitcode username stderr
	| exitcode == ExitSuccess = "Password successfully changed for "++username++"."
	| otherwise = "Error: "++stderr

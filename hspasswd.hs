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

import Data.Maybe
import Text.StringTemplate

main :: IO ()
main = scotty 3000 $ do

	get "/" $ file "index.html"

	get "/images/:handle" $ do
		handle <- param "handle"
		file $ "images/" ++ unpack handle
	get "/style.css" $ file "style.css"
	
	post "/cpw" $ do
		password <- param "password"
		newPassword <- param "newPassword"
		username <-  liftIO $ getEnv "REMOTE_USER"
		(exitcode, stdout, stderr) <- liftIO $ changePassword username password newPassword
		page <- liftIO $ outputText exitcode username stderr
		html $ page
	get "/test" $ do
		page <- liftIO $ renderErrorPage "test"
		html $ page

changePassword username password newPassword = do
	(exitcode, stdout, stderr) <- readProcessWithExitCode "/usr/bin/su" ["-l", username, "-c", "passwd"] (password++"\n"++password++"\n"++newPassword++"\n"++newPassword++"\n")
	return (exitcode, stdout, stderr)

renderErrorPage error = do
	templateFile <- readFile "templates/webauth.csh.template.html"
	let template = newSTMP templateFile :: StringTemplate String
	let page = pack $ toString $ setAttribute "bodyText" (error :: String) template
	return page

outputText exitcode username stderr
	| exitcode == ExitSuccess = renderErrorPage ("Password successfully changed for "++username++".")
	| otherwise = renderErrorPage stderr

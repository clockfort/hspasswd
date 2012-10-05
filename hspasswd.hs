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
import Data.Map((!))  -- !!!
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = scotty 3006 $ do

	get "/" $ file "index.html"

	get "/images/:handle" $ do
		handle <- param "handle"
		file $ "images/" ++ unpack handle
	get "/style.css" $ file "style.css"
	
	post "/cpw" $ do
		password <- param "password"
		newPassword <- param "newPassword"
		req <- request
		let username = C.unpack ((Map.fromList $ requestHeaders req) ! "X-WEBAUTH-USER")
		(exitcode, stdout, stderr) <- liftIO $ changePassword username password newPassword
		page <- liftIO $ outputText exitcode username stderr
		html page

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
	| otherwise = renderErrorPage $ filterError stderr

filterError stderr =
	Prelude.concat [ Prelude.concat ["<p>", errorMap line, "</p>"] | line <- Prelude.lines stderr]

errorMap stderr
	| stderr == "Password: /usr/bin/su: incorrect password" = "Current password entry was incorrect."
	| stderr == "passwd: Have exhausted maximum number of retries for service" = ""
	| stderr == "Password: (current) UNIX password: New password: BAD PASSWORD: The password fails the dictionary check - it is based on a dictionary word" = ""
	| stderr == "Password: (current) UNIX password: New password: BAD PASSWORD: The password is shorter than 7 characters" = ""
	| otherwise = stderr

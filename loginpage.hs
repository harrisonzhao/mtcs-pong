-- netstat -a -o -n
-- taskkill /F /PID ####
-- stack runghc loginpage.hs
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Login to Pong"
    toWidgetHead
        [hamlet|
            <meta charset="utf-8">
        |]
    [whamlet|
    <h1>Login to Pong Web App</h1>
    <div class="login">
    <form method="post" action="index.html">
    <p><input type="text" name="login" value="" placeholder="Username or Email"></p>
    <p><input type="password" name="password" value="" placeholder="Password"></p>
    
    <p class="submit">
    <input type="submit" name="commit" value="Login">
    |]

main :: IO ()
main = warp 3000 HelloWorld
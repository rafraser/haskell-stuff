echo off
elm make client/Main.elm --optimize --output=public/js/elm.js
uglifyjs public/js/elm.js --compress --output=public/js/elm.js | uglifyjs public/js/elm.js --mangle --output=public/js/elm.js
echo Elm code built and minified successfully
PAUSE
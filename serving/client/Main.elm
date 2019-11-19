module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, img)
import Html.Attributes exposing (class, src)
import Http
import Json.Decode exposing (Decoder, field, string, map2, list, int)

-- MAIN
main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL
type alias Model = 
    { users: Maybe (List User)
    }
    
type alias User =
    { name : String
    , score: Int
    }
    
init : () -> (Model, Cmd Msg)
init _ = 
    ( {users = Nothing}, loadUserData )
    
-- UPDATE
type Msg = GotData (Result Http.Error (List User))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok users ->
                    ({model | users = Just users}, Cmd.none)
                Err e ->
                    (model, Cmd.none)
    
-- SUBSCRIPTIONS?
subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> Html Msg
view model =
    case model.users of
        Nothing ->
            div [class "title"] [text "Loading..."]
        
        Just data ->
            div [class "userList"] ( List.append [div [class "title"] [text "User List"]] (renderUsers data) )
            
         
renderUser user =
    div [class "user"] 
        [
          div [class "userScore"] [text (String.fromInt user.score)]
        , div [class "userName"] [text user.name]
        ]
    
renderUsers users =
    List.map renderUser users
           
-- HTTP
loadUserData : Cmd Msg
loadUserData =
    Http.get
    { url = "http://localhost:8080/users"
    , expect = Http.expectJson GotData usersDecoder
    }
    
usersDecoder : Decoder (List User)
usersDecoder =
    list userDecoder
         
userDecoder : Decoder User
userDecoder =
    map2 User
        (field "name" string)
        (field "score" int)
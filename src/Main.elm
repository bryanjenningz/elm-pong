module Main exposing (..)

{- A multiplayer pong game.
   Ideas:
       A "play online" option
       2-player and 4-player option
       A single-player practice option where you practice against the wall
       A single-player option where the ball goes in random directions
       A single-player option against a computer
       Multiple difficulty levels for the computer player
       Add spin to the ball if the user is moving while the ball hits their paddle
       Power-ups like speed mode, big paddle mode, small paddle mode, random direction mode
-}

import Html exposing (..)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes as SvgAttr exposing (..)
import Keyboard exposing (KeyCode)
import Time exposing (Time, millisecond)


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | Tick Time


type alias KeysDown =
    { w : Bool, s : Bool, up : Bool, down : Bool }


type alias Ball =
    { x : Float, y : Float, vx : Float, vy : Float, width : Float, height : Float }


type alias Paddle =
    { x : Float, y : Float, width : Float, height : Float }


type alias Model =
    { leftPaddle : Paddle, rightPaddle : Paddle, ball : Ball, keysDown : KeysDown }


init : ( Model, Cmd Msg )
init =
    ( Model (Paddle 5 50 2 10) (Paddle 93 50 2 10) (Ball 50 50 1 0.5 2 2) (KeysDown False False False False), Cmd.none )


view : Model -> Html Msg
view model =
    div []
        --[ div [] [ Html.text (toString model) ]
        [ gameView model
        ]


gameView : Model -> Html Msg
gameView model =
    svg
        [ width "500", height "500", viewBox "0 0 100 100", SvgAttr.style "fill: black;" ]
        [ rect [ x "0", y "0", width "100", height "100" ] []
        , whiteRectangle model.leftPaddle
        , whiteRectangle model.rightPaddle
        , whiteRectangle model.ball
        ]


whiteRectangle : { a | x : Float, y : Float, width : Float, height : Float } -> Html Msg
whiteRectangle object =
    {- The reason why we can't just use the rect function is because we want the color to
       be different from the background. So we can't get around using the svg function to wrap
       the rect funciton so we can make the rectangle white.
    -}
    svg
        [ width "100", height "100", viewBox "0 0 100 100", SvgAttr.style "fill: white;" ]
        [ rect
            [ x (toString object.x)
            , y (toString object.y)
            , width (toString object.width)
            , height (toString object.height)
            ]
            []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown keyCode ->
            updateKeys True keyCode model

        KeyUp keyCode ->
            updateKeys False keyCode model

        Tick time ->
            let
                -- Destructuring so that we don't have to type model.ball, model.leftPaddle, etc.
                { leftPaddle, rightPaddle, ball, keysDown } =
                    model

                leftYChange =
                    if keysDown.w then
                        -1
                    else if keysDown.s then
                        1
                    else
                        0

                rightYChange =
                    if keysDown.up then
                        -1
                    else if keysDown.down then
                        1
                    else
                        0

                ballX =
                    clamp 0 (100 - ball.width) (ball.x + ball.vx)

                ballY =
                    clamp 0 (100 - ball.height) (ball.y + ball.vy)

                {- The ball collisions with the paddles only accounts for collisions in
                   the front of the paddle and it assumes that the collision is an exact collision.
                   An exact collision means the ball is not overlapping the paddle; it is only touching the very tip.
                   We might have to change this in the future because this only works if it's always
                   guaranteed that the ball will have an exact collision. If we change the rate at which
                   the ball moves per frame, it's possible that it would lead to it not being possible to
                   get an exact collision.
                   Since the ball moves by 1 (an integer that evenly divides all integers) right now, we don't
                   have to worry because we have the left and right paddle's x-coordinates set as integers
                   (which are divisible by 1, how much the ball moves per frame).
                -}
                ballVX =
                    if
                        -- left paddle collision
                        (ball.x == leftPaddle.x + leftPaddle.width)
                            && (leftPaddle.y - ball.height < ball.y)
                            && (ball.y < leftPaddle.y + leftPaddle.height)
                    then
                        abs ball.vx
                    else if
                        -- right paddle collision
                        (ball.x == rightPaddle.x - ball.width)
                            && (rightPaddle.y - ball.height < ball.y)
                            && (ball.y < rightPaddle.y + rightPaddle.height)
                    then
                        -1 * abs ball.vx
                    else
                        -- left and right wall collisions
                        (if ballX <= 0 || ballX >= 100 - ball.width then
                            -1
                         else
                            1
                        )
                            * ball.vx

                ballVY =
                    -- top and bottom wall collisions
                    (if ballY <= 0 || ballY >= 100 - ball.height then
                        -1
                     else
                        1
                    )
                        * ball.vy
            in
                ( { model
                    | leftPaddle = { leftPaddle | y = clamp 0 (100 - leftPaddle.height) (leftPaddle.y + leftYChange) }
                    , rightPaddle = { rightPaddle | y = clamp 0 (100 - rightPaddle.height) (rightPaddle.y + rightYChange) }
                    , ball = { ball | x = ballX, y = ballY, vx = ballVX, vy = ballVY }
                  }
                , Cmd.none
                )


updateKeys : Bool -> KeyCode -> Model -> ( Model, Cmd Msg )
updateKeys isDown keyCode model =
    let
        keysDown =
            model.keysDown

        newKeysDown =
            case keyCode of
                87 ->
                    { keysDown | w = isDown }

                83 ->
                    { keysDown | s = isDown }

                38 ->
                    { keysDown | up = isDown }

                40 ->
                    { keysDown | down = isDown }

                _ ->
                    keysDown
    in
        ( { model | keysDown = newKeysDown }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Time.every (15 * millisecond) Tick
        ]


main : Program Never Model Msg
main =
    program { init = init, view = view, update = update, subscriptions = subscriptions }

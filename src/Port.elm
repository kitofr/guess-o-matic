module Port exposing (playChar, correct)

port playChar : String -> Cmd msg
port playChar ch =
  Cmd ch

-- charBox : Signal.Mailbox Msg
-- charBox =
--   Signal.mailbox (PlayChar "")

--port correct : Bool -> Cmd msg
--port correct = Signal.map (\m -> Types.correct m.guess) app.model

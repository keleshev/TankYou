module Controls where

import Keyboard exposing (KeyCode)
import Set exposing (Set)


type alias Controls = { tracks: { left: Int, right: Int } }


dropMap : (a -> b) -> Signal a -> Signal b
dropMap f signal =
  Signal.dropRepeats (Signal.map f signal)



type alias KeyMapping = { left: { up: KeyCode, down: KeyCode },
                         right: { up: KeyCode, down: KeyCode } }


toControls : KeyMapping -> Set KeyCode -> Controls
toControls {left, right} keyCodeSet =
  let is keyCode = if Set.member keyCode keyCodeSet then 1 else 0 in
  { tracks={ left=(is left.up - is left.down),
             right=(is right.up - is right.down) } }

wesd = { left={ up=87, down=83 }, right={ up=69, down=68 } }
iokl = { left={ up=73, down=75 }, right={ up=79, down=76 } }

all : Signal Controls
all =
  dropMap (toControls wesd) Keyboard.keysDown

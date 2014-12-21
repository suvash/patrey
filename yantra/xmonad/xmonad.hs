import XMonad

myTerminal = "lilyterm"

main = do
   xmonad $ defaultConfig {
       terminal = myTerminal
   }

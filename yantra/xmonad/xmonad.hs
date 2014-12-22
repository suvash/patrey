import           XMonad
import           XMonad.Util.Run
import qualified Data.Map as M

myTerminal = "lilyterm"

-- | Keys begin -------------------

-- Define keys to add
keysToAdd _ =
    [
        -- Monitor brightness up key
           ((0, 0x1008ff02), spawn "xbacklight -inc 10")
        -- Monitor brightness down key
        ,  ((0, 0x1008ff03), spawn "xbacklight -dec 10")
    ]

-- Define keys to remove
keysToRemove _ = [ ]

-- Delete the keys combinations we want to remove.
strippedKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)

-- Compose the new key combinations.
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))

-- | Keys end ---------------------

main = do
   xmonad $ defaultConfig {
       terminal = myTerminal
     , keys = myKeys
   }

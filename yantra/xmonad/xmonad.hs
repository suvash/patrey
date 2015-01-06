import           XMonad
import           XMonad.Util.Run
import qualified Data.Map as M

myTerminal = "lilyterm"

-- | Keys begin -------------------

-- Define keys to add
keysToAdd x =
    [
        -- Monitor brightness up key
           ((0, 0x1008ff02), spawn "xbacklight -inc 10")
        -- Monitor brightness down key
        ,  ((0, 0x1008ff03), spawn "xbacklight -dec 10")
        -- Toggle Mute
        ,  ((0, 0x1008ff12), spawn "pamixer --toggle-mute")
        -- Increase Volume
        ,  ((0, 0x1008ff13), spawn "pamixer --increase 10")
        -- Decrease Volume
        ,  ((0, 0x1008ff11), spawn "pamixer --decrease 10")
        -- Screensaver and Lock
        ,  (((modMask x .|. controlMask), xK_l), spawn "xscreensaver-command -lock")
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
       modMask = mod4Mask
     , terminal = myTerminal
     , keys = myKeys
   }

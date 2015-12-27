import           XMonad
import           XMonad.Util.Run
import           XMonad.Util.Paste
import           XMonad.Actions.GridSelect
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import qualified Data.Map as M

myTerminal = "lilyterm"

-- | Keys begin -------------------

-- Define keys to add
keysToAdd x =
    [
        -- Monitor brightness up key
        -- ((0, 0x1008ff02), spawn "xbacklight -inc 10")
        -- Monitor brightness down key
     -- ,  ((0, 0x1008ff03), spawn "xbacklight -dec 10")
        -- Toggle Mute
     -- ,  ((0, 0x1008ff12), spawn "pamixer --toggle-mute")
        -- Increase Volume
     -- ,  ((0, 0x1008ff13), spawn "pamixer --increase 10")
        -- Decrease Volume
     -- ,  ((0, 0x1008ff11), spawn "pamixer --decrease 10")
        -- X-selection paste
           (((modMask x .|. controlMask), xK_v), pasteSelection)
        -- Screensaver and Lock
        ,  (((modMask x .|. controlMask), xK_l), spawn "xscreensaver-command -lock")
        -- Battery
        ,  (((modMask x .|. controlMask), xK_b), spawn "notify-send -t 4000 Battery \"$(acpi)\" ")
        -- Date and Time
        ,  (((modMask x .|. controlMask), xK_d), spawn "notify-send -t 4000 Date/Time \"$(date)\" ")
        -- Network
        ,  (((modMask x .|. controlMask), xK_n), spawn "notify-send -t 4000 Network \"$(ip -4 -o addr show | cut -d' ' -f2,7)\"")
        -- Display all the windows
        ,  (((modMask x .|. controlMask), xK_s), goToSelected defaultGSConfig)
        -- Toggle the Xmobar
        ,  (((modMask x .|. controlMask), xK_m), sendMessage ToggleStruts)
        -- Launch Firefox
        ,  (((modMask x .|. controlMask), xK_f), spawn "firefox")
        -- Launch Chrome Igcognito
        ,  (((modMask x .|. controlMask), xK_i), spawn "chromium-browser --incognito")
        -- Launch Tor Browser
        ,  (((modMask x .|. controlMask), xK_t), spawn "start-tor-browser")
        -- Launch Emacs
        ,  (((modMask x .|. controlMask), xK_e), spawn "emacs")
    ]

-- Define keys to remove
keysToRemove _ = [ ]

-- Delete the keys combinations we want to remove.
strippedKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)

-- Compose the new key combinations.
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))

-- | Keys end ---------------------

-- | Log Hook begin

myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8

-- | Log Hook end

-- | Border begin

myBorderWidth = 0
myNormalBorderColor = "#696969" -- dark-grey
myFocusedBorderColor = "#F0E686"  -- khaki

-- | Border end

-- | Focus follows mouse

myFocusFollowsMouse = False

-- | Focus follows mouse end

-- | Startup Hook

myStartupHook = spawn "~/.xmonad/on_xmonad_start.sh"

-- | Startup Hook end


-- | Layout Hook for XMobar things

myManageHook  = manageDocks <+> manageHook defaultConfig
myLayoutHook  = avoidStruts $ layoutHook defaultConfig

-- | Layout Hook End

main = do
   xmonad $ defaultConfig {
       modMask = mod4Mask
     , terminal = myTerminal
     , keys = myKeys
     , logHook = myLogHook
     , borderWidth = myBorderWidth
     , normalBorderColor = myNormalBorderColor
     , focusedBorderColor = myFocusedBorderColor
     , focusFollowsMouse = myFocusFollowsMouse
     , startupHook = myStartupHook
     , manageHook = myManageHook
     , layoutHook = myLayoutHook
   }

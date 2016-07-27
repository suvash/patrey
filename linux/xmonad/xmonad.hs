import           XMonad
import           XMonad.Util.Run(spawnPipe)
import           XMonad.Util.Paste
import           XMonad.Actions.GridSelect
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Tabbed
import           XMonad.Layout.Grid
import           XMonad.Layout.ThreeColumns
import           System.IO
import           XMonad.Util.Scratchpad(scratchpadSpawnActionTerminal, scratchpadManageHook)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal = "lilyterm"

-- | Keys begin -------------------

-- Define keys to add
newKeys x = M.fromList $
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
           ((modMask x .|. controlMask, xK_v), pasteSelection)
        -- Floating scratchpad
        ,  ((modMask x .|. controlMask, xK_space), scratchpadSpawnActionTerminal "rxvt-unicode")
        -- Focus on urgent window
        ,  ((modMask x .|. controlMask, xK_u), focusUrgent)
        -- Screensaver and Lock
        ,  ((modMask x .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
        -- Battery
        ,  ((modMask x .|. controlMask, xK_b), spawn "notify-send -t 4000 Battery \"$(acpi)\" ")
        -- Date and Time
        ,  ((modMask x .|. controlMask, xK_d), spawn "notify-send -t 4000 Date/Time \"$(date)\" ")
        -- Network
        ,  ((modMask x .|. controlMask, xK_n), spawn "notify-send -t 4000 Network \"$(ip -4 -o addr show | cut -d' ' -f2,7)\"")
        -- Display all the windows
        ,  ((modMask x .|. controlMask, xK_g), goToSelected defaultGSConfig)
        -- Toggle the Xmobar
        ,  ((modMask x .|. controlMask, xK_m), sendMessage ToggleStruts)
        -- Launch Spotify
        ,  ((modMask x .|. controlMask, xK_s), spawn "spotify --force-device-scale-factor=1.8")
        -- Launch Opera
        ,  ((modMask x .|. controlMask, xK_o), spawn "opera --private")
        -- Launch Firefox
        ,  ((modMask x .|. controlMask, xK_f), spawn "firefox")
        -- Launch Chrome Igcognito
        ,  ((modMask x .|. controlMask, xK_c), spawn "chromium-browser --incognito --force-device-scale-factor=1.8")
        -- Launch Tor Browser
        ,  ((modMask x .|. controlMask, xK_t), spawn "start-tor-browser")
        -- Launch Emacs
        ,  ((modMask x .|. controlMask, xK_e), spawn "emacs")
        -- Change wallpaper
        ,  ((modMask x .|. controlMask, xK_w), spawn "sh $HOME/.fehbg")
        -- Attach Detach workstation
        ,  ((modMask x .|. controlMask, xK_a), spawn "autoconfigure-workstation")
        -- Play Pause Spotify
        ,  ((modMask x .|. controlMask, xK_8), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        -- Next Spotify
        ,  ((modMask x .|. controlMask, xK_0), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
        -- Previous Spotify
        ,  ((modMask x .|. controlMask, xK_6), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    ]

-- Compose the new key combinations.
myKeys x = newKeys x `M.union` keys defaultConfig x

-- | Keys end ---------------------

-- | Log Hook begin

xmobarLogHook xmobarProcess =
  dynamicLogWithPP xmobarPP
  {  ppCurrent = xmobarColor xmobarCurrentWSColor "" -- . wrap "[" "]"
  ,  ppVisible = xmobarColor xmobarVisibleWSColor "" -- . wrap "(" ")"
  ,  ppHidden  = xmobarColor xmobarHiddenWSColor ""
  ,  ppUrgent  = xmobarColor xmobarUrgentWSColor "" . wrap ">" "<" . xmobarStrip
  --,  ppHiddenNoWindows  = xmobarColor xmobarHiddenNoWinWSColor ""
  ,  ppSep     = " : "
  ,  ppWsSep   = " "
  ,  ppTitle   = xmobarColor xmobarTitleColor "" . shorten cutOffTitleLength
  ,  ppLayout  = xmobarColor xmobarLayoutColor "" . layoutNameToIcon
  ,  ppOutput  = hPutStrLn xmobarProcess
  ,  ppOrder   = \(workspace:layout:_title:_extras) -> [workspace, layout]
  }
  where cutOffTitleLength = 15
        xmobarCurrentWSColor = "orange"
        xmobarVisibleWSColor = "darkgreen"
        xmobarHiddenWSColor  = "gray"
        --xmobarHiddenNoWinWSColor = "blue"
        xmobarUrgentWSColor  = "red"
        xmobarTitleColor  = "darkgreen"
        xmobarLayoutColor  = "darkgray"

        layoutNameToIcon ln =
          case ln of
            "Tall"                   -> "[|--]"
            "Grid"                   -> "[-|-]"
            "ThreeCol"               -> "[|||]"
            "Tabbed Bottom Simplest" -> "[___]"
            "Full"                   -> "[===]"
            _                        -> ln

fadeLogHook = fadeInactiveLogHook fadeAmount
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

-- | Layout begin

-- add Mirror to have the same layout in horizontal (90 deg +) direction
myLayout = tiled ||| threecolmid ||| Grid ||| simpleTabbedBottomAlways ||| Full
  where
    tiled       = Tall nmaster delta ratio
    -- threecol    = ThreeCol nmaster delta ratio
    threecolmid = ThreeColMid nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

-- | Layout end

-- | Layout Hook

manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.8       -- terminal height, 60%
    w = 0.8       -- terminal width,  80%
    t = (1-h)/2   -- distance from top edge, 90%
    l = (1-w)/2   -- distance from left edge, 0%

floatTileManageHook = composeAll. concat $
  [ [ className =? "vlc" --> doFloat ] ]

myManageHook  = manageDocks <+> floatTileManageHook <+> manageScratchPad <+> manageHook defaultConfig
myLayoutHook  = avoidStruts $ myLayout

-- | Layout Hook End

-- | Spawn processes

spawnXmobarProcess = spawnPipe "xmobar"

-- | Startup Hook end

myWorkspaces = ["१", "२", "३", "४", "५", "६", "७", "८", "९"]
-- myWorkspaces = ["1:one", "2:two" ] ++ map show [3..9]

main = do
  xmobarproc <- spawnXmobarProcess
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig {
       modMask = mod4Mask
     , terminal = myTerminal
     , keys = myKeys
     , logHook = xmobarLogHook xmobarproc >> fadeLogHook
     , borderWidth = myBorderWidth
     , normalBorderColor = myNormalBorderColor
     , focusedBorderColor = myFocusedBorderColor
     , focusFollowsMouse = myFocusFollowsMouse
     , startupHook = myStartupHook
     , manageHook = myManageHook
     , layoutHook = myLayoutHook
     , workspaces = myWorkspaces
   }

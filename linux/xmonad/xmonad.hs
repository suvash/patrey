import           XMonad
import qualified XMonad.Util.Run            as XMUR
import qualified XMonad.Util.Paste          as XMUP
import qualified XMonad.Actions.GridSelect  as XMAG
import qualified XMonad.Hooks.DynamicLog    as XMHD
import qualified XMonad.Hooks.FadeInactive  as XMHF
import qualified XMonad.Hooks.ManageDocks   as XMHM
import qualified XMonad.Hooks.EwmhDesktops  as XMHE
import qualified XMonad.Hooks.UrgencyHook   as XMHU
import qualified XMonad.Layout.Tabbed       as XMLTa
import qualified XMonad.Layout.Grid         as XMLG
import qualified XMonad.Layout.ThreeColumns as XMLTh
import qualified System.IO                  as SI
import qualified XMonad.Util.Scratchpad     as XUSC
import qualified XMonad.StackSet            as XMST
import qualified Data.Map                   as DM

myTerminal = "lilyterm"

-- | Keys begin -------------------

-- Define keys to add
newKeys x = DM.fromList $
    [
        -- Monitor brightness up key
           ((0, 0x1008ff02),
            spawn "xbacklight -inc 10")

        -- Monitor brightness down key
        ,  ((0, 0x1008ff03),
            spawn "xbacklight -dec 10")

        -- Increase Volume
        ,  ((modMask x .|. controlMask, xK_Up),
            spawn "pactl set-sink-volume 0 +5%")

        -- Decrease Volume
        ,  ((modMask x .|. controlMask, xK_Down),
            spawn "pactl set-sink-volume 0 -5%")

        -- X-selection paste
        ,  ((modMask x .|. controlMask, xK_v),
            XMUP.pasteSelection)

        -- Floating scratchpad
        ,  ((modMask x .|. controlMask, xK_space),
            XUSC.scratchpadSpawnActionTerminal "rxvt-unicode")

        -- Focus on urgent window
        ,  ((modMask x .|. controlMask, xK_u),
            XMHU.focusUrgent)

        -- Screensaver and Lock
        ,  ((modMask x .|. controlMask, xK_l),
            spawn "xscreensaver-command -lock")

        -- Battery
        ,  ((modMask x .|. controlMask, xK_b),
            spawn "notify-send -t 4000 Battery \"$(acpi)\" ")

        -- Date and Time
        ,  ((modMask x .|. controlMask, xK_d),
            spawn "notify-send -t 4000 Date/Time \"$(date)\" ")

        -- Network
        ,  ((modMask x .|. controlMask, xK_n),
            spawn "notify-send -t 4000 Network \"$(ip -4 -o addr show | cut -d' ' -f2,7)\"")

        -- Display all the windows
        ,  ((modMask x .|. controlMask, xK_g),
            XMAG.goToSelected XMAG.defaultGSConfig)

        -- Toggle the Xmobar
        ,  ((modMask x .|. controlMask, xK_m),
            sendMessage XMHM.ToggleStruts)

        -- Launch Spotify
        ,  ((modMask x .|. controlMask, xK_s),
            spawn "spotify --force-device-scale-factor=1.0")

        -- Launch Opera
        ,  ((modMask x .|. controlMask, xK_o),
            spawn "opera --private")

        -- Launch Firefox
        ,  ((modMask x .|. controlMask, xK_f),
            spawn "firefox")

        -- Launch Chrome Igcognito
        ,  ((modMask x .|. controlMask, xK_c),
            spawn "chromium --incognito --force-device-scale-factor=1.0")

        -- Launch Tor Browser
        ,  ((modMask x .|. controlMask, xK_t),
            spawn "torbrowser-launcher")

        -- Launch Emacs
        ,  ((modMask x .|. controlMask, xK_e),
            spawn "emacs")

        -- Change wallpaper
        ,  ((modMask x .|. controlMask, xK_w),
            spawn "sh $HOME/.fehbg")

        -- Attach Detach workstation
        ,  ((modMask x .|. controlMask, xK_a),
            spawn "autoconfigure-workstation")

        -- Play Pause Spotify
        ,  ((modMask x .|. controlMask, xK_8),
            spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")

        -- Next Spotify
        ,  ((modMask x .|. controlMask, xK_0),
            spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

        -- Previous Spotify
        ,  ((modMask x .|. controlMask, xK_6),
            spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    ]

-- Compose the new key combinations.
myKeys x = newKeys x `DM.union` keys defaultConfig x

-- | Keys end ---------------------

-- | Log Hook begin

xmobarLogHook xmobarProcess =
  XMHD.dynamicLogWithPP XMHD.xmobarPP
  {  XMHD.ppCurrent = XMHD.xmobarColor xmobarCurrentWSColor "" -- . wrap "[" "]"
  ,  XMHD.ppVisible = XMHD.xmobarColor xmobarVisibleWSColor "" -- . wrap "(" ")"
  ,  XMHD.ppHidden  = XMHD.xmobarColor xmobarHiddenWSColor "" . noScratchPad
  ,  XMHD.ppUrgent  = XMHD.xmobarColor xmobarUrgentWSColor "" . XMHD.wrap ">" "<" . XMHD.xmobarStrip
  -- ,  XMHD.ppHiddenNoWindows  = XMHD.xmobarColor xmobarHiddenNoWinWSColor ""
  ,  XMHD.ppSep     = " : "
  ,  XMHD.ppWsSep   = " "
  ,  XMHD.ppTitle   = XMHD.xmobarColor xmobarTitleColor "" . XMHD.shorten cutOffTitleLength
  ,  XMHD.ppLayout  = XMHD.xmobarColor xmobarLayoutColor "" . layoutNameToIcon
  ,  XMHD.ppOutput  = SI.hPutStrLn xmobarProcess
  ,  XMHD.ppOrder   = \(workspace:layout:_title:_extras) -> [workspace, layout]
  }
  where cutOffTitleLength = 15
        xmobarCurrentWSColor = "orange"
        xmobarVisibleWSColor = "darkgreen"
        xmobarHiddenWSColor  = "gray"
        -- xmobarHiddenNoWinWSColor = "blue"
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

        noScratchPad ws = if ws == "NSP" then "" else ws

fadeLogHook = XMHF.fadeInactiveLogHook fadeAmount
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
myLayout = tiled ||| threecolmid ||| XMLG.Grid ||| XMLTa.simpleTabbedBottomAlways ||| Full
  where
    tiled       = Tall nmaster delta ratio
    -- threecol    = ThreeCol nmaster delta ratio
    threecolmid = XMLTh.ThreeColMid nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

-- | Layout end

-- | Layout Hook

manageScratchPad = XUSC.scratchpadManageHook (XMST.RationalRect l t w h)
  where
    h = 0.8       -- terminal height, 60%
    w = 0.8       -- terminal width,  80%
    t = (1-h)/2   -- distance from top edge, 90%
    l = (1-w)/2   -- distance from left edge, 0%

floatTileManageHook = composeAll. concat $
  [ [ className =? "vlc" --> doFloat ] ]

myManageHook  = XMHM.manageDocks <+> floatTileManageHook <+> manageScratchPad <+> manageHook defaultConfig
myLayoutHook  = XMHM.avoidStruts $ myLayout

-- | Layout Hook End

-- | Spawn processes

spawnXmobarProcess = XMUR.spawnPipe "xmobar"

-- | Startup Hook end

myWorkspaces = ["१", "२", "३", "४", "५", "६", "७", "८", "९"]
-- myWorkspaces = ["1:one", "2:two" ] ++ map show [3..9]

main = do
  xmobarproc <- spawnXmobarProcess
  xmonad $ XMHU.withUrgencyHook XMHU.NoUrgencyHook $ XMHE.ewmh defaultConfig {
       modMask            = mod4Mask
     , keys               = myKeys
     , terminal           = myTerminal
     , workspaces         = myWorkspaces
     , logHook            = xmobarLogHook xmobarproc >> fadeLogHook
     , startupHook        = myStartupHook
     , manageHook         = myManageHook
     , layoutHook         = myLayoutHook
     , borderWidth        = myBorderWidth
     , normalBorderColor  = myNormalBorderColor
     , focusedBorderColor = myFocusedBorderColor
     , focusFollowsMouse  = myFocusFollowsMouse
   }

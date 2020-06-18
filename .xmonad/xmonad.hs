------------------------------------------------------------------------
--   ___  _   _ ____  ____      _    ____
--  / _ \| \ | |  _ \|  _ \    / \  | ___|
-- | | | |  \| | | | | |_) |  / _ \ |___ \
-- | |_| | |\  | |_| |  _ <  / ___ \ ___) |
--  \___/|_| \_|____/|_| \_\/_/   \_\____/
-- 
------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import Data.Maybe (isJust)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

    -- Utilities
import XMonad.Util.Loggers
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, wrap, pad, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat) 
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

    -- Actions
import XMonad.Actions.Minimize (minimizeWindow)
import XMonad.Actions.Promote
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)
import XMonad.Actions.MouseResize
import XMonad.Actions.GridSelect

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace) 
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing (spacing) 
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))
import qualified XMonad.Layout.Magnifier as Mag

    -- Keyboard stuff
import Graphics.X11.ExtraTypes.XorgDefault
import Graphics.X11.ExtraTypes.XF86
------------------------------------------------------------------------
---VARIABLES
------------------------------------------------------------------------
myFont          = "xft:Hack Nerd Font:size=10:antialias=true:autohint=true"
myGridFont      = "xft:Hack Nerd Font:size=15:antialias=true:autohint=true"
myModMask       = mod4Mask
myTerminal      = "st"
myTextEditor    = "nvim"
myBorderWidth   = 2
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
------------------------------------------------------------------------
--GRID SELECT
------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x29,0x2d,0x3e) -- lowest inactive bg
                  (0x29,0x2d,0x3e) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x29,0x2d,0x3e) -- active fg
                  
-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 60
    , gs_cellwidth    = 250
    , gs_cellpadding  = 8
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myGridFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def

-- Set favorite apps for the spawnSelected'
myAppGrid :: [(String, String)]
myAppGrid = [ ("Firefox", "firefox")
            , ("qutebrowser", "qutebrowser")
            , ("ThunderBird", "thunderbird")
            , ("Spotify", "st -e sptui")
            , ("NewsBoat", "st -e newsboat")
            , ("ViFm", "st -e vifm")
            , ("Simple Terminal", "st")
            , ("Neomutt", "st -e neomutt")
            , ("Calendar", "st -e calcurse")
            , ("Gimp", "gimp")
            , ("PcManFM", "pcmanfm")
            , ("Inkscape", "inkscape")
            , ("LibreOffice Writer", "lowriter")
            ]
------------------------------------------------------------------------
--MAIN
------------------------------------------------------------------------
main = do
    -- Launching two instances of xmobar on their monitors.
    xmproc0 <- spawnPipe "xmobar -x 0 /home/ondra/.config/xmobar/xmobarrc"
       -- the xmonad
    xmonad $ewmh desktopConfig
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x 
                        , ppCurrent = xmobarColor "#98971a" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppHidden = xmobarColor "#8ec07c" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#ebdbb2" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#ebdbb2" "" . shorten 30     -- Title of active window in xmobar
                        , ppSep =  "<fc=#ebdbb2> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#cc241d" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook 
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = "#444444"
        , focusedBorderColor = "#98971a"
        } 
------------------------------------------------------------------------
--AUTOSTART
------------------------------------------------------------------------
myStartupHook = do
         spawnOnce "nitrogen --restore &"
         spawnOnce "picom &"
         spawnOnce "start-pulseaudio-x11 &"
         spawnOnce "lxqt-policykit-agent &"
         spawnOnce "dunst &"
         spawnOnce "playerctl &"
         spawnOnce "numlockx on &"
         spawnOnce "NetworkManager &"
         spawnOnce "xsetroot -cursor_name left_ptr &" --set normal mouse cursor
------------------------------------------------------------------------
--KEYBINDINGS
------------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return ), spawn $ XMonad.terminal conf)
    -- dmenu stuff
    , ((modm,               xK_e     ), spawn "emoji")
    , ((modm .|. shiftMask, xK_e     ), spawn "dmenuexit.sh")
    , ((modm .|. shiftMask, xK_m     ), spawn "dmenumount")
    , ((modm .|. shiftMask, xK_u     ), spawn "dmenuumount")
    , ((modm,               xK_d     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    -- launch XMonad prompt
    --, ((modm,               xK_d     ), shellPrompt oXPConfig)
    -- launching apps
    , ((modm,               xK_a     ), spawnSelected' myAppGrid)
    -- window manipulation
    , ((modm .|. shiftMask, xK_q     ), kill1) -- close a window
    , ((modm,               xK_space ), sendMessage NextLayout) -- rotate layouts
    , ((modm .|. shiftMask, xK_space ), sendMessage FirstLayout) -- default layout
    , ((modm,               xK_n     ), refresh) -- resize viewed windows to the correct size
    , ((modm,               xK_Tab   ), windows W.focusDown) -- move focus
    , ((modm,               xK_j     ), windows W.focusDown) -- move focus
    , ((modm,               xK_k     ), windows W.focusUp  ) -- move focus
    , ((modm,               xK_m     ), windows W.swapMaster) -- make the current window master
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  ) --move a window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    ) -- move a window
    , ((modm,               xK_h     ), sendMessage Shrink) -- shrint the master area
    , ((modm,               xK_l     ), sendMessage Expand) -- expand the master area
    , ((modm,               xK_t     ), withFocused $ windows . W.sink) -- push window back into tiling
    , ((modm .|. shiftMask, xK_i     ), sendMessage (IncMasterN 1)) -- increment the number of windows in master
    , ((modm .|. shiftMask, xK_d     ), sendMessage (IncMasterN (-1))) -- decrease the number of windows in master
    , ((modm .|. shiftMask, xK_o     ), sendMessage Mag.Toggle) -- magnify a window --this doesn't do jackshit, I need to fix that
    , ((modm,               xK_b     ), sendMessage ToggleStruts) -- toggle bar
    -- Multimedia keys
    , ((0 , xF86XK_AudioLowerVolume  ), spawn "amixer -q -D pulse sset Master 5%-")
    , ((0 , xF86XK_AudioRaiseVolume  ), spawn "amixer -q -D pulse sset Master 5%+")
    , ((0 , xF86XK_AudioMute         ), spawn "amixer -q -D pulse sset Master toggle")
    , ((0 , xF86XK_TouchpadToggle    ), spawn "(synclient | grep 'Touchpad0ff.*1' && synclient Touchpad0ff=0) || synclient Touchpad0ff=1")
    , ((0 , xF86XK_MonBrightnessDown ), spawn "xbacklight -inc 10")
    , ((0 , xF86XK_MonBrightnessUp   ), spawn "xbacklight -dec 10")
    , ((modm, xF86XK_AudioMute       ), spawn "playerctl previous")
    , ((modm, xF86XK_AudioLowerVolume), spawn "playerctl play-pause")
    , ((modm, xF86XK_AudioRaiseVolume), spawn "playerctl next")
    , ((0 , xK_Print                 ), spawn "maim --hidecursor $HOME/Pictures/screenshots/$(date +%d_%b_%H:%M ).png")
    , ((0 .|. shiftMask, xK_Print    ), spawn "maim -s --hidecursor $HOME/Pictures/screenshots/$(date +%d_%b_%H:%M ).png")
    -- Quit xmonad
    -- , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm .|. shiftMask , xK_r     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_plus, xK_ecaron, xK_scaron, xK_ccaron, xK_rcaron, xK_zcaron, xK_yacute, xK_aacute, xK_iacute]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    
       -- ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --

    --[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
      --  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        --, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
---WORKSPACES
--should be clickable, but guess what, it doesn't work 😂
------------------------------------------------------------------------

xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]
        
myWorkspaces :: [String]   
myWorkspaces = clickable . (map xmobarEscape) 
               $ ["www", "file", "dev", "chat", "exp", "vid", "doc", "uni", "spt"]
  where                                                                      
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,                                        
                      let n = i ] 

------------------------------------------------------------------------
--MANAGEHOOK --This is random stuff that's basically a template 
             --but it doesn't work 🤷
             --placing this here to remind me to fix xdotool
------------------------------------------------------------------------
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [  className =? "firefox"     --> doShift "<action=xdotool key super+1>www</action>"
      , className =? "vlc"         --> doShift "<action=xdotool key super+8>vid</action>"
      , title =? "Oracle VM VirtualBox Manager"  --> doFloat
      , className =? "Oracle VM VirtualBox Manager"  --> doShift "<action=xdotool key super+5>vbox</action>"
      , className =? "Gimp"        --> doFloat
      , className =? "Gimp"        --> doShift "<action=xdotool key super+9>gimp</action>"
      , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ]
------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $ 
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
             where 
                 myDefaultLayout = tall
                               ||| magnify -- this is here just as a workaround, because the keybinding for it doesn't work...
                               ||| threeRow 
                               ||| noBorders monocle


tall       = renamed [Replace "tall"]
             $ limitWindows 12
             $ spacing 0
             $ ResizableTall 1 (3/100) (1/2) []
threeRow   = renamed [Replace "threeRow"]
             $ limitWindows 3
             $ Mirror
             $ mkToggle (single MIRROR) zoomRow
monocle    = renamed [Replace "monocle"]
             $ limitWindows 20 $ Full
floats     = renamed [Replace "floats"]
             $ limitWindows 20
             $ simplestFloat
magnify    = renamed [Replace "magnify"]
             $ Mag.magnifier
             $ limitWindows 12
             $ spacing 0
             $ ResizableTall 1 (3/100) (1/2) []

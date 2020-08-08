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
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

    -- Utilities
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat) 
import XMonad.Hooks.EwmhDesktops

    -- Actions
import qualified XMonad.Actions.Search as S
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.MouseResize
import XMonad.Actions.Submap

    -- Layouts modifiers
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns(ThreeCol(..))
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow)
import qualified XMonad.Layout.Magnifier as Mag

    -- Keyboard stuff
import Graphics.X11.ExtraTypes.XorgDefault
import Graphics.X11.ExtraTypes.XF86

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Pass (passPrompt)

------------------------------------------------------------------------
---VARIABLES
------------------------------------------------------------------------
myFont :: [Char]
myFont          = "xft:Mononoki Nerd Font:size=11:antialias=true:autohint=true"
myModMask       = mod4Mask
myTerminal :: [Char]
myTerminal      = "alacritty"
myTextEditor :: [Char]
myTextEditor    = "nvim"
myBorderWidth :: Dimension
myBorderWidth   = 2
windowCount :: X (Maybe String)
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
--MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
    -- Launching two instances of xmobar on their monitors.
    xmproc0 <- spawnPipe "xmobar -x 0 /home/ondra/.config/xmobar/xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 1 /home/ondra/.config/xmobar/xmobarrc"
       -- the xmonad
    xmonad $ewmh desktopConfig
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x
                        , ppCurrent = xmobarColor "#fb4934" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppHidden = xmobarColor "#FE8019" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#8ec07c" ""        -- Hidden workspaces (no windows)
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
myStartupHook :: X ()
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
         spawnOnce "pulse_volume.py | xob"

------------------------------------------------------------------------
--PROMPT
------------------------------------------------------------------------
oXPConfig :: XPConfig
oXPConfig = def
   { font                  = myFont
   , bgColor               = "#282828"
   , fgColor               = "#ebdbb2"
   , bgHLight              = "#98971a"
   , fgHLight              = "#282828"
   , borderColor           = "#444444"
   , promptBorderWidth     = 0
   --, promptKeymap          = 
   , position              = Top
   , height                = 20
   , historySize           = 256
   , historyFilter         = id
   , defaultText           = []
   , autoComplete          = Just 100000 --100000 for 0.1 seconds -- "Nothing" to turn it off
   , showCompletionOnTab   = False
   , searchPredicate       = fuzzyMatch
   , alwaysHighlight       = True
   , maxComplRows          = Just 5 --you can set number to specify, "Just 1" for one row -- "Nothing" for unlimited
   }

------------------------------------------------------------------------
--SEARCH PROMPT
------------------------------------------------------------------------
sXPConfig :: XPConfig
sXPConfig = def
   { font                  = myFont
   , bgColor               = "#282828"
   , fgColor               = "#ebdbb2"
   , bgHLight              = "#98971a"
   , fgHLight              = "#282828"
   , borderColor           = "#444444"
   , promptBorderWidth     = 0
   --, promptKeymap          = 
   , position              = Top
   , height                = 20
   , historySize           = 0
   , historyFilter         = id
   , defaultText           = []
   , autoComplete          = Nothing --100000 for 0.1 seconds -- "Nothing" to turn it off
   , showCompletionOnTab   = False
   , searchPredicate       = fuzzyMatch
   , alwaysHighlight       = True
   , maxComplRows          = Just 1 --you can set number to specify, "Just 1" for one row -- "Nothing" for unlimited
   }

------------------------------------------------------------------------
--KEYBINDINGS
------------------------------------------------------------------------
myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return ), spawn $ XMonad.terminal conf)
    -- make fullscreen
    , ((modm,               xK_f     ), sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)
    -- dmenu stuff
    , ((modm,               xK_e     ), spawn "emoji")
    , ((modm .|. shiftMask, xK_e     ), spawn "dmenuexit.sh")
    , ((modm .|. shiftMask, xK_m     ), spawn "dmenumount")
    , ((modm .|. shiftMask, xK_u     ), spawn "dmenuumount")
    , ((modm,            xK_BackSpace), spawn "dmenufm")
    --, ((modm,               xK_d     ), spawn "dmenu_run -i")
    -- XMonad prompts
    , ((modm,               xK_d     ), shellPrompt oXPConfig)
    , ((modm .|. shiftMask, xK_p     ), passPrompt sXPConfig)
    , ((modm,               xK_s     ), submap . M.fromList $
         [ ((0, xK_g),      S.promptSearch sXPConfig S.google)
         , ((0, xK_d),      S.promptSearch sXPConfig S.duckduckgo)
         , ((0, xK_y),      S.promptSearch sXPConfig S.youtube)
         , ((0, xK_h),      S.promptSearch sXPConfig S.hoogle)
         , ((0, xK_a),      S.promptSearch sXPConfig archwiki)
         , ((0, xK_r),      S.promptSearch sXPConfig reddit)
         , ((0, xK_u),      S.promptSearch sXPConfig urban)
         , ((0, xK_w),      S.promptSearch sXPConfig wikiskripta)
         ])
    -- launching apps
    -- Submaps
    , ((modm,               xK_a     ), submap . M.fromList $
         [ ((0, xK_q),     spawn "qutebrowser")
         , ((0, xK_f),     spawn "firefox")
         , ((0, xK_n),     spawn (myTerminal ++ " -e newsboat"))
         , ((0, xK_c),     spawn (myTerminal ++ " -e calcurse"))
         , ((0, xK_b),     spawn "dmenu_websearch")
         , ((0, xK_w),     spawn "weatherradar")
         , ((0, xK_m),     spawn (myTerminal ++ " -e neomutt"))
         , ((0, xK_t),     spawn "telegram-desktop")
         , ((0, xK_v),     spawn (myTerminal ++ " -e vifm"))
         , ((0, xK_p),     spawn (myTerminal ++ " -e pulsemixer"))
         ])
    , ((modm,               xK_p     ), submap . M.fromList $
         [ ((0, xK_n),     spawn "playerctl next")
         , ((0, xK_p),     spawn "playerctl previous")
         , ((0, xK_space), spawn "playerctl play-pause")
         ])
    -- workspace switching back and forth
    , ((modm,               xK_Tab   ), toggleWS)
    -- magnify a window (another failed attempt)
    --, ((modm,               xK_o     ), sendMessage (T.Toggle "magnify"))
    -- window manipulation
    , ((modm .|. shiftMask, xK_q     ), kill1) -- close a window
    , ((modm,               xK_space ), sendMessage NextLayout) -- rotate layouts
    , ((modm .|. shiftMask, xK_space ), sendMessage FirstLayout) -- default layout
    , ((modm,               xK_n     ), refresh) -- resize viewed windows to the correct size
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
    
        ++
    --
    -- mod-{backslash,y,x}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{backslash,y,x}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_y, xK_x, xK_c] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
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
--SEARCH ENGINES
------------------------------------------------------------------------
-- Define search engines not included in the module
archwiki, reddit, urban, wikiskripta :: S.SearchEngine

archwiki    = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
reddit      = S.searchEngine "reddit" "https://www.reddit.com/search?q="
urban       = S.searchEngine "urban" "https://www.urbandictionary.com/define.php?term="
wikiskripta = S.searchEngine "wikiskripta" "https://www.wikiskripta.eu/index.php?search="

------------------------------------------------------------------------
--WORKSPACES
--the part that should make it clickable doesn't really work because xdotool doesn't play well with my special characters or I'm dumb
--THIS WILL SOON BR OBSOLOTE AS THERE'S A MODULE FOR IT IN THE WORKS (WILL BE A PP)
------------------------------------------------------------------------

xmobarEscape :: String -> String
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
--MANAGEHOOK
------------------------------------------------------------------------
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll . concat $
      [ [isDialog --> doCenterFloat]
      , [className =? "firefox"     --> doShift "<action=xdotool key super+1>www</action>"]
      , [className =? "qutebrowser" --> doShift "<action=xdotool key super+1>www</action>"]
      , [className =? "Pcmanfm"     --> doShift "<action=xdotool key super+2>file</action>"]
      , [className =? "Telegram"    --> doShift "<action=xdotool key super+4>chat</action>"]
      , [className =? "mpv"         --> doShift "<action=xdotool key super+6>vid</action>"]
      , [className =? "Galculator"  --> doCenterFloat]
      , [(className =? "firefox" <&&> resource =? "Dialog") --> doCenterFloat]  -- Float Firefox Dialog
     ]
------------------------------------------------------------------------
--GLOBAL LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $ 
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
             where 
                 myDefaultLayout = smartBorders tall
                               ||| smartBorders magnify -- this is here just as a workaround, because the keybinding for it doesn't work...
                               ||| threeRow 
                               -- ||| noBorders monocle --not needed anymore I guess as I have a proper keybinding for fullscreen
                               ||| threeColMid

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
threeColMid = renamed [Replace "threeColMid"]
             $ limitWindows 6
             $ spacing 4
             $ ThreeColMid 1 (3/100) (1/2)

import qualified Codec.Binary.UTF8.String as UTF8
---import XMonad.Layout.NoBorders

import Control.Monad (liftM2)
import qualified DBus as D
import qualified DBus.Client as D
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Char (isSpace)
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import Data.List.Split
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.WindowBringer
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Hooks.DynamicProperty
import XMonad.Actions.WindowGo
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.Cross (simpleCross)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Gaps
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)

myTerminal :: String
myTerminal = "alacritty"

myStartupHook :: X ()
myStartupHook = do
  --spawnOnce "redshiftgui &"
  spawnOnce "dropbox start &"
  setWMName "LG3D"
  --spawnOn "emacs" "emacs"
  --runOnce "xrandr --auto --output DisplayPort-2 --mode 2560x1440 --right-of DisplayPort-1"
  -- spawnOnce "emacs --daemon &"

-- colours
normBord = "#4c566a"

focdBord = "#f70d1a"

fore = "#DEE3E0"

back = "#282c34"

winType = "#c678dd"

--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

myModMask = mod4Mask

encodeCChar = map fromIntegral . B.unpack

myFocusFollowsMouse = True

myBorderWidth = 2

--myWorkspaces    = ["\61612","\61899","\61947","\61635","\61502","\61501","\61705","\61564","\62150","\61872"]
myWorkspaces = ["emacs", "dev", "dev2", "mail", "web", "rocket", "tests", "eshell", "vm", "10"]

--myWorkspaces    = ["I","II","III","IV","V","VI","VII","VIII","IX","X"]

myBaseConfig = desktopConfig

-- window manipulations
myManageHook =
  composeAll . concat $
    [ [isDialog --> doCenterFloat],
      [className =? c --> doCenterFloat | c <- myCFloats],
      [title =? t --> doFloat | t <- myTFloats],
      [resource =? r --> doFloat | r <- myRFloats],
      [resource =? i --> doIgnore | i <- myIgnores]
       , [(fmap (myTrim . head . splitOn "-") title) =? t --> doShift (myWorkspaces !! 6) | t <- ["Run", "Debug"]]
      -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61899" | x <- my2Shifts]
      -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61947" | x <- my3Shifts]
      -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61635" | x <- my4Shifts]
      -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61502" | x <- my5Shifts]
      -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61501" | x <- my6Shifts]
      -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61705" | x <- my7Shifts]
      -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61564" | x <- my8Shifts]
      -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\62150" | x <- my9Shifts]
      -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61872" | x <- my10Shifts]
    ]
  where
    -- doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Arandr", "Arcolinux-tweak-tool.py", "Arcolinux-welcome-app.py", "Galculator", "feh", "mpv", "Xfce4-terminal"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]

-- my1Shifts = ["Chromium", "Vivaldi-stable", "Firefox"]
-- my2Shifts = []
-- my3Shifts = ["Inkscape"]
-- my4Shifts = []
-- my5Shifts = ["Gimp", "feh"]
-- my6Shifts = ["vlc", "mpv"]
-- my7Shifts = ["Virtualbox"]
-- my8Shifts = ["Thunar"]
-- my9Shifts = []
-- my10Shifts = ["discord"]

--myLayout = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Mirror tiled ||| spiral (6/7)  ||| ThreeColMid 1 (3/100) (1/2) ||| Full
myLayout = tiled ||| Full ||| threeRow
  where
    tiled = Tall nmaster delta tiled_ratio
    nmaster = 1
    delta = 3 / 100
    tiled_ratio = 1 / 2
    threeRow = renamed [Replace "threeRow"]
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)



myMouseBindings (XConfig {XMonad.modMask = modMask}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)),
      -- mod-button2, Raise the window to the top of the stack
      ((modMask, 2), (\w -> focus w >> windows W.shiftMaster)),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ((modMask, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
      -- launch dmenu
      ((modm, xK_p), spawn "rofi -show run"),
      -- launch gmrun
      ((modm .|. shiftMask, xK_p), spawn "gmrun"),
      -- close focused window
      ((modm .|. shiftMask, xK_c), kill),
      -- Rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
      ((modm .|. shiftMask , xK_s), rotAllDown ),
      ((modm .|. shiftMask, xK_u), spawn "emacs"),
      ((modm .|. shiftMask, xK_l), spawn "xsecurelock"),
      ((modm .|. shiftMask, xK_i), spawn "/home/piotr/scripts/idea"),
      ((modm .|. shiftMask, xK_y), spawn "/home/piotr/scripts/webstorm"),
      ((modm .|. shiftMask, xK_p), spawn "/home/piotr/scripts/datagrip"),
      ((modm .|. shiftMask, xK_o), spawn "brave"),
      -- SCRATCHPADS
      ((modm .|. controlMask, xK_p), namedScratchpadAction myScratchPads "pomodoro"),
      ((modm .|. controlMask, xK_o), namedScratchpadAction myScratchPads "ranger"),
      ((modm .|. controlMask, xK_u), namedScratchpadAction myScratchPads "terminal"),
      --  Reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ((modm, xK_n), refresh),
      -- Move focus to the next window
      ((modm, xK_Tab), windows W.focusDown),
      -- Move focus to the next window
      ((modm, xK_j), windows W.focusDown),
      -- Move focus to the previous window
      ((modm, xK_k), windows W.focusUp),
      ((modm , xK_u     ), gotoMenu),
      ((modm , xK_i     ), bringMenu),
      -- Move focus to the master window
      ((modm, xK_m), windows W.focusMaster),
      -- Swap the focused window and the master window
      ((modm, xK_Return), windows W.swapMaster),
      -- Swap the focused window with the next window
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      -- Shrink the master area
      ((modm, xK_h), sendMessage Shrink),
      -- Expand the master area
      ((modm, xK_l), sendMessage Expand),
      -- Push window back into tiling
      ((modm, xK_t), withFocused $ windows . W.sink),
      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      --
      -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

      -- Quit xmonad
      ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess)),
      -- Restart xmonad
      ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
      -- Run xmessage with a summary of the default keybindings (useful for beginners)
    ]
      ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

-- keys config
--
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "pomodoro" spawnPom findPom managePom,
    NS "ranger" spawnTerm findTerm manageTerm,
    NS "terminal" spawnT findT manageT
  ]
  where
    spawnPom = "st -n pomodoro 'gnome-pomodoro &'"
    findPom = resource =? "gnome-pomodoro"
    managePom = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.8
        t = 0.95 - h
        l = 0.95 - w

    spawnTerm = "alacritty -t ranger -e ranger"
    findTerm = title =? "ranger"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.8
        t = 0.95 - h
        l = 0.95 - w
    spawnT = myTerminal ++ " -t scratchpad"
    findT = title =? "scratchpad"
    manageT = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

myTrim :: String -> String
myTrim = f . f
    where f = reverse . dropWhile isSpace


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


main :: IO ()
main = do
  --update it
  xmproc0 <- spawnPipe "xmobar -x 0 /home/piotr/.xmonad/xmobar.config"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/piotr/.xmonad/xmobar.config"
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad . ewmh $
    --Keyboard layouts
    --qwerty users use this line
    myBaseConfig
      { --French Azerty users use this line
        --myBaseConfig { keys = azertyKeys <+> keys azertyConfig }
        --Belgian Azerty users use this line
        --myBaseConfig { keys = belgianKeys <+> keys belgianConfig }

        startupHook = myStartupHook,
        layoutHook = gaps [(U, 20)] myLayout,
        manageHook = manageSpawn <+> myManageHook <+> manageHook myBaseConfig,
        modMask = myModMask,
        borderWidth = myBorderWidth,
        handleEventHook = handleEventHook myBaseConfig <+> fullscreenEventHook,
        --handleEventHook = handleEventHook myBaseConfig <+> fullscreenEventHook,
        focusFollowsMouse = myFocusFollowsMouse,
        logHook =
          dynamicLogWithPP
            xmobarPP
              { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x,
                ppTitle = xmobarColor "#b3afc2" "" . shorten 50,
                ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]",
                ppVisible = xmobarColor "#98be65" "",
                ppHidden = xmobarColor "#98be65" "" . wrap "*" "",
                ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!",
                ppSep = " | ",
                ppExtras = [windowCount],
                ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
              },
        workspaces = myWorkspaces,
        focusedBorderColor = focdBord,
        normalBorderColor = normBord,
        keys = myKeys,
        mouseBindings = myMouseBindings,
        terminal = myTerminal
      }

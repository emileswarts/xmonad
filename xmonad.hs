import XMonad
import Data.Monoid
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.Paste
import XMonad.Util.EZConfig
import XMonad.Util.Font
import XMonad.Hooks.FadeInactive
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "xterm"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myBorderWidth   = 1
myModMask       = mod4Mask

-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = [
	"1:mail",
	"2:www",
	"3:local",
	"4:brix",
	"5:homer",
	"6:virtualbox",
	"7:chat",
	"8:otherservers",
	"9:other"]

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
     ((modm,               xK_space ), sendMessage NextLayout)
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
	-- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown) 
    -- Push window back into tiling
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm,               xK_a     ), spawn "gnome-terminal -e 'alsamixer'")
    , ((modm .|. shiftMask, xK_b     ), withFocused $ windows . W.sink)
    , ((modm .|. shiftMask, xK_c), 		kill)
    , ((modm,               xK_d     ), spawn "dmenu_run")
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
	, ((modm, xK_m ), windows copyToAll) -- @@ Make focused window always visible
	, ((modm .|. shiftMask, xK_m ),  killAllOtherCopies) -- @@ Toggle window state back
    -- Move focus to the master window
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_b     ), spawn "gnome-terminal --profile=blue")
    , ((modm,               xK_g     ), spawn "gnome-terminal --profile=green")
    , ((modm,               xK_o     ), spawn "gnome-terminal --profile=orange")
    , ((modm,               xK_p     ), spawn "gnome-terminal --profile=purple")
    , ((modm,               xK_v     ), spawn "gnome-terminal --profile=red")
	-- launch xfce panel for settings
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm,               xK_s     ), spawn "scrot '%Y-%m-%d_$w$h.png' -e 'mv $f ~/shots/'")
	, ((modm,               xK_t), 		spawn "gnome-terminal")
	, ((modm,               xK_w), 		spawn "~/bin/wallpaper")
	--, ((modm .|. shiftMask, xK_t), 		spawn "terminator -e ls")
    -- Deincrement the number of windows in the master area
    -- Quit xmonad
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_n, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "TogglDesktop"   --> doFloat
    , className  =? "Xfdesktop" 	--> doIgnore 
	]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--myLogHook :: X ()
--myLogHook = fadeInactiveLogHook fadeAmount
	where fadeAmount = 0.7

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar -x 1 ~/.xmobarrc"
  xmonad defaultConfig {
    terminal 				= myTerminal,
	focusFollowsMouse  		= myFocusFollowsMouse,
	borderWidth        		= myBorderWidth,
	modMask            		= myModMask,
	workspaces         		= myWorkspaces,
      -- key bindings
	keys               		= myKeys,
	mouseBindings      		= myMouseBindings,
	handleEventHook    		= myEventHook,
	startupHook        		= myStartupHook,
-- if you are using xmonad 0.9, you can avoid web flash videos getting cropped in fullscreen like so:
-- manageHook = ( isFullscreen --> doFullFloat ) <+> manageDocks <+> manageHook defaultConfig,
-- (no longer needed in 0.10)
    manageHook = myManageHook, 
    layoutHook = avoidStruts $ layoutHook defaultConfig,
    logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc,
                          ppTitle = xmobarColor "green" "" . shorten 50
                        }
  }

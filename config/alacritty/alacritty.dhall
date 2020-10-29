-- https://github.com/alacritty/alacritty/blob/master/alacritty.yml

let OS = ./common/OS.dhall

let os = env:DHALL_OS

let font = "Fira Mono"

let kb = { action = None Text, chars = None Text }

in
{ window =
  { decorations = "none"
  , dynamic_padding = True
  , dynamic_title = True
  , padding = { x = 2, y = 2 }
  , startup_mode = "Maximized"
  }
, colors =
  { bright =
    { black = "0x000000"
    , blue = "0x4170AE"
    , cyan = "0x3D999F"
    , green = "0x708B00"
    , magenta = "0x8958A7"
    , red = "0xC82828"
    , white = "0xFFFEFE"
    , yellow = "0xE9B600"
    }
  , cursor = { cursor = "0x4c4c4c", text = "0xffffff" }
  , normal =
    { black = "0x000000"
    , blue = "0x4171AE"
    , cyan = "0x3E999F"
    , green = "0x718C00"
    , magenta = "0x8959A8"
    , red = "0xC82828"
    , white = "0xFFFEFE"
    , yellow = "0xEAB700"
    }
  , primary = { background = "0xffffff", foreground = "0x4d4d4c" }
  }
, font =
  { bold.family = font
  , italic.family = font
  , normal.family = font
  , size = merge { Linux = 10, Macos = 12 } os
  }
, key_bindings =
  [ kb // { action = Some "Paste", key = "V", mods = "Control|Shift" }
  , kb // { action = Some "Copy", key = "C", mods = "Control|Shift" }
  -- Translate Ctrl-Shift-T to Ctrl-Shift-F12, otherwise tmux can't tell it
  -- from Ctrl-T
  -- to obtain the scancode, use 'showkeys -a' and press the desired key combo
  , kb // { chars = Some "\u001b[24;6~", key = "T", mods = "Control|Shift" }
  , kb // { action = Some "ReceiveChar", key = "Insert", mods = "Shift" }
  , kb // { action = Some "ResetFontSize", key = "Key0", mods = "Control" }
  , kb // { action = Some "IncreaseFontSize", key = "Equals", mods = "Control" }
  , kb // { action = Some "IncreaseFontSize", key = "Add", mods = "Control" }
  , kb // { action = Some "DecreaseFontSize", key = "Subtract", mods = "Control" }
  , kb // { action = Some "DecreaseFontSize", key = "Minus", mods = "Control" }
  , kb // { action = Some "ReceiveChar", key = "Key0", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "Equals", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "Add", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "Minus", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "K", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "K", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "V", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "C", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "H", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "M", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "Q", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "W", mods = "Command" }
  , kb // { action = Some "ReceiveChar", key = "F", mods = "Command|Control" }
  ]
, mouse.url.modifiers = "Control"
, shell =
  { args = [ "-c", "tmux new -A -s alacritty" ]
  , program = merge { Linux = "/usr/bin/zsh", Macos = "/usr/local/bin/zsh" } os
  }
}

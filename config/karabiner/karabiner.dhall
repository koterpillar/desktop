let optionFold = https://prelude.dhall-lang.org/Optional/fold.dhall
let optionDefault = https://prelude.dhall-lang.org/Optional/default.dhall

let showOptional = \(t: Type) -> \(show: t -> Text) -> \(value: Optional t) -> optionFold t value Text show ""

let showOptionalText = \(value: Optional Text) -> optionDefault Text "(-)" value

let Modifiers = List Text

let concatSep = https://prelude.dhall-lang.org/Text/concatSep.dhall

let showModifiers = \(m: Modifiers) -> concatSep " " m

let showOptionalModifiers = showOptional Modifiers showModifiers

let FromModifiers = {
    mandatory: Optional Modifiers,
    optional: Optional Modifiers
}

let showFromModifiers = \(fm: FromModifiers) -> showOptionalModifiers fm.mandatory

let nullFM: FromModifiers = {
    mandatory = None Modifiers,
    optional = None Modifiers
}

let fmAnyOptional = nullFM // { optional = Some ["any"] }

let fmStrict = \(mandatory: Modifiers) -> nullFM // { mandatory = Some mandatory }

let fmLax = \(mandatory: Modifiers) -> fmAnyOptional // { mandatory = Some mandatory }

let From = {
    key_code: Optional Text,
    pointing_button: Optional Text,
    modifiers: Optional FromModifiers
}

let nullFrom = { key_code = None Text, pointing_button = None Text, modifiers = None FromModifiers }: From

let showFrom = \(f: From) -> showOptional FromModifiers showFromModifiers f.modifiers ++ " " ++ showOptionalText f.key_code ++ " " ++ showOptionalText f.pointing_button

let fromKeyCode = \(key_code: Text) -> nullFrom // { key_code = Some key_code }

let fromModifiers = \(modifiers: Modifiers) -> \(key_code: Text) -> fromKeyCode key_code // {
    modifiers = Some (fmLax modifiers)
}

let fromModifiersStrict = \(modifiers: Modifiers) -> \(key_code: Text) -> fromKeyCode key_code // {
    modifiers = Some (fmStrict modifiers)
}
let fromModifier = \(modifier: Text) -> fromModifiers [modifier]

let fromCtrl = fromModifier "control"

let fromCtrlShift = fromModifiers ["control", "shift"]

let To = {
    key_code: Optional Text,
    pointing_button: Optional Text,
    modifiers: Optional Modifiers
}

let nullTo = { key_code = None Text, pointing_button = None Text, modifiers = None Modifiers }: To

let toKeyCode = \(key_code: Text) -> nullTo // { key_code = Some key_code }: To

let showTo = \(f: To) -> showOptional Modifiers showModifiers f.modifiers ++ " " ++ showOptionalText f.key_code ++ " " ++ showOptionalText f.pointing_button

let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep.dhall

let showListTo = concatMapSep " " To showTo

let toModifiers = \(modifiers: Modifiers) -> \(key_code: Text) -> toKeyCode key_code // {
    modifiers = Some modifiers
}

let toModifier = \(modifier: Text) -> toModifiers [modifier]

let toCommand = toModifier "command"

let toOption = toModifier "option"

let Condition = {
    bundle_identifiers: List Text,
    type: Text
}

let unlessApp = \(apps: List Text) -> {
    bundle_identifiers = apps,
    type = "frontmost_application_unless"
}

let ifApp = \(apps: List Text) -> {
    bundle_identifiers = apps,
    type = "frontmost_application_if"
}

let concat = https://prelude.dhall-lang.org/List/concat.dhall

let terminalApps = [
    "^com\\.apple\\.Terminal$",
    "^com\\.googlecode\\.iterm2$"
]

let terminals: Condition = ifApp terminalApps

let niceApps = [
    "^com\\.jetbrains\\.intellij\\.ce$",
    "^net\\.kovidgoyal\\.kitty$",
    "^org\\.virtualbox\\.app\\.VirtualBoxVM$"
]

let unlessNiceApp: Condition = unlessApp niceApps

let unlessNiceAppOrTerminal: Condition = unlessApp (concat Text [terminalApps, niceApps])

let vimApps = [
    "^com\\.microsoft\\.VSCode$",
    "^com\\.todesktop\\.230313mzl4w4u92$" -- Cursor
]

let unlessVim: Condition = unlessApp (concat Text [terminalApps, niceApps, vimApps])

let browser: Condition = ifApp [
    "^org\\.mozilla\\.firefox$"
]

let Manipulator = {
    conditions: Optional (List Condition),
    from: From,
    to: Optional (List To),
    to_if_alone: Optional (List To),
    type: Text
}

let showManip = \(m: Manipulator) -> showFrom m.from ++ " -> " ++ showOptional (List To) showListTo m.to

let manipulatorForConditions = \(conditions: Optional (List Condition)) -> \(from: From) -> \(to: List To) -> {
    type = "basic",
    conditions = conditions,
    from = from,
    to_if_alone = None (List To),
    to = Some to
}: Manipulator

let manipulatorFor = \(condition: Condition) -> manipulatorForConditions (Some [condition])

let manipulatorForAll = manipulatorForConditions (None (List Condition))

let manipulator = manipulatorFor unlessNiceAppOrTerminal

let controlToCommand = \(key_code: Text) -> manipulator (fromCtrl key_code) [toCommand key_code]

let controlToOption = \(key_code: Text) -> manipulator (fromCtrl key_code) [toOption key_code]

let Rule = {
    description: Text,
    manipulators: List Manipulator
}

let rule = \(manipulator: Manipulator) -> { description = showManip manipulator, manipulators = [manipulator] }: Rule

let SimpleModification = {
    from: From,
    to: List To
}

let map = https://prelude.dhall-lang.org/List/map.dhall

let manipulators1 = [
    -- Mouse
    manipulator
        (nullFrom // { pointing_button = Some "button1", modifiers = Some { mandatory = Some ["control"], optional = None (List Text) } })
        [nullTo // { pointing_button = Some "button1", modifiers = Some ["command"]}],
    -- Insert
    manipulator (fromCtrl "insert") [toCommand "c"],
    manipulator (fromModifier "shift" "insert") [toCommand "v"],
    -- Home
    manipulator (fromKeyCode "home") [toCommand "left_arrow"],  -- TODO: Exclude Firefox
    manipulator (fromCtrl "home") [toCommand "up_arrow"],
    manipulator (fromModifier "shift" "home") [toModifiers ["command", "shift"] "left_arrow"], -- TODO: above with shift
    manipulator (fromCtrlShift "home") [toModifiers ["command", "shift"] "up_arrow"], -- TODO: above with shift
    -- End
    manipulator (fromKeyCode "end") [toCommand "right_arrow"],  -- TODO: Exclude Firefox
    manipulator (fromCtrl "end") [toCommand "down_arrow"],
    manipulator (fromModifier "shift" "end") [toModifiers ["command", "shift"] "right_arrow"], -- TODO: above with shift
    manipulator (fromCtrlShift "end") [toModifiers ["command", "shift"] "down_arrow"], -- TODO: above with shift
    -- Left arrow
    manipulator (fromCtrl "left_arrow") [toOption "left_arrow"],
    manipulator (fromCtrlShift "left_arrow") [toModifiers ["option", "shift"] "left_arrow"],
    -- Right arrow
    manipulator (fromCtrl "right_arrow") [toOption "right_arrow"],
    manipulator (fromCtrlShift "right_arrow") [toModifiers ["option", "shift"] "right_arrow"],
    -- Backspace
    controlToOption "delete_or_backspace",
    -- Delete
    controlToOption "delete_forward",
    -- Page Up/Down
    manipulatorFor terminals (fromCtrl "page_up") [toCommand "left_arrow"],
    manipulatorFor terminals (fromCtrl "page_down") [toCommand "right_arrow"],
    -- A-Z
    controlToCommand "a",
    controlToCommand "b",
    manipulatorFor browser (fromCtrlShift "c") [toModifiers ["command", "option"] "c"],
    manipulatorFor terminals (fromCtrlShift "c") [toCommand "c"],
    controlToCommand "c",
    controlToCommand "e",
    manipulatorFor unlessNiceApp (fromCtrl "f") [toCommand "f"],
    manipulatorFor browser (fromCtrlShift "i") [toModifiers ["command", "option"] "i"],
    controlToCommand "i",
    controlToCommand "k",
    controlToCommand "n",
    controlToCommand "o",
    controlToCommand "p",
    manipulatorFor unlessVim (fromCtrl "r") [toCommand "r"],
    controlToCommand "s",
    controlToCommand "t",
    manipulatorFor terminals (fromCtrlShift "t") [toCommand "t"],
    manipulatorFor browser (fromCtrl "l") [toCommand "l"],
    manipulatorFor browser (fromCtrlShift "p") [toModifiers ["command", "shift"] "p"],
    controlToCommand "u",
    manipulatorFor unlessVim (fromCtrl "v") [toCommand "v"],
    manipulatorFor terminals (fromCtrlShift "v") [toCommand "v"],
    controlToCommand "w",
    controlToCommand "x",
    controlToCommand "y",
    controlToCommand "z",
    manipulatorFor unlessNiceApp (fromCtrl "hyphen") [toCommand "hyphen"],
    manipulatorFor unlessNiceApp (fromCtrl "equal_sign") [toCommand "equal_sign"],
    manipulatorFor unlessNiceApp (fromCtrl "0") [toCommand "0"],
    -- Window manipulation (Gnome defaults to Rectangle defaults)
    manipulatorForAll (fromModifiersStrict ["command"] "left_arrow") [toModifiers ["control", "option"] "left_arrow"],
    manipulatorForAll (fromModifiersStrict ["command"] "right_arrow") [toModifiers ["control", "option"] "right_arrow"],
    manipulatorForAll (fromModifiersStrict ["command"] "up_arrow") [toModifiers ["control", "option"] "return_or_enter"],
    manipulatorForAll (fromModifiersStrict ["command", "shift"] "left_arrow") [toModifiers ["control", "option", "command"] "left_arrow"],
    manipulatorForAll (fromModifiersStrict ["command", "shift"] "right_arrow") [toModifiers ["control", "option", "command"] "right_arrow"],
    -- Generic OS and windows management
    manipulatorForAll (fromModifier "option" "f4") [toCommand "q"],
    manipulatorForAll (fromModifier "option" "grave_accent_and_tilde") [toModifier "left_command" "grave_accent_and_tilde"],
    manipulatorForAll (fromModifier "command" "l") [toModifiers ["control", "command"] "q"]
]

let enumerate = https://prelude.dhall-lang.org/Natural/enumerate.dhall

let drop = https://prelude.dhall-lang.org/List/drop.dhall

let showNatural = https://prelude.dhall-lang.org/Natural/show.dhall

let replicate = https://prelude.dhall-lang.org/List/replicate.dhall

let dockSwitch = \(i: Natural) ->
    manipulatorForAll
        (fromModifier "command" (showNatural i))
        (concat To [
            [toModifier "control" "f3"],
            [toKeyCode "home"],
            (replicate i To (toKeyCode "right_arrow")),
            [toKeyCode "return_or_enter"]
        ])

let manipulators2 = map Natural Manipulator dockSwitch (drop 1 Natural (enumerate 10)) : List Manipulator

let tabSwitch = \(i: Natural) ->
    manipulatorFor browser
        (fromModifiersStrict ["option"] (showNatural i))
        [toModifier "command" (showNatural i)]

let manipulators3 = map Natural Manipulator tabSwitch (drop 1 Natural (enumerate 10)) : List Manipulator

let manipulators = concat Manipulator [manipulators1, manipulators2, manipulators3]

let Identifiers = {
    is_keyboard: Bool,
    product_id: Optional Natural,
    vendor_id: Optional Natural
}

let builtInKeyboard: Identifiers = {
    is_keyboard = True,
    vendor_id = None Natural,
    product_id = None Natural
}

let sydneyOfficeKeyboard: Identifiers = {
    is_keyboard = True,
    vendor_id = Some 7247,
    product_id = Some 67
}

let melbourneHubKeyboard: Identifiers = {
    is_keyboard = True,
    vendor_id = Some 7247,
    product_id = Some 99
}

let DeviceRules = {
    identifiers: Identifiers,
    simple_modifications: List SimpleModification
}

let macLikeKeyboardRules = \(device: Identifiers) -> {
    identifiers = device,
    simple_modifications = [
        {
            from = fromKeyCode "left_command",
            to = [ toKeyCode "left_option" ]
        },
        {
            from = fromKeyCode "left_option",
            to = [ toKeyCode "left_command" ]
        }
    ]
}

in
{
    profiles = [
        {
            complex_modifications = {
                rules = map Manipulator Rule rule manipulators
            },
            devices = map Identifiers DeviceRules macLikeKeyboardRules [builtInKeyboard, sydneyOfficeKeyboard, melbourneHubKeyboard],
            name = "Default profile",
            selected = True,
            virtual_hid_keyboard = {
                caps_lock_delay_milliseconds = 0,
                country_code = 0,
                keyboard_type = "ansi",
                keyboard_type_v2 = "ansi"
            }
        }
    ]
}

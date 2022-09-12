let optionFold = https://prelude.dhall-lang.org/Optional/fold

let showOptional = \(t: Type) -> \(show: t -> Text) -> \(value: Optional t) -> optionFold t value Text show ""

let Modifiers = List Text

let concatSep = https://prelude.dhall-lang.org/Text/concatSep

let showModifiers = \(m: Modifiers) -> concatSep " " m

let showOptionalModifiers = showOptional Modifiers showModifiers

let FromModifiers = {
    mandatory: Optional Modifiers,
    optional: Optional Modifiers
}

let showFromModifiers = \(fm: FromModifiers) -> showOptionalModifiers fm.mandatory

let modifiers: FromModifiers = {
    mandatory = None Modifiers,
    optional = None Modifiers
}

let From = {
    key_code: Text,
    modifiers: Optional FromModifiers
}

let showFrom = \(f: From) -> showOptional FromModifiers showFromModifiers f.modifiers ++ " " ++ f.key_code

let fromModifiers = \(modifiers: Modifiers) -> \(key_code: Text) -> {
    key_code = key_code,
    modifiers = Some {
        mandatory = Some modifiers,
        optional = Some ["any"]
    }
}

let fromKeyCode = \(key_code: Text) -> {
    key_code = key_code,
    modifiers = None FromModifiers
}

let fromModifier = \(modifier: Text) -> fromModifiers [modifier]

let fromCtrl = fromModifier "control"

let fromCtrlShift = fromModifiers ["control", "shift"]

let To = {
    key_code: Text,
    modifiers: Optional Modifiers
}

let showTo = \(f: To) -> showOptional Modifiers showModifiers f.modifiers ++ " " ++ f.key_code

let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

let showListTo = concatMapSep " " To showTo

let toModifiers = \(modifiers: Modifiers) -> \(key_code: Text) -> {
    key_code = key_code,
    modifiers = Some modifiers
}

let toKeyCode = \(key_code: Text) -> {
    key_code = key_code,
    modifiers = None Modifiers
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

let concat = https://prelude.dhall-lang.org/List/concat

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
    "^com\\.microsoft\\.VSCode$"
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

let manipulatorForConditions = \(conditions: Optional (List Condition)) -> \(from: From) -> \(to: To) -> {
    type = "basic",
    conditions = conditions,
    from = from,
    to_if_alone = None (List To),
    to = Some [to]
}: Manipulator

let manipulatorFor = \(condition: Condition) -> manipulatorForConditions (Some [condition])

let manipulatorForAll = manipulatorForConditions (None (List Condition))

let manipulator = manipulatorFor unlessNiceAppOrTerminal

let controlToCommand = \(key_code: Text) -> manipulator (fromCtrl key_code) (toCommand key_code)

let controlToOption = \(key_code: Text) -> manipulator (fromCtrl key_code) (toOption key_code)

let Rule = {
    description: Text,
    manipulators: List Manipulator
}

let rule = \(manipulator: Manipulator) -> { description = showManip manipulator, manipulators = [manipulator] }: Rule

let map = https://prelude.dhall-lang.org/List/map

in
{
    global = {
        check_for_updates_on_startup = True,
        show_in_menu_bar = True,
        show_profile_name_in_menu_bar = False
    },
    profiles = [
        {
            complex_modifications = {
                parameters = {
                    `basic.simultaneous_threshold_milliseconds` = 50,
                    `basic.to_delayed_action_delay_milliseconds` = 500,
                    `basic.to_if_alone_timeout_milliseconds` = 1000,
                    `basic.to_if_held_down_threshold_milliseconds` = 500,
                    `mouse_motion_to_scroll.speed` = 100
                },
                rules = map Manipulator Rule rule [
                    -- Insert
                    manipulator (fromCtrl "insert") (toCommand "c"),
                    manipulator (fromModifier "shift" "insert") (toCommand "v"),
                    -- Home
                    manipulator (fromKeyCode "home") (toCommand "left_arrow"),  -- TODO: Exclude Firefox
                    manipulator (fromCtrl "home") (toCommand "up_arrow"),
                    manipulator (fromModifier "shift" "home") (toModifiers ["command", "shift"] "left_arrow"), -- TODO: above with shift
                    manipulator (fromCtrlShift "home") (toModifiers ["command", "shift"] "up_arrow"), -- TODO: above with shift
                    -- End
                    manipulator (fromKeyCode "end") (toCommand "right_arrow"),  -- TODO: Exclude Firefox
                    manipulator (fromCtrl "end") (toCommand "down_arrow"),
                    manipulator (fromModifier "shift" "end") (toModifiers ["command", "shift"] "right_arrow"), -- TODO: above with shift
                    manipulator (fromCtrlShift "end") (toModifiers ["command", "shift"] "down_arrow"), -- TODO: above with shift
                    -- Left arrow
                    manipulator (fromCtrl "left_arrow") (toOption "left_arrow"),
                    manipulator (fromCtrlShift "left_arrow") (toModifiers ["option", "shift"] "left_arrow"),
                    -- Right arrow
                    manipulator (fromCtrl "right_arrow") (toOption "right_arrow"),
                    manipulator (fromCtrlShift "right_arrow") (toModifiers ["option", "shift"] "right_arrow"),
                    -- Backspace
                    controlToOption "delete_or_backspace",
                    -- Delete
                    controlToOption "delete_forward",
                    -- Page Up/Down
                    manipulatorFor terminals (fromCtrl "page_up") (toCommand "left_arrow"),
                    manipulatorFor terminals (fromCtrl "page_down") (toCommand "right_arrow"),
                    --
                    controlToCommand "a",
                    controlToCommand "b",
                    controlToCommand "c",
                    manipulatorFor terminals (fromCtrlShift "c") (toCommand "c"),
                    manipulatorFor unlessNiceApp (fromCtrl "f") (toCommand "f"),
                    controlToCommand "i",
                    controlToCommand "k",
                    controlToCommand "n",
                    controlToCommand "o",
                    controlToCommand "p",
                    manipulatorFor unlessVim (fromCtrl "r") (toCommand "r"),
                    controlToCommand "s",
                    controlToCommand "t",
                    manipulatorFor terminals (fromCtrlShift "t") (toCommand "t"),
                    manipulatorFor browser (fromCtrl "l") (toCommand "l"),
                    manipulatorFor browser (fromCtrlShift "p") (toModifiers ["command", "shift"] "p"),
                    controlToCommand "u",
                    manipulatorFor unlessVim (fromCtrl "v") (toCommand "v"),
                    manipulatorFor terminals (fromCtrlShift "v") (toCommand "v"),
                    controlToCommand "w",
                    controlToCommand "x",
                    controlToCommand "y",
                    controlToCommand "z",
                    manipulatorFor unlessNiceApp (fromCtrl "hyphen") (toCommand "hyphen"),
                    manipulatorFor unlessNiceApp (fromCtrl "equal_sign") (toCommand "equal_sign"),
                    manipulatorFor unlessNiceApp (fromCtrl "0") (toCommand "0"),
                    manipulatorForAll (fromModifier "option" "f4") (toCommand "q"),
                    manipulatorForAll (fromModifier "option" "grave_accent_and_tilde") (toModifier "left_command" "grave_accent_and_tilde"),
                    manipulatorForAll (fromModifier "command" "l") (toModifiers ["control", "command"] "q")
                ]: List Rule
            },
            devices = [
                {
                    disable_built_in_keyboard_if_exists = False,
                    fn_function_keys = []: List Text,
                    identifiers = {
                        is_keyboard = True,
                        is_pointing_device = False,
                        product_id = 632,
                        vendor_id = 1452
                    },
                    ignore = False,
                    manipulate_caps_lock_led = True,
                    simple_modifications = []: List Text
                },
                {
                    disable_built_in_keyboard_if_exists = False,
                    fn_function_keys = []: List Text,
                    identifiers = {
                        is_keyboard = True,
                        is_pointing_device = False,
                        product_id = 34304,
                        vendor_id = 1452
                    },
                    ignore = False,
                    manipulate_caps_lock_led = True,
                    simple_modifications = []: List Text
                }
            ],
            fn_function_keys = []: List Text,
            name = "Default profile",
            parameters = {
                delay_milliseconds_before_open_device = 1000
            },
            selected = True,
            simple_modifications = []: List Text,
            virtual_hid_keyboard = {
                caps_lock_delay_milliseconds = 0,
                country_code = 0,
                keyboard_type = "ansi",
                mouse_key_xy_scale = 100
            }
        }
    ]
}

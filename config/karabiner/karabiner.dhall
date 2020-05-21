let Modifiers = List Text

let FromModifiers = {
    mandatory: Optional Modifiers,
    optional: Optional Modifiers
}

let modifiers: FromModifiers = {
    mandatory = None Modifiers,
    optional = None Modifiers
}

let From = {
    key_code: Text,
    modifiers: Optional FromModifiers
}

let fromModifiers = \(modifiers: Modifiers) -> \(key_code: Text) -> {
    key_code = key_code,
    modifiers = Some {
        mandatory = Some modifiers,
        optional = None Modifiers
    }
}

let fromKeyCode = \(key_code: Text) -> {
    key_code = key_code,
    modifiers = None FromModifiers
}

let fromModifier = \(modifier: Text) -> fromModifiers [modifier]

let fromCtrl = fromModifier "control"

let To = {
    key_code: Text,
    modifiers: Optional Modifiers
}

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

let niceApps = [
    "^com\\.apple\\.Terminal$",
    "^com\\.googlecode\\.iterm2$",
    "^io\\.alacritty$",
    "^com\\.jetbrains\\.intellij\\.ce$"
]

let unlessNiceApp: Condition = {
    bundle_identifiers = niceApps,
    type = "frontmost_application_unless"
}

let browser: Condition = {
    bundle_identifiers = [
        "^org\\.mozilla\\.firefox$"
    ],
    type = "frontmost_application_if"
}

let Manipulator = {
    conditions: Optional (List Condition),
    from: From,
    to: Optional (List To),
    to_if_alone: Optional (List To),
    type: Text
}

let manipulatorForConditions = \(conditions: Optional (List Condition)) -> \(from: From) -> \(to: To) -> {
    type = "basic",
    conditions = conditions,
    from = from,
    to_if_alone = None (List To),
    to = Some [to]
}: Manipulator

let manipulatorFor = \(condition: Condition) -> manipulatorForConditions (Some [condition])

let manipulatorForAll = manipulatorForConditions (None (List Condition))

let manipulator = manipulatorFor unlessNiceApp

let controlToCommand = \(key_code: Text) -> manipulator (fromCtrl key_code) (toCommand key_code)

let controlToOption = \(key_code: Text) -> manipulator (fromCtrl key_code) (toOption key_code)

let Rule = {
    description: Text,
    manipulators: List Manipulator
}

let rule = \(manipulator: Manipulator) -> { description = "Rule", manipulators = [manipulator] }: Rule

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
                    manipulator (fromModifiers ["control", "shift"] "home") (toModifiers ["command", "shift"] "up_arrow"), -- TODO: above with shift
                    -- End
                    manipulator (fromKeyCode "end") (toCommand "right_arrow"),  -- TODO: Exclude Firefox
                    manipulator (fromCtrl "end") (toCommand "down_arrow"),
                    manipulator (fromModifier "shift" "end") (toModifiers ["command", "shift"] "right_arrow"), -- TODO: above with shift
                    manipulator (fromModifiers ["control", "shift"] "end") (toModifiers ["command", "shift"] "down_arrow"), -- TODO: above with shift
                    -- Left arrow
                    manipulator (fromCtrl "left_arrow") (toOption "left_arrow"),
                    manipulator (fromModifiers ["control", "shift"] "left_arrow") (toModifiers ["option", "shift"] "left_arrow"),
                    -- Right arrow
                    manipulator (fromCtrl "right_arrow") (toOption "right_arrow"),
                    manipulator (fromModifiers ["control", "shift"] "right_arrow") (toModifiers ["option", "shift"] "right_arrow"),
                    -- Backspace
                    controlToOption "delete_or_backspace",
                    -- Delete
                    controlToOption "delete_forward",
                    -- Win
                    {
                        conditions = None (List Condition),
                        type = "basic",
                        from = fromKeyCode "command",
                        to_if_alone = Some [toKeyCode "launchpad"],
                        to = None (List To)
                    },
                    controlToCommand "a",
                    controlToCommand "b",
                    controlToCommand "c",
                    controlToCommand "f",
                    controlToCommand "i",
                    controlToCommand "n",
                    controlToCommand "o",
                    controlToCommand "p",
                    controlToCommand "r",
                    controlToCommand "s",
                    controlToCommand "t",
                    manipulatorFor browser (fromCtrl "l") (toCommand "l"),
                    controlToCommand "u",
                    controlToCommand "v",
                    controlToCommand "w",
                    controlToCommand "x",
                    controlToCommand "y",
                    controlToCommand "z",
                    controlToCommand "slash", -- TODO: even nice applications are included?
                    manipulator (fromKeyCode "f1") (toModifiers ["command", "shift"] "slash"),
                    manipulator (fromKeyCode "f3") (toCommand "g"),
                    manipulatorForAll (fromModifier "option" "f4") (toCommand "q"),
                    manipulatorForAll (fromModifier "option" "grave_accent_and_tilde") (toModifier "left_command" "grave_accent_and_tilde"),
                    manipulatorForAll (fromModifier "option" "tab") (toModifier "left_command" "tab")
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

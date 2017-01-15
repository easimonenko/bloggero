module Alert.AlertLevel exposing (Level(..), toCSSClassName)


type Level
    = SuccessLevel
    | InfoLevel
    | WarningLevel
    | DangerLevel
    | NoneLevel


toCSSClassName : Level -> String
toCSSClassName level =
    "alert-"
        ++ case level of
            SuccessLevel ->
                "success"

            InfoLevel ->
                "info"

            WarningLevel ->
                "warning"

            DangerLevel ->
                "danger"

            NoneLevel ->
                "none"

module Domain.Commands (
    Command(..)
  , fromMaybeString
) where


data Command = Add | Delete | List | Help | Find | Update
    deriving( Eq )


_fromString :: String -> Command
_fromString []       = Help
_fromString "-add"   = Add
_fromString "-del"   = Delete
_fromString "-lst"  = List
_fromString "-upd"   = Update
_fromString "-help"  = Help
_fromString (x : xs) = if x == '-' then Help else Find


fromMaybeString :: Maybe String -> Command
fromMaybeString (Just str)  = _fromString str
fromMaybeString Nothing     = Help


instance Show Command where
    show Add    = "-add"
    show Delete = "-del"
    show Update = "-upd"
    show List   = "-lst"
    show Help   = "-help"
    show Find   = ""


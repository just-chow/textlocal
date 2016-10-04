import Data.Text

data ErrorMsg
    = NoCommandSpecified
    | UnrecognizedCommand
    | InvalidLoginDetails
    | OtherError Text
    deriving (Eq,Ord,Show)

data WarningMsg
    = UnrecognizedResponse
    | OtherWarning Text
    deriving (Eq,Ord,Show)

data Error = Error
    { ecode :: Int
    , emessage :: ErrorMsg
    } deriving (Eq,Ord,Show)

data Warning = Warning
    { wcode :: Int
    , wmessage :: WarningMsg
    } deriving (Eq,Ord,Show)
module UI.Names where

data Name =
    NameField
  | Name String
  | Nop
  | BuildList
  | OrgList
  | LogList
  | LogsVP
  deriving (Show, Eq, Ord)

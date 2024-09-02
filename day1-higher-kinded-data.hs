{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)

data Gender = Male | Female deriving (Show)

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

data Person m = Person
  { name :: !(HKD m String),
    gender :: !(HKD m Gender)
  }

deriving instance Show (Person Identity)

deriving instance Show (Person Maybe)

definition :: Person (Const String)
definition = Person (Const "column_name") (Const "column_gender")

-- >>> show person
-- "Person {name = \"\", gender = Male}"
person :: Person Identity
person = Person "" Male

-- >>> show partial
-- "Person {name = Just \"John\", gender = Nothing}"
partial :: Person Maybe
partial = Person (Just "John") Nothing

update :: Person Maybe -> Person Identity -> Person Identity
update l r =
  Person (fromMaybe r.name l.name) (fromMaybe r.gender l.gender)

write :: Person (Const String) -> Person Identity -> String
write def p =
  "["
    <> getConst def.name
    <> " = "
    <> p.name
    <> " , "
    <> getConst def.gender
    <> " = "
    <> show p.gender
    <> "]"

-- >>> write definition $ update partial person
-- "[column_name = John , column_gender = Male]"

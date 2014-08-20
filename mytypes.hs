
newtype Img = Integer Integer deriving (Show)

type Nickname = String
type Sex = Char
type Age = Int

data Person = Person Nickname Age Sex deriving (Show)

data Booll = True | False deriving Show
data Color = Blue | Red | Green deriving Show
data Point a = Pt a a deriving Show

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
  deriving (Eq, Ord, Enum)

data Expression = Char
                | Add Expression Expression
                | Minus Expression
                | Mult Expression Expression
                | Divide Expression Expression




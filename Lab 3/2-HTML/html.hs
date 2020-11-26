
module Template where

-- * HTML
-- ----------------------------------------------------------------------------

-- Simple (X)HTML markup.
data Attr = Attr String String
  deriving (Eq,Show)

data HtmlElement
  = HtmlString String                    -- Plain text.
  | HtmlTag String [Attr] HtmlElements   -- Structured markup.
  deriving (Eq, Show)

type HtmlElements = [HtmlElement]

example :: HtmlElement
example =
  HtmlTag "a" [Attr "href" "https://www.kuleuven.be/kuleuven/"]
    [HtmlString "KU Leuven"]

-- HTML renderable class.
class HTML a where
  toHtml :: a -> HtmlElement

data Link =
  Link
    String  -- Link target.
    String  -- Text to show.
  deriving (Eq,Show)

instance HTML Link where
  toHtml (Link targetLink txt)= HtmlTag "a" [Attr "href" targetLink] [HtmlString txt]

instance HTML a => HTML [a] where
  toHtml [] = HtmlTag "ul" [] []
  toHtml (x:xs) = HtmlTag "ul" [] ([toHtml x] ++ [toHtml xs])

{-
helper :: HtmlElements -> [String]
helper [] = []
helper (HtmlString str):xs = [str] ++ helper xs
helper (HtmlTag _ _ htmlEllems):xs = helper htmlEllems ++ helper xs
-}

-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Apples</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>
exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" [] [HtmlTag "li" [] [HtmlString "Apples"], HtmlTag "li" [] [HtmlString "Bananas"], HtmlTag "li" [] [HtmlString "Oranges"]]

data AddressBook = AddressBook [Person]

data Person = Person
 String -- name
 [Email]

data Email= Priv String | Work String deriving (Show)


myAddressBook :: AddressBook
myAddressBook = AddressBook [Person "me" [Work "me@vtk.be", Priv "me@home.be"], Person "me2" [Priv "me2@home.be"] ]
--exercise says it should be called "exampleAddressBook" therefor we copy paste
exampleAddressBook :: AddressBook
exampleAddressBook = AddressBook [Person "me" [Work "me@vtk.be", Priv "me@home.be"], Person "me2" [Priv "me2@home.be"] ]


instance HTML AddressBook where
  toHtml (AddressBook personen)= HtmlTag "AddressBook" [] (helperPersonen personen)

helperPersonen :: [Person] -> HtmlElements
helperPersonen []                        = []
helperPersonen ((Person name emails):xs) = (HtmlTag "Person" [Attr "Name" name] (helperEmails emails)) : (helperPersonen xs)

helperEmails :: [Email] -> HtmlElements
helperEmails []                = []
helperEmails ((Priv email):xs) = (HtmlTag "email" [Attr "priv" email] []) : (helperEmails xs)
helperEmails ((Work email):xs) = (HtmlTag "email" [Attr "work" email] []) : (helperEmails xs)



-- file: ch03/BookStore.hs
-- We can create individual types like this
data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

-- Then, this is how we create instances of it
myInfo = Book 8675309 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

-- Sometimes its nicer to 'typedef' some of them to make them easier to read
type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody 
type BookRecord = (BookInfo, BookReview)

-- These are manual 'getters'; basically showing wildcarding;
bookID      (Book id _ _) = id
bookTitle   (Book _ title _) = title
bookAuthors (Book _ _ authors) = authors

-- We can also have algebraic data types (well, everything is, but we can use the | to add other alternatives
type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- We can also get away from boilerplate getters by using record syntax
data Customer = Customer {
    customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
} deriving (Show)

-- We can still use the old syntax
customer1 = Customer 1337 "J.R. Hacker" ["255 Syntax Ct", "Milpitas, CA 95134", "USA"]

-- Or we can use the new record syntax
customer2 = Customer {
    customerID = 55555,
    customerAddress = ["1048576 Disk Drive", "Multpitas, CA 95134", "USA"],
    customerName = "Jane Q. Citizen"
}

sayHello :: String -> IO()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

-- where defines declarations
printInc n = print plusTwo
  where
    plusTwo = n + 2

-- let defines expressions
printInc2 n =
  let plusTwo = n + 2
  in print plusTwo

-- EXERCISES
let x = 5 in x     -- 5
let x = 5 in x * x -- 25
let x = 5; y = 6 in x * y -- 30
let x = 3; y = 1000 in x + 3 --6

-- Rewrite with where clauses
let x = 3; y = 1000 in x * 3 + y

addMulti = x * 3 + y
  where
    x = 3
    y = 1000

let y = 10; x = 10 * 5 + y in x * 5

timesFive = x * 5
  where
    y = 10
    x = 10 * 5

let x = 7; y = negate x; z = y * 10 in z / x + y

divideAndSum = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10

-- Rewrite using where
waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y^2

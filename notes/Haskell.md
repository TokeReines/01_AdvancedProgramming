# Haskell

## Miscellaneous commands

Launch an interactive Haskell interpreter: `ghci`
Change the interactive interpreter prefix: `:set prompt "ghci> "`

## Within the interactive interpreter

Load a script into the interactive environment `:l fileNameWithoutExtension`

## Notes

### Functions, statements, and definations (2. Starting Out)

- If statements <u>ALWAYS</u> has to return, thus contain a `else` statement.
- An expression is a piece of code that returns a value, such as `5`, `4+8`, `x+y`, and a if statement.
- Functions can't begin with uppercase letters.
- When a function doesn't take any parameters, it's called a definition (or a name).
- `'` is a valid character to use in a function name. It's usually used to either denote a strict version of a function (one that isn't lazy) or a slightly modified version of a function or a variable

### Lists, strings, and list comprehensions (2. An intro to lists)

- Stores several elements of <u>the same type</u>
- `++` adds two lists together. "Ineffective" as the left list has to be walkedthrough to add something at the end.
- `:` (also called the cons operator) is effective and adds something at the beginning of a list. Therefore `[1,2,3]` is syntactic sugar for `1:2:3:[]`
– A list can also be a list of lists e.g. `[[], [], []]`
- `!!` gets an element out of a list by index.
- List can be compared in lexicographical order with `<`, `<=`, `>`, `>=`. First the heads are compared. If they are equal then the second elements are compared, etc.
- List functions:
  - `head [x,y,z]` returns the head (first element, see figure below)
  - `tail [x,y,z]` returns the tail (chops of head. see figure below)
  - `last [x,y,z]` returns the last element (see figure below)
  - `init [x,y,z]` returns except last element (opisite of tail, see figure below)
  - `length [x,y,z]` returns the length of a list.
  - `length [x,y,z]` returns the length of a list.
  - `null [x, y, z]` checks if a list is empty.
  - `reverse [x, y, z]` reverses a list.
  - `take x [x, y, z]` extracts `x` elements from the beginning of the list.
  - `drop x [x, y, z]` drops `x` elements from the beginning of the list.
  - `maximum [x, y, z]` returns the biggest element. (Has to be orderable).
  - `minimum [x, y, z]` returns the smallest element. (Has to be orderable).
  - `sum [x, y, z]` returns the sum of a list of numbers.
  - `product [x, y, z]` returns the product of a list of numbers.
  - ``x `elem` [x, y, z]`` returns a bool telling if x is an element of the list.

```text
| head |    tail     |
|------|-------------|  
|  x      y       z  |
|-------------|------|
|     init    | last |
```

- Ranges:
  - Numbers: `[1..10]` = `[1,2,3,4,5,6,7,8,9,10]`
  - Numbers (reverse): `[10,9..1]` = `[10,9,8,7,6,5,4,3,2,1]`
  - Chars (lowercase): ``['a'..'z']``= `"abcdefghijklmnopqrstuvwxyz"`
  - Chars (uppercase): ``['K'..'Z']``= `"KLMNOPQRSTUVWXYZ"`
  - Numbers in steps: `[2,4..20]` = `[2,4,6,8,10,12,14,16,18,20]`
  - Don't use ranges with floating point numbers.
  - Infinite lists: `take 24 [13,26..]` returns the first 24 multiples of 13. Because Haskell is lazy, it won't try to evaluate the infinite list immediately because it would never finish
    - `cycle` takes a list and cycles it into an infinite list (f you just try to display the result, it will go on forever so you have to slice it off somewhere.): `take 10 (cycle [1,2,3])` = `[1,2,3,1,2,3,1,2,3,1]`
    - `repeat`: takes an element and produces an infinite list of just that element. It's like cycling a list with only one element. `take 10 (repeat 5)` = `[5,5,5,5,5,5,5,5,5,5]`
    - `replicate`: some number of the same element in a list. `replicate 10 5` == `[5,5,5,5,5,5,5,5,5,5]`.

- List comprehension examples:
  - Naming: `[outputFunction | inputSet, predicate]`
  - Weeding out lists by predicates is also called **filtering**. 
  - Basic list comprehension: `[x*2 | x <- [1..10]]` = `[2,4,6,8,10,12,14,16,18,20]`
  - With condition: `[x*2 | x <- [1..10], x*2 >= 12]` = `[12,14,16,18,20]`
  - Numbers from 50 to 100 whose remainder when divided with the number 7 is 3: ``[ x | x <- [50..100], x `mod` 7 == 3]`` = `[52,59,66,73,80,87,94]`
  - With multiple predicates: `[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]`
  - With multiple lists: `[ x*y | x <- [2,5,10], y <- [8,10,11]]`
  - Length function: `length' xs = sum [1 | _ <- xs]`
  - Produce string: `removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]`
  - Nested list comprehensions: `[ [ x | x <- xs, even x ] | xs <- xxs]`
  - Function that replaces each odd number greater than 10 with `"BANG!"` and each odd number that's less than 10 with `"BOOM!"`:

```haskell
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   
```
   
### Tuples (2. Tupples)

- Tuples can be compared with each other if their components can be compared and the tuples have same size.
- `fst` (works only on pairs/2-tuple): takes a pair and returns its first component.
- `snd` (works only on pairs/2-tuple): takes a pair and returns its second component. Surprise!
- `zip`: takes two and combines theme togehter into a list of pairs.
- Combining tuples with list comprehension: 
```haskell
let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
```

### 3. Types and Typeclasses

- `:t x` returns the type of x. Can beused to infere the type of a function afterwards in ghci.
- `::`translates to "has type of"

Explicitit type declaration:

```haskell
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String -- Same thing 
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   
```

Declaring multiple parameters (last declaration is the return type):
```haskell
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  
```

| Type | Represents |
|---|---|
| Int | Between -2147483648 and 2147483647 |
| Integer | Unbounded Int |
| Float | Floating point with single precision |
| Double | Floiting point with double precision |
| Bool | True/False |
| Char | Character |
| String | [Char] |
| () | Tuple |
| [] | List |
| a | any type ("polymorphic" a note written in capital) |

Types are written in capital letters.

```bash
ghci> :t (==)  
(==) :: (Eq a) => a -> a -> Bool 
```

#### Typeclasses (3. Types and Typeclasses)

Everything before the `=>` symbol is called a class constraint.

| Class | Def |
|---|---|
| Eq | its members implement are `==` and `/=`, these used somewhere inside its definition |
| Ord | is for types that have an ordering (`<`, `>`, `<=`, `>=`). |
| Ordering | `GT`, `LT` or `EQ` |
| Show | can be presented as strings with `show` function |
| Read | Opisite of show (`read` function takes string and returns it as a typed. `:: type` where type is Int, Float, etc) might need to be added for letting haskell infer the type |
| Enum | sequentially ordered types — they can be enumerated. Can be used for ranges. `succ` and `pred` can be used. Types in this class: `()`, `Bool`, `Char`, `Ordering`, `Int`, `Interger`, `Float`, `Double` |
| Bounded | Members have an upper and a lower bound, such as: `Int`, `Char`, `Bool`. |
| Num |  is a numeric typeclass. Its members have the property of being able to act like numbers. Let's examine the type of a number. |
| Integral | Int or Integer |
| Floating | Float or Double |

`fromIntegral` function fx adds a num to an Integral.

### 4. Syntax in Function

#### Pattern matching 

You can pattern match on any data type — numbers, characters, lists, tuples, etc.

```haskell
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  -- cath all
```

Recursive example

```haskell
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  
```

Pattern matching with functions example

```haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  
```

Can also be used for list comprehenssion

**As pattern:** `xs@(x:y:ys)` This pattern will match exactly the same thing as `x:y:ys` but you can easily get the whole list via `xs`.

#### Guards

Used for testing whether some property of a value (or several of them) are true or false

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!" 
```

Pattern matching can also be mixed with guards like so:

```haskell
accumulate_list' :: (Eq a, Num a) => [a] -> ( a -> a -> a ) -> a
accumulate_list' l f
    | []     <- l = 0          --pattern for the empty list case
    | 10 < 5      = 10         --arbitrary regular guard just because 
    | (x:xs) <- l = undefined  --pattern for the non-empty case
```

Or as shown in [some of these examples](https://stackoverflow.com/a/52507955)  

Using `where` for not repeating ourselves exmpale:

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
```

or as such

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
    -- or used with pattern matchin as such:
    -- (skinny, normal, fat) = (18.5, 25.0, 30.0)
```

`where` can also be used globally.

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  
```

**Let bindings**:

```haskell
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
```

Has the form `let <bindings> in <expression>`

**Cases**: syntax

```haskell
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
```
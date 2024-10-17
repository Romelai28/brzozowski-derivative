data RegEx = EmptySet
            | Lambda
            | Symbol Char
            | Concatenation [RegEx]
            | Sum [RegEx]
            | KleeneClosure RegEx
            deriving (Show, Eq)

-- A valid list of RegEx must have length > 0.

positiveClosure :: RegEx -> RegEx
positiveClosure re = Concatenation [re, (KleeneClosure re)]

stringConcatenation :: String -> RegEx
stringConcatenation = Concatenation . map Symbol

stringSum :: String -> RegEx
stringSum = Sum . map Symbol


hasLambda :: RegEx -> Bool
hasLambda re = case re of
    EmptySet         -> False
    Lambda           -> True
    Symbol _         -> False
    Concatenation rs -> all hasLambda rs
    Sum rs           -> any hasLambda rs
    KleeneClosure _  -> True


derivativeWithRespectTo :: Char -> RegEx -> RegEx
derivativeWithRespectTo c re = simplify $ case re of
    EmptySet              -> EmptySet
    Lambda                -> EmptySet
    Symbol char           -> if c == char then Lambda else EmptySet
    Concatenation (re:[]) -> derivative re
    Concatenation (re:rs) -> Sum [
                                    Concatenation $ [derivative re] ++ rs,
                                    Concatenation [epsilonOperator re, derivative $ Concatenation rs]
                                ] 
    Sum rs                -> Sum $ map derivative rs
    KleeneClosure re      -> Concatenation [derivative re, KleeneClosure re]
    where
        derivative = simplify . derivativeWithRespectTo c
        epsilonOperator re = if hasLambda re then Lambda else EmptySet 


-- Simplifica de adentro hacia afuera. 
simplify :: RegEx -> RegEx
simplify re = case re of
    -- Concatenar con el conjunto vacio resulta en el conjunto vacio.
    -- Lambda es el elemento neutro en la concatenación.   
    Concatenation rs ->
        let simplifiedRs = simplifyElementsOf rs
            filteredRs = filter (/= Lambda) simplifiedRs
        in  case filteredRs of
            []   -> Lambda  -- Si el filter borro todo, es que solo había lambdas.
            [re] -> simplify re
            _    -> if any (== EmptySet) filteredRs
                        then EmptySet
                        else Concatenation $ simplifyElementsOf filteredRs

    -- EmptySet es el neutro de la suma.
    Sum rs ->
        let simplifiedRs = simplifyElementsOf rs
            filteredRs = filter (/= EmptySet) simplifiedRs
        in case filteredRs of
            []   -> EmptySet  -- Si el filter borro todo, es que solo había emptySets.
            [re] -> simplify re
            _    -> Sum $ simplifyElementsOf filteredRs

    KleeneClosure re -> KleeneClosure $ simplify re

    _ -> re


simplifyElementsOf :: [RegEx] -> [RegEx]
simplifyElementsOf = map simplify


prettyPrint :: RegEx -> String
prettyPrint re = case re of
    EmptySet         -> "∅"
    Lambda           -> "λ"
    Symbol c         -> [c]
    Concatenation rs -> concatMap prettyPrint rs
    Sum rs           -> join " + " (map prettyPrint rs)
    KleeneClosure r -> "(" ++ prettyPrint r ++ ")*"


join :: String -> [String] -> String
join sep xs = foldr (\a b -> a ++ if b=="" then b else sep ++ b) "" xs

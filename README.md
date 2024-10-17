# brzozowski-derivative

example:
prettyPrint $ derivativeWithRespectTo 'a' $ KleeneClosure (Concatenation [Symbol 'a', KleeneClosure (stringConcatenation "ab")])

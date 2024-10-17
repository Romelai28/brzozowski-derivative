# brzozowski-derivative

example:

Input:
`prettyPrint $ derivativeWithRespectTo 'a' $ KleeneClosure (Concatenation [Symbol 'a', KleeneClosure (stringConcatenation "ab")])`

Output:
`(ab)*(a(ab)*)*`

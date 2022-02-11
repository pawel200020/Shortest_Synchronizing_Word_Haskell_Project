# Shortest Synchronizing Word Algorithm
Shortest Synchronizing Word algorithm  implemented in Haskell. Checks if derived automat has a reset word or it not.

## Usage
```
ghc Main.hs 
./Main data/data.txt
```
where data.txt is a file with automat
## File syntax
- fist line alphabet
- state number (Integer)
- pairs when first letter is a letter by whom we can go to step which number is a second item of a pair

(see file examples in data)

Project made for Functional Programming course at Jagiellonian University in winter semester 2021/2022.
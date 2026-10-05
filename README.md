# Quantum Circuit Description Language

The project consists on 3 main stages: 
- The parser (that takes a $\lambda^Q$ term and structures it) ``Lexer.hs`` ``Main.hs`` ``TypeTree.hs``
- The creation of the derivation tree corresponding to the $\lambda^Q$ term ``CreateDerivation.hs`` ``DerivationZipper.hs``
- The building of the circuit corresponding to the $\lambda^Q$ term ``CircuitGraph.hs``

## Installation requirements

- GHC
- Cabal

## Installation
Clone the repository and install dependencies:
```bash
git clone https://github.com/yourname/QuantumCircuitDescriptionLanguage.git
cd QuantumCircuitDescriptionLanguage

Usage

Run the main script on your dataset:

cabal run QuantumCircuitDescriptionLanguage -- examples/the-test-preffered.qqdc

Example Output

cabal run QuantumCircuitDescriptionLanguage -- examples/testFig7Paper.qqdc 

x Lab 1 -------o------- [L] Lab 6
               |       
y Lab 2 --H----X------- [R] Lab 7
```


[License](/LICENSE)

## Project Development

- 31/12/2025
  - Initial creation of a parser for recognizing arithmetic expressions + var + let written in Haskell language
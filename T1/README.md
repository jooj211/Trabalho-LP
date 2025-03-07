# Jogo dos Palitinhos

Implementação do Jogo dos Palitinhos em Haskell para o Trabalho Prático 1 da disciplina de Linguagens de Programação (DCC019 - 2024.2 - UFJF).

Desenvolvido por:
- Jonatas Dias Machado Costa
- Maria Luísa Riolino Guimarães

## Como executar o jogo

### Pré-requisito:
- Pacote random instalado

### Compilação e execução no GHC:
```bash
ghc --make -package random -o jogo Main.hs
./jogo
```
### Execução no GHCi:
```bash
ghci -package random
:l Main.hs
main
```
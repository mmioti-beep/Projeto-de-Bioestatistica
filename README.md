# Projeto de Bioestatística

Este repositório contém o código utilizado para análise dos casos de Síndrome Respiratória Aguda Grave (SRAG) por Vírus Sincicial Respiratório (VSR).

## Arquivos

- Script_bioestatística.R → script principal da análise.

## Banco de dados

O banco SQLite não está incluído neste repositório devido ao seu tamanho (~2 GB).

Ele pode ser baixado em:

(https://drive.google.com/file/d/19Z1Juugq5WBzD9z-LoNAdxENBw2VPv8A/view?usp=sharing)

Após baixar, altere esta linha do script:

```r
dbConnect(
  RSQLite::SQLite(),
  "C:/CAMINHO/DO/SEU/srag_ate25.sqlite"
)
```

para o caminho onde você salvou o arquivo.

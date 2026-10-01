# Saúde no Semiárido

Painel Shiny com TerraClimate 1.1 e indicadores mensais de SIH, SIM e SINAN dengue,
para os municípios da delimitação territorial registrada nos dados. Clima e saúde
são consultados em DuckDB; o aplicativo lê apenas a série ou o mapa selecionado.

## Preparar e abrir

Execute na raiz deste projeto, depois que `../sentseca2_data` concluir a publicação:

```sh
Rscript scripts/prepare_data.R
Rscript -e 'shiny::runApp()'
```

A preparação verifica o relatório de validação e os checksums, lê `health.rds` uma
única vez e cria um banco derivado em `data/cache/`. Essa conversão exige memória
para deserializar a tabela completa; execute-a no ambiente de processamento antes
de iniciar o Shiny. O painel não deserializa essa tabela.

Repetir o comando reaproveita um banco íntegro. `--force` cria uma nova geração;
`--data-dir DIRETORIO` seleciona outra publicação ou candidato validado. Consulte
`Rscript scripts/prepare_data.R --help` para os argumentos.

As variáveis `SENTSECA_DATA_DIR` (padrão `data`) e `SENTSECA_CACHE_DIR` (padrão
`data/cache`) devem apontar para os mesmos diretórios na preparação e no painel:

```sh
export SENTSECA_DATA_DIR=/caminho/para/dados
export SENTSECA_CACHE_DIR=/caminho/para/cache
Rscript scripts/prepare_data.R
Rscript -e 'shiny::runApp()'
```

Dependências do aplicativo e da preparação: shiny, bslib, leaflet, plotly, sf, DBI,
duckdb, digest e filelock. O ambiente reproduzível está em
`../sentseca2_data/renv.lock`; não é preciso baixar microdados para abrir o painel.
`app_b.R` é uma entrada de compatibilidade para a mesma implementação.

## Publicações e recuperação

O produtor publica versões imutáveis em `data/releases/` e troca atomicamente
`data/current.rds`. O cache é identificado pelos checksums de toda a publicação e
pela versão de seu formato. Cada preparação usa arquivos próprios, bloqueio contra
concorrência e ativação atômica depois de validar quantidade de registros, chaves,
taxas e equivalência de uma amostra com o RDS original.

Após cada publicação: prepare o banco e reinicie o aplicativo. Um processo em
execução mantém os bancos da versão que abriu, mesmo que os ponteiros mudem.
Sem um cache compatível, o painel informa indisponibilidade e registra no log o
comando de preparação. Não há conversão automática durante a abertura.

Falhas na preparação preservam a geração ativa. Arquivos de uma preparação
interrompida não são ativados; execute novamente o comando. Publicações e gerações
anteriores são mantidas. Para restaurar a publicação anterior, pare o aplicativo,
substitua `data/current.rds` pelo conteúdo de `data/previous.rds`, execute a
preparação e reinicie. Alternativamente, use `SENTSECA_DATA_DIR` apontando diretamente
para a versão desejada. Não altere arquivos dentro das publicações.

## Interpretação

- TerraClimate: 12 indicadores mensais, com unidades e períodos nos metadados.
- SIH/SUS: internações financiadas pelo SUS, por residência, diagnóstico principal
  e data de internação; SIM: óbitos por causa básica e data do óbito.
- Dengue: casos prováveis por residência e início dos sintomas.
- Taxas mensais: eventos divididos pela população anual compatível, multiplicados
  por 100 mil. Não há anualização ou extrapolação de população.
- Idade ignorada integra as contagens e o total, sem taxa específica. Ausências
  permanecem lacunas; zero é preservado apenas onde foi produzido com cobertura
  confirmada. Dados preliminares são identificados.

A IA PCDaS é opcional e usa Shiny >= 1.8.1, bslib >= 0.7.0, httr2 >= 1.3.0,
promises e typedjs, além de `pcdas_token.R`. O typedjs pode ser instalado com
`remotes::install_github("JohnCoene/typedjs")`. A configuração local é lida somente
após clicar no botão; credenciais não fazem parte dos dados nem do repositório.
O modal abre imediatamente com spinners no botão e na janela. A consulta ocorre
em segundo plano, mantendo o painel disponível, com limite de espera de 60
segundos e uma mensagem específica quando esse limite é atingido.

A IA recebe todos os registros municipais do indicador e período selecionados,
com nome do município, estado e valor, preservando a precisão numérica. Registros
sem observação também são enviados, com valor nulo; ausências não são convertidas
em zero. A IA é instruída a responder em até 120 palavras, apresentar os valores
do indicador e as estatísticas com duas casas decimais e vírgula decimal, e manter
anos e contagens como inteiros. O arredondamento é solicitado apenas na
apresentação, preservando a precisão original nos cálculos. A resposta aparece
com efeito de digitação e nomes em negrito. Até 24 respostas válidas são reutilizadas
na mesma sessão para dados e seleções idênticos; erros não são armazenados. Sem
observações válidas, nenhuma consulta é feita.

## Verificação

```sh
Rscript tests/test-data-access.R
Rscript tests/test-ia-map.R
Rscript tests/test-ia-http.R
Rscript tests/smoke.R
/usr/bin/time -v Rscript tests/benchmark.R
```

Os dois primeiros usam fixtures de teste e respostas simuladas, sem credenciais
ou chamadas à IA. O teste HTTP usa um servidor local e verifica respostas e
tempo limite reais, sem acessar a PCDaS; requer callr. Os demais exigem uma
publicação preparada: verificam seletores,
consultas, taxas, lacunas, municípios e a ausência de leitura integral de saúde na
abertura. O benchmark registra tempo de inicialização e consulta; `time -v`
registra o pico de memória do processo.

Para verificar também o navegador, deixe o aplicativo aberto em `127.0.0.1:8765`
e execute `Rscript tests/browser.R` (requer chromote, jsonlite e Chrome). Esse teste
confere os polígonos e as trocas de seleção, sem acionar a IA. As capturas ficam em
`data/diagnostics/`.

`Rscript tests/browser-ia.R` abre sua própria instância do painel e verifica o
ícone, os spinners, a digitação, o negrito, o reaproveitamento de respostas e
novas tentativas após falhas usando
respostas simuladas com atraso, sem ler credenciais nem consultar a API. Requer
também callr e grava `ia-loading.png` e `ia-response.png` em `data/diagnostics/`.

Medição local em 01/10/2026, com a publicação `20260930T200759-1243406`
(41.542.102 registros de saúde):

| Operação | Tempo | Pico de memória do processo |
|---|---:|---:|
| Preparação completa e validação | 93,15 s | 8,51 GiB |
| Reutilização com verificação de integridade | 5,86 s | 127 MiB |
| Carregamento do aplicativo | 2,77 s | 273 MiB |
| Consulta de uma série de saúde, 222 linhas | 9 ms | — |

Os tempos variam com o ambiente e o cache do sistema operacional. O teste de
abertura bloqueia explicitamente qualquer tentativa de ler `health.rds`.

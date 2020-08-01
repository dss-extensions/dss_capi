[![Travis-CI: estado de compilação em Linux e MacOS](https://travis-ci.com/dss-extensions/dss_capi.svg?branch=master)](https://travis-ci.com/dss-extensions/dss_capi)
[![AppVeyor: estado de compilação em Windows](https://ci.appveyor.com/api/projects/status/eekf7ry9d0cy4k1b?svg=true)](https://ci.appveyor.com/project/PMeira/dss-capi-u9ben)
[![Downloads no GitHub](https://img.shields.io/github/downloads/dss-extensions/dss_capi/total?logo=GitHub&cacheSeconds=86400)](https://github.com/dss-extensions/dss_capi/releases)

*For an English version of this file, see [README.md](https://github.com/dss-extensions/dss_capi/blob/master/README.md).*

# DSS C-API: Uma interface (não oficial) para o OpenDSS do EPRI

Esta biblioteca expõe o motor do OpenDSS/OpenDSS-PM (v7/v8) através de uma interface C plana, que tenta reproduzir a maioria dos métodos COM. De fato, a maior parte do código foi inicialmente derivado dos arquivos da implementação COM. O DLL resultante pode ser usado diretamente ou através de módulos de interface, como o módulo `DSS Python`. DSS Python representa um módulo para linguagem Python que imita a mesma estrutura do módulo COM (como exposto via `win32com` ou `comtypes`), efetivamente nos permitindo alcançar compatilibilidade multi-plataforma a nível de Python. Também há suporte para outras linguagens diversas -- caso tenha interesse numa linguagem não suportada, abra um novo "issue".

<p align="center">
    <img alt="Visão geral dos repositórios relacionados" src="https://raw.githubusercontent.com/dss-extensions/dss_capi/master/docs/images/repomap.png" width=600>
</p>

Caso procure integração com outras linguagens de programação:

- [DSS Python](http://github.com/dss-extensions/dss_python/) é o módulo Python multi-plataforma (Windows, Linux, MacOS) bastante compatível com o módulo COM. Veja também [OpenDSSDirect.py](http://github.com/dss-extensions/OpenDSSDirect.py/) caso não precise de compatibilidade com COM, ou deseje empregar as funcionalidades extras do módulo (inclusive em conjunto).
- [OpenDSSDirect.jl](http://github.com/dss-extensions/OpenDSSDirect.jl/) é um módulo em Julia, criado por Tom Short (@tshort), que recentemente passou a empregar DSS C-API no lugar do DLL direto com a ajuda de Dheepak Krishnamurthy (@kdheepak).
- [DSS Sharp](http://github.com/dss-extensions/dss_sharp/) para .NET/C#, no momento apenas Windows. Em breve também será possível usá-lo via COM.
- [DSS MATLAB](http://github.com/dss-extensions/dss_matlab/) permite integração multi-plataforma (Windows, Linux, MacOS) bastante compatível com o módulo COM, de fato contorna algumas dificuldades de COM.

Esta é a versão 0.10.6, baseada no OpenDSS SVN r2909 (em torno do OpenDSS v9.0.0.3).

Apesar de o objetivo principal (compatibilidade com COM) ter sido alcançado, este é um sempre um trabalho em andamento.
*Observe que, enquanto a interface clássica (v7 + aprimoramentos) é estável, a interface para o OpenDSS-PM (v8, baseada em atores e execução paralela) ainda é experimental.* A partir da versão 0.10, a interface v8 está bem mais estável que na versão 0.9.8 da DSS C-API. A partir da versão 0.10.5, o código da pasta `Version8` não é mais compilado -- uma nova versão unificada é esperada para uma futura versão.

Ao invés de usar parâmetros numéricos como na interface DDLL oficial (OpenDSSDirect), cada propriedade COM foi exposta como um par de funções. Por exemplo, a propriedade `kVA` das cargas é exposta como:

```
    double Loads_Get_kva();
    void Loads_Set_kva(double Value);
```

Com exceção de detalhes de baixo nível como gerenciamento de memória, a maioria da documentação do módulo COM pode ser usada como referência para este projeto.

**A partir da versão 0.9.8, desabilitamos a criação do `opendsscmd.ini`, visto que causava estranheza entre os usuários. O usuário pode configurar a frequência de base padrão usando a variável de ambiente `DSS_BASE_FREQUENCY`, ou simplesmente através de scripts DSS (opção recomendada por nós). Isto também significa que o `datapath` inicial é configurado automaticamente para o diretório de trabalho corrente.**

Este repositório contém apenas o código fonte da API customizada.

A partir de 2019-03-05, este repositório contém todo o código fonte em linguagem Pascal necessário para compilar a DSS C-API. O código da API é mantido na pasta `src/`, enquanto o código principal do OpenDSS (com modificações) é mantido em `Version7/` e `Version8/`. O código fonte da versão oficial é mantido no branch `opendss-official-svn` e é integrado periodicamente -- veja o documento [upstream branch](https://github.com/dss-extensions/dss_capi/blob/master/docs/upstream_branch.md) (em inglês) para mais informações.

## Mudanças recentes

Veja o [registro de alterações (em inglês)](https://github.com/dss-extensions/dss_capi/blob/master/docs/changelog.md) para listagem detalhada.

- **2020-07-31 / version 0.10.6: Novas extensões para a API e alterações do OpenDSS oficial portadas. Inclui algumas correções de bugs, um novo mecanismo de mensagens de erro de validação, além de novos flags de compatibilidade.**
- 2020-03-03 / version 0.10.5: Principalmente manutenção, com correção de algumas falhas. Inclui alterações portadas da versão COM e do código do OpenDSS oficial. Binários da variação da versão 8 excluidos desta versão.
- 2019-11-16 / version 0.10.4: Apenas manutenção: Corrige acesso a arquivos com caminho longo no Linux, e inclui alterações portadas da versão COM e do código do OpenDSS oficial.
- 2019-05-22 / version 0.10.3: Algumas correções importantes, desempenho geral bastante melhorado, novas funções estendidas, e novas funcionalidades portadas do módulo COM e da versão atual do OpenDSS.
- 2019-03-05: o repositório Git `electricdss-src` foi integrado diretamente em `dss_capi`.
- 2019-02-28 / version 0.10.2: Implementa a função `CtrlQueue_Push` (faltante na versão anterior); modificações em `LoadShapes` para melhor desempenho e mais validação; introduz `DSS_Get_AllowEditor`/`DSS_Set_AllowEditor` para (desa)ativar chamadas ao editor externo.
- 2019-02-12 / version 0.10.1: Verificação de erros mais ampla, introdução da função `Error_Get_NumberPtr`, correções e melhor tratamento em `Meters`.
- 2018-11-17 / versão 0.10.0: Reduz o número de operações de alocação de memória se os buffers atuais forem reutilizados, introduz o mecanismo de Resultado Global, várias extensões de API (`LineGeometry`, `WireData`, `LineSpacing`, `CNData`, `TSData`, `Reactor`) -- veja [o documento de uso](https://github.com/dss-extensions/dss_capi/blob/master/docs/usage.md)(em inglês) e o [ticket #11](https://github.com/dss-extensions/dss_capi/issues/11).
- 2018-08-10 / versão 0.9.8: Grande reorganização do código fonte, várias pequenas correções, e novos scripts de compilação.
- 2018-04-05 / versão 0.9.5: Novas funções `Circuit_SetCktElement*` para definir o elemento de circuito ativo.
- 2018-03-06 / versão 0.9.4: Correções para DSSProperty, inclui os textos de ajuda originais no header C, integra modificações oficiais até a revisão 2152. Esta versão introduz a primeira versão de bindings de .NET para o DLL nativo.
- 2018-02-16 / versão 0.9.3: Integra correções da interface COM na revisão SVN 2136 (iteração via `First` `Next`)
- 2018-02-12 / versão 0.9.2: Suporte experimental para OpenDSS-PM and correções da interface COM integradas (OpenDSS revisão 2134)
- 2018-02-08 / versão 0.9.1: Primeira versão pública (OpenDSS revisão 2123)

## Funcionalidades faltantes e limitações

- Ainda não implementados:
    - `DSSEvents` de `DLL/ImplEvents.pas`: parece ser muito dependente de COM.
    - Gráficos em geral
    
## Funcionalides extras

Além da grande maioria dos métodos da interface COM, alguns dos métodos únicos da DDLL oficial (como acesso a ponteiros internos) foram expostos em formas adaptadas. Entre eles estão métodos de `DYMatrix.pas`, em especial `GetCompressedYMatrix` (veja os headers ou código fonte em Pascal para maiores detalhes).

Veja também (em inglês) o documento de diferenças conhecidas, [list of known differences](https://github.com/dss-extensions/dss_capi/blob/master/docs/known_differences.md), para métodos e opções extras não disponíveis no OpenDSS oficial.

## Download

[Siga aqui](https://github.com/dss-extensions/dss_capi/releases). Disponibilizados downloads pré-compilados para Windows, Linux e MacOS. 

## Como compilar?

Caso deseje compilar o DLL:

- Instale o compilador [Free Pascal](https://freepascal.org/). Caso já tenha instalado a IDE Lazarus, você já deve ter o compilador instalado. Adicione a pasta contendo o compilador (`fpc.exe`) para sua variável de ambiente PATH.

- Baixe (e compile) o código, ou baixe os binários já compilados, do projeto DSS-Extensions KLUSolve de https://github.com/dss-extensions/klusolve

- Clone este repositório:
```    
    git clone https://github.com/dss-extensions/dss_capi
```

- Sobreponha uma cópia do diretório `lib/` da KLUSolve sobre a pasta  `lib/` da `dss_capi`.


### No Windows

Se você precisa apenas do arquivo DLL, lembre-se que ele pode ser baixado na página de "Releases" no GitHub. Pode ser necessário instalar o pacote de [runtime do Microsoft Visual Studio 2017](https://go.microsoft.com/fwlink/?LinkId=746572).
Caso precisa compilar:

- Caso pretenda utilizar a DLL no Visual Studio, você precisa gerar uma biblioteca de importação. Isto pode ser feito iniciando a próxima etapa em um prompt do Visual Studio, como o "x64 Native Tools Command Prompt for VS 2017" (ou equivalente para sua versão) -- você precisa apenas dos utilitários  `dumpbin.exe` e `lib.exe`.

- Abra um prompt de comando na pasta `dss_capi` que você clonou anteriormente e execute `build_win_x64.bat`

Os arquivos de saída do processo são depositados na subpasta `lib/win_x64`. 

Caso precise apenas dos DLLs para versões ainda não lançadas, você pode encontrar os DLLs e LIBs para x64 nos [artefatos da instância do AppVeyor](https://ci.appveyor.com/project/dss-extensions/dss-capi/branch/master/artifacts). Estes arquivos são criados automaticamente a cada commit neste repositório e são mantidos por 6 meses.

### No Linux

- Compile o projeto principal:
```
    bash build_linux_x64.sh
```

### No MacOS

- Compile o projeto principal:
```
    bash build_macos_x64.sh
```

## Como usar e exemplos

Para entender os principais conceitos da DSS C-API e como ela gerencia memória, veja [o documento de uso](https://github.com/dss-extensions/dss_capi/blob/master/docs/usage.md)(em inglês).

Dois exemplos mínimos (sem scripts DSS, use um dos seus) estão disponíveis em [examples](examples). Após compilar a DSS C-API, adicione aos parâmetros de compilação a subpasta da pasta `include` apropriada (`include/v7` ou `include/v8`), e a biblioteca da pasta `lib` para seu sistema. O código dos exemplos, com seus comentários traduzidos para português, também estão disponíveis abaixo.

O código fonte da DSS Python e da OpenDSSDirect.py são exemplos de uso mais completos e avançados.


```c
#include <stdint.h>
#include <stdio.h>
#include "dss_capi.h"

int main(void)
{
    // Para numVoltages, o primeiro `int` representa o número atual de doubles, 
    // enquant o segundo `int` representa a capacidade alocada.
    int numVoltages[2] = {0, 0}; 
    double *voltages = NULL;
    int numNodes;
    int i;

    DSS_Start(0);
    Text_Set_Command("compile master.dss");
    Solution_Solve();
    Circuit_Get_AllBusVolts(&voltages, numVoltages);

    if (numVoltages[0] == 0)
    {
        return -1;
    }
    numNodes = numVoltages[0] / 2;
    
    for (i = 0; i < numNodes; ++i)
    {
        printf("node %d: %f + j%f\n", i, voltages[2*i], voltages[2*(i + 1)]);
    }

    // Before v0.10.0, if you needed to recall Circuit_Get_AllBusVolts,
    // you would need to call
    
    // DSS_Dispose_PDouble(&voltages);

    // Desde a versão 0.10.0, mesmo se mudanças ocorreram no circuito, 
    // ainda podemos reutilizar o ponteiro anterior.

    // Se a memória alocada previamente for suficiente para o número 
    // de doubles retornado por AllBusVolts, não será necessário
    // realocar memória!
    
    Solution_Solve();
    Circuit_Get_AllBusVolts(&voltages, numVoltages);

    // Como a memória para voltages é alocada em Pascal, precisamos
    // liberá-la em Pascal, logo a chamada para DSS_Dispose_PDouble
    DSS_Dispose_PDouble(&voltages);
    
    return 0;
}

```

Para usar a nova API empregando resultados globais (GR), os pointeiros para os GR precisam ser inicializados. 

```c
#include <stdint.h>
#include <stdio.h>
#include "dss_capi.h"

int main(void)
{
    char*** data_PPAnsiChar;
    double** data_PDouble;
    int32_t** data_PInteger;
    int8_t** data_PByte;
    int32_t* count_PPAnsiChar;
    int32_t* count_PDouble;
    int32_t* count_PInteger;
    int32_t* count_PByte;
    
    double* voltages;
    int numNodes;
    int i;
    
    DSS_Start(0);
    DSS_GetGRPointers(
        &data_PPAnsiChar,
        &data_PDouble,
        &data_PInteger,
        &data_PByte,
        &count_PPAnsiChar,
        &count_PDouble,
        &count_PInteger,
        &count_PByte
    );
    
    Text_Set_Command("compile master.dss");
    Solution_Solve();
    Circuit_Get_AllBusVolts_GR();
    
    // O resultado da chamada para Circuit_Get_AllBusVolts agora
    // está disponível em dataPtr_PDouble[0] e countPtr_PDouble
    
    // Aqui copiamos apenas o pointeiro para conveniência,
    // a memória ainda é de responsabilidade do mecanismo GR em 
    // Pascal
    voltages = dataPtr_PDouble[0];
    
    numNodes = count_PDouble[0]/2;
    if (numNodes == 0)
    {
        return -1;
    }
    for (i = 0; i < numVoltages; ++i)
    {
        printf("node %d: %f + j%f\n", i, voltages[2*i], voltages[2*(i + 1)]);
    }
    
    return 0;
}

```


## Testes

Atualmente, todos os testes e validação são baseados no [DSS Python](http://github.com/dss-extensions/dss_python/). Outros projetos como [OpenDSSDirect.py](http://github.com/dss-extensions/OpenDSSDirect.py/) e [OpenDSSDirect.jl](http://github.com/dss-extensions/OpenDSSDirect.jl/) contêm testes que foram importantes para encontrar e corrigir bugs.

## Planos

Além de correções de problemas, a funcionalidade principal desta biblioteca está pronta. Alguns pontos que pretendemos trabalhar envolvem:
- Expor os principais métodos e propriedades faltantes (não presentes nem mesmo na interface COM), assim como classes.
- Documentação melhor e mais completa. As strings de ajuda dos arquivos de definição IDL/COM já estão reproduzidos nos headers (pasta `include`), mas apenas em inglês.
- Validação automatizada dos binários no Linux (comparação das saídas com a versão Windows e oficial). Atualmente a validação é rodada manualmente.
- C++: Expor a API em C++ usando namespaces para organização, métodos com overload, etc.

Outras funções desejadas podem necessitar de mudanças invasivas na base de código provavelmente serão desenvolvidas inicialmente em um repositório a parte.

## Perguntas?

Caso tenha alguma pergunta, sinta à vontade para abrir um "issue" no GitHub or me contatar diretamente (pmeira arroba ieee.org).
Em geral, peço que me dê alguns dias para responder.


## Créditos

Este projeto é derivado do OpenDSS, desenvolvido pelo EPRI, e assim mantém a mesma base de licença. Veja `LICENSE` e `OPENDSS_LICENSE`, e também verifique cada subpasta para maiores detalhes.

Note que, já que o OpenDSS depende da biblioteca através da KLUSolve, as condições de licença da KLU (LGPL oou GPL, dependendo de como a KLU for compilada) se aplicam aos binários resultantes; veja os arquivos  `klusolve/COPYING`, `klusolve/lgpl_2_1.txt` e a documentação da SuiteSparse.

Agradecimentos aos colegas do Departamento de Sistemas e Energia, da Faculdade de Engenharia Elétrica e de Computação, na Universidade Estadual de Campinas (UNICAMP), pelos comentários e ajuda geral para testar este projeto.

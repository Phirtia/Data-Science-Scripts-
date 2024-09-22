# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 12:05:09 2024

@author: phirt
"""

#%% Instalação dos pacotes

!pip install pandas
!pip install numpy
!pip install -U seaborn
!pip install matplotlib
!pip install plotly
!pip install scipy
!pip install statsmodels
!pip install scikit-learn
!pip install statstests


#%% Importação dos pacotes

import pandas as pd # manipulação de dados em formato de dataframe
import numpy as np # operações matemáticas
import seaborn as sns # visualização gráfica
import matplotlib.pyplot as plt # visualização gráfica
from scipy.interpolate import UnivariateSpline # curva sigmoide suavizada
import statsmodels.api as sm # estimação de modelos
import statsmodels.formula.api as smf # estimação do modelo logístico binário
from statstests.process import stepwise # procedimento Stepwise
from scipy import stats # estatística chi2
from scipy.stats import chi2_contingency
import plotly.graph_objects as go # gráficos 3D
from statsmodels.iolib.summary2 import summary_col # comparação entre modelos
from statsmodels.discrete.discrete_model import MNLogit # estimação do modelo
                                                        #logístico multinomial
import warnings
warnings.filterwarnings('ignore')

#%% importando BD

df_cluster = pd.read_csv('BD_cluster_final.csv', delimiter=',')
df_cluster

df_cluster.info()

################################# MODELO LOGÍSTICO MULTINOMIAL PARA PREVER O CLUSTER DE CADA PESSOA ###################################

#%% Criando outro BD com variáveis de interesse sem os clusters

Variaveis_interesse_2 = [2,3,4,5,6,7,8,9,10,11,32]

df_modelo_supervisionado = df_cluster.iloc[:,Variaveis_interesse_2]

df_modelo_supervisionado['Grupo'].value_counts().sort_index()

#%% Relembrando grupos

tabela_cruzada = pd.crosstab(df_modelo_supervisionado['Raça/cor'], df_modelo_supervisionado['Grupo'], margins=True)
tabela_cruzada = pd.crosstab(df_modelo_supervisionado['NATURALIDADE_B'], df_modelo_supervisionado['Grupo'], margins=True)

#%% Criando faixa para a idade a partir dos resultados da clusterização 

df_modelo_supervisionado['Faixa_Etaria'] = df_modelo_supervisionado['IDADE'].apply(lambda x: 'Até 37 anos' if x <= 37 else 'Maior do que 37 anos')

#%% Dummies de variáveis x qualitativas 

df_modelo1_dummies = pd.get_dummies(df_modelo_supervisionado,
                                    columns=['NATURALIDADE_B',
                                    'Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B',
                                    'Faixa_Etaria'],
                                    dtype=int,
                                    drop_first=False)

df_modelo1_dummies.info()

#%% Renomeando variáveis para não dar programa de sintaxe

df_modelo1_dummies.rename(columns={
    'Estado_Civil_Casada(o)': 'Estado_Civil_Casada',
    'RENDA_B_Até um salário mínimo': 'RENDA_B_Até um salário mínimo',
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'Raça/cor_Branca': 'Raça_cor_Branca',
    'RELAÇÃO_TRABALHO_B_Empregada(o) com carteira': 'RELAÇÃO_TRABALHO_B_Empregada_com_carteira',  # Adicione a vírgula aqui
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (com registro em cartório': 'Estado_Civil_União_Estável_com_registro_em_cartório',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'Estado_Civil_Viúva(o)': 'Estado_Civil_Viúva',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'AUTOR_B_Cônjuge, companheiro, namorado': 'AUTOR_B_Cônjuge_companheiro_namorado',
    'RENDA_B_Até um salário mínimo': 'RENDA_B_Até_um_salário_mínimo',
    'Faixa_Etaria_Até 37 anos': 'Faixa_Etaria_Até_37_anos' 
}, inplace=True)

df_modelo1_dummies.info()
#%% Tratamento da variável y para uso na função MNLogit]

df_modelo1_dummies.loc[df_modelo1_dummies['Grupo']==
                           'Grupo 1',
                           'GRUPO_V2'] = 1

df_modelo1_dummies.loc[df_modelo1_dummies['Grupo']==
                           'Grupo 2',
                           'GRUPO_V2'] = 2

df_modelo1_dummies.loc[df_modelo1_dummies['Grupo']==
                           'Grupo 3',
                           'GRUPO_V2'] = 0

df_modelo1_dummies['GRUPO_V2'] = df_modelo1_dummies['GRUPO_V2'].astype('int64')
    
df_modelo1_dummies.info()

#%% Investigando multicolinearidade

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_g1 = pd.DataFrame(df_modelo1_dummies)

# Lista de variáveis categóricas binárias (as colunas do DataFrame)
categorical_columns = Correlacao_g1.columns

# Matriz de correlação de Cramér's V
corr_matrix = pd.DataFrame(np.zeros((len(categorical_columns), len(categorical_columns))),
                           index=categorical_columns, columns=categorical_columns)

# Preenchendo a matriz de correlação
for col1 in categorical_columns:
    for col2 in categorical_columns:
        corr_matrix.loc[col1, col2] = cramers_v(Correlacao_g1[col1], Correlacao_g1[col2])

# Gráfico de calor (heatmap) da matriz de correlação
fig = go.Figure()

fig.add_trace(
    go.Heatmap(
        x = corr_matrix.columns,
        y = corr_matrix.index,
        z = np.array(corr_matrix),
        text=corr_matrix.values,
        texttemplate='%{text:.2f}',
        colorscale='viridis',
        xgap=1,  # Espaçamento entre os rótulos do eixo x
        ygap=1   # Espaçamento entre os rótulos do eixo y
    )
)

# Ajustar layout
fig.update_layout(
    height = 1200,  # Altura maior para mais variáveis
    width = 1200,   # Largura maior para mais variáveis
    title='Heatmap de Correlação (Cramér\'s V) entre Variáveis Categóricas Binárias',
    xaxis=dict(
        tickangle=45,  # Rotaciona os rótulos do eixo x para torná-los legíveis
        automargin=True
    ),
    yaxis=dict(
        automargin=True
    ),
    margin=dict(
        l=100,  # Margens ajustadas para evitar que os rótulos sejam cortados
        r=100,
        b=150,
        t=150
    )
)

# Exibir no navegador ou no ambiente específico (como Jupyter)
fig.show(renderer='browser')

#%% Estimação do MODELO

x = df_modelo1_dummies.drop(columns=['REGRESSAO_MULTINOMIAL','IDADE', 'GRUPO_V2',
                                     'ESCOLARIDADE_B_Média',
                                     'RENDA_B_Sem_renda',
                                     'RELAÇÃO_TRABALHO_B_Regimes_informais',
                                     'Estado_Civil_Casada',
                                     'NATURALIDADE_B_São_Paulo',
                                     'NATURALIDADE_B_Outros',
                                     'Grupo',
                                     'Raça_cor_Branca',
                                     'AUTOR_B_Cônjuge_companheiro_namorado',
                                     'Faixa_Etaria_Maior do que 37 anos',
                                     'Estado_Civil_Viúva',
                                     'Filhos_Menores_Não',
                                     'ESCOLARIDADE_B_Baixa',
                                     'RENDA_B_Mais_de_2_salários_mínimos',
                                     'RENDA_B_De_1_a_2_salários_mínimos',
                                     'Estado_Civil_União_Estável_com_registro_em_cartório',
                                     'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
                                     'RELAÇÃO_TRABALHO_B_Outros',
                                     'RENDA_B_Até_um_salário_mínimo',
                                     'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
                                     'AUTOR_B_Outros',
                                     'IDADE'])
y = df_modelo1_dummies['GRUPO_V2']

X = sm.add_constant(x)

modelo_grupos = MNLogit(endog=y, exog=X).fit()

modelo_grupos.summary()

#%% função 'Qui2' para se extrair a estatística geral do modelo

def Qui2(modelo_grupos):
    maximo = modelo_grupos.llf
    minimo = modelo_grupos.llnull
    qui2 = -2*(minimo - maximo)
    pvalue = stats.distributions.chi2.sf(qui2,1)
    df = pd.DataFrame({'Qui quadrado':[qui2],
                       'pvalue':[pvalue]})
    return df
# Estatística geral do 'modelo_atrasado'

Qui2(modelo_grupos)

#%% PREDIÇÃO GRUPOS

# Probabilidades de ocorrência das três categoriais
# Definição do array 'phats':
phats = modelo_grupos.predict()
phats

# Transformação do array 'phats' para o dataframe 'phats':
phats = pd.DataFrame(phats)
phats

# Concatenando o dataframe original com o dataframe 'phats':
df_modelo1_dummies = pd.concat([df_modelo1_dummies, phats], axis=1)
df_modelo1_dummies

# Analisando o resultado de acordo com a categoria de resposta:
predicao = phats.idxmax(axis=1)
predicao

# Adicionando a categoria de resposta 'predicao' ao dataframe original,
#por meio da criação da variável 'predicao'
df_modelo1_dummies['predicao'] = predicao
df_modelo1_dummies

# Criando a variável 'predicao_label' a partir da variável 'predicao'

df_modelo1_dummies.loc[df_modelo1_dummies['predicao']==0,
                            'predicao_label'] ='Grupo 3'
df_modelo1_dummies.loc[df_modelo1_dummies['predicao']==1,
                            'predicao_label'] ='Grupo 1'
df_modelo1_dummies.loc[df_modelo1_dummies['predicao']==2,
                            'predicao_label'] ='Grupo 2'

df_modelo1_dummies

#%% Criação de tabela para cálculo da eficiência global do modelo

# Criando uma tabela para comparar as ocorrências reais com as predições
table = pd.pivot_table(df_modelo1_dummies,
                       index=['predicao_label'],
                       columns=['Grupo'],
                       aggfunc='size')
table

# Substituindo 'NaN' por zero
table = table.fillna(0)
table


#%% Eficiência global do modelo propriamente dita

# Transformando o dataframe 'table' para 'array', para que seja possível
#estabelecer o atributo 'diagonal'
table = table.to_numpy()
table

# Eficiência global do modelo
acuracia = table.diagonal().sum()/table.sum()
acuracia


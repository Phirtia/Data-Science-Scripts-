# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 11:32:41 2024

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


#%% Criando outro BD com variáveis de interesse sem os clusters

Variaveis_interesse = [2,3,4,5,6,7,8,9,10,32]

df_cluster2 = df_cluster.iloc[:,Variaveis_interesse]

df_cluster2.info()

#%% TODOS OS GRUPOS

get_dummies_binario = pd.get_dummies(df_cluster2,
                                     columns=['NATURALIDADE_B',
                                     'Raça/cor',
                                     'ESCOLARIDADE_B',
                                     'RELAÇÃO_TRABALHO_B',
                                     'Estado_Civil',
                                     'Filhos_Menores',
                                     'RENDA_B',
                                     'AUTOR_B', 
                                     'REGRESSAO_MULTINOMIAL'],
                                     dtype=int,
                                     drop_first=True)

get_dummies_binario.info()

#%% Renomeando variáveis para não dar programa de sintaxe

get_dummies_binario.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
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
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo'
}, inplace=True)


get_dummies_binario.info()

#%% Verificando Multicolinearidade Geral

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_geral = pd.DataFrame(get_dummies_binario)

# Lista de variáveis categóricas binárias (as colunas do DataFrame)
categorical_columns = Correlacao_geral.columns

# Matriz de correlação de Cramér's V
corr_matrix = pd.DataFrame(np.zeros((len(categorical_columns), len(categorical_columns))),
                           index=categorical_columns, columns=categorical_columns)

# Preenchendo a matriz de correlação
for col1 in categorical_columns:
    for col2 in categorical_columns:
        corr_matrix.loc[col1, col2] = cramers_v(Correlacao_geral[col1], Correlacao_geral[col2])

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

#%% Modelo binário geral

lista_colunas_geral = list(get_dummies_binario.drop(columns=['REGRESSAO_MULTINOMIAL_Retornou',
                                                             'REGRESSAO_MULTINOMIAL_Reativou',
                                                             'AUTOR_B_Outros',
                                                             'RENDA_B_De_1_a_2_salários_mínimos',
                                                             'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
                                                         'ESCOLARIDADE_B_Baixa',
                                                         'ESCOLARIDADE_B_Média',
                                                         'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
                                                         'RELAÇÃO_TRABALHO_B_Regimes_informais',
                                                         'Estado_Civil_União_Estável_sem_registro_no_cartório',
                                                         'ESCOLARIDADE_B_Média',
                                                         'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
                                                         'NATURALIDADE_B_Outros']).columns)
formula_dummies_modelo = ' + '.join(lista_colunas_geral)
formula_dummies_modelo = "REGRESSAO_MULTINOMIAL_Reativou ~ " + formula_dummies_modelo
print("Fórmula utilizada: ",formula_dummies_modelo)

modelo_binario = sm.Logit.from_formula(formula_dummies_modelo,
                                               get_dummies_binario).fit()

modelo_binario.summary()

#%% stepwise

from statstests.process import stepwise

step_modelo_binario = stepwise(modelo_binario, pvalue_limit=0.11)


########################################### MODELO BINÁRIO POR GRUPO #####################################

############ GRUPO 1 ###############################

#%% Reelaborando modelo por cluster 

Variaveis_interesse = [2,3,4,5,6,7,8,9,10,11,32]

df_cluster3 = df_cluster.iloc[:,Variaveis_interesse]

df_cluster3['Grupo'].value_counts().sort_index()

#%% Datasets por cluster

df_grupo1 = df_cluster3[df_cluster3['Grupo'] == 'Grupo 1']
df_grupo2 = df_cluster3[df_cluster3['Grupo'] == 'Grupo 2']
df_grupo3 = df_cluster3[df_cluster3['Grupo'] == 'Grupo 3']


#%% Dummies para grupo 1 para modelo binário

grupo1_dummies_binario = pd.get_dummies(df_grupo1,
                                    columns=['NATURALIDADE_B',
                                        'Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B', 
                                    'REGRESSAO_MULTINOMIAL'],
                                    dtype=int,
                                    drop_first=True)

grupo1_dummies_binario.info()


#%% Renomeando variáveis para não dar programa de sintaxe

grupo1_dummies_binario.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (com registro em cartório': 'Estado_Civil_União_Estável_com_registro_em_cartório',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo'
}, inplace=True)


grupo1_dummies_binario.info()

#%% Verificando Multicolinearidade GRUPO 1

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_g1 = pd.DataFrame(grupo1_dummies_binario)

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


#%% Modelo binário grupo 1

lista_colunas1 = list(grupo1_dummies_binario.drop(columns=['REGRESSAO_MULTINOMIAL_Retornou',
                                                           'Grupo',
                                                           'IDADE',
                                                         'ESCOLARIDADE_B_Média',
                                                         'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
                                                         'RELAÇÃO_TRABALHO_B_Regimes_informais',
                                                         'NATURALIDADE_B_Outros',
                                                         'RELAÇÃO_TRABALHO_B_Outros',
                                                         'AUTOR_B_Outros',
                                                         'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
                                                         'RENDA_B_De_1_a_2_salários_mínimos',
                                                         'Estado_Civil_União_Estável_sem_registro_no_cartório',
                                                         'Estado_Civil_Divorciada',
                                                         'ESCOLARIDADE_B_Média',
                                                         'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
                                                         'REGRESSAO_MULTINOMIAL_Reativou']).columns)
formula_dummies_modelo1 = ' + '.join(lista_colunas1)
formula_dummies_modelo1 = "REGRESSAO_MULTINOMIAL_Reativou ~ " + formula_dummies_modelo1
print("Fórmula utilizada: ",formula_dummies_modelo1)

modelo_binario1 = sm.Logit.from_formula(formula_dummies_modelo1,
                                               grupo1_dummies_binario).fit()

modelo_binario1.summary()

#%% stepwise

from statstests.process import stepwise

step_modelo_binario1 = stepwise(modelo_binario1, pvalue_limit=0.11)

#%% Construção de função para a definição da matriz de confusão

from sklearn.metrics import confusion_matrix, accuracy_score,\
    ConfusionMatrixDisplay, recall_score

def matriz_confusao(predicts, observado, cutoff):
    
    values = predicts.values
    
    predicao_binaria = []
        
    for item in values:
        if item < cutoff:
            predicao_binaria.append(0)
        else:
            predicao_binaria.append(1)
           
    cm = confusion_matrix(predicao_binaria, observado)
    disp = ConfusionMatrixDisplay(confusion_matrix=cm)
    disp.plot()
    plt.xlabel('True')
    plt.ylabel('Classified')
    plt.gca().invert_xaxis()
    plt.gca().invert_yaxis()
    plt.show()
        
    sensitividade = recall_score(observado, predicao_binaria, pos_label=1)
    especificidade = recall_score(observado, predicao_binaria, pos_label=0)
    acuracia = accuracy_score(observado, predicao_binaria)

    #Visualizando os principais indicadores desta matriz de confusão
    indicadores = pd.DataFrame({'Sensitividade':[sensitividade],
                                'Especificidade':[especificidade],
                                'Acurácia':[acuracia]})
    return indicadores

#%% Predição e Construção da matriz de confusão 
grupo1_dummies_binario['phat'] = step_modelo_binario1.predict()

# Matriz de confusão para cutoff = 0.4
matriz_confusao(observado=grupo1_dummies_binario['REGRESSAO_MULTINOMIAL_Reativou'],
                predicts=grupo1_dummies_binario['phat'],
                cutoff=0.4)

cutoff = 0.4
grupo1_dummies_binario['Sim'] = (grupo1_dummies_binario['phat'] >= cutoff).astype(int)


#%% Indices de Sensitividade, especificidade e acurácia
matriz_confusao(observado=grupo1_dummies_binario['REGRESSAO_MULTINOMIAL_Reativou'],
                predicts=grupo1_dummies_binario['phat'], 
                cutoff=0.4)

#%% Eficiência do modelo com a Curva ROC
from sklearn.metrics import roc_curve, auc


fpr, tpr, thresholds =roc_curve(grupo1_dummies_binario['REGRESSAO_MULTINOMIAL_Reativou'],
                                grupo1_dummies_binario['phat'])
roc_auc = auc(fpr, tpr)

# Cálculo do coeficiente de GINI
gini = (roc_auc - 0.4)/(0.4)

# Plotando a curva ROC
plt.figure(figsize=(15,10))
plt.plot(fpr, tpr, marker='o', color='darkorchid', markersize=10, linewidth=3)
plt.plot(fpr, fpr, color='gray', linestyle='dashed')
plt.title('Área abaixo da curva: %g' % round(roc_auc, 4) +
          ' | Coeficiente de GINI: %g' % round(gini, 4), fontsize=22)
plt.xlabel('1 - Especificidade', fontsize=20)
plt.ylabel('Sensitividade', fontsize=20)
plt.xticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.yticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.show()


######################### GRUPO 2 ##########################################################

#%% Dummies para grupo 2 para modelo binário

grupo2_dummies_binario = pd.get_dummies(df_grupo2,
                                    columns=['Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B', 
                                    'REGRESSAO_MULTINOMIAL'],
                                    dtype=int,
                                    drop_first=True)

grupo2_dummies_binario.info()

#%% Inicio de separação de variável Nordeste

grupo2_dummies_binario = pd.get_dummies(grupo2_dummies_binario,
                                    columns=['NATURALIDADE_B'],
                                    dtype=int,
                                    drop_first=False)

#%% Renomeando variáveis para não dar programa de sintaxe

grupo2_dummies_binario.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'Estado_Civil_Viúva(o)': 'Estado_Civil_Viuva',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo'
}, inplace=True)

grupo2_dummies_binario.info()

#%% Verificando Multicolinearidade gRUPO 2

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_g2 = pd.DataFrame(grupo2_dummies_binario)

# Lista de variáveis categóricas binárias (as colunas do DataFrame)
categorical_columns = Correlacao_g2.columns

# Matriz de correlação de Cramér's V
corr_matrix = pd.DataFrame(np.zeros((len(categorical_columns), len(categorical_columns))),
                           index=categorical_columns, columns=categorical_columns)

# Preenchendo a matriz de correlação
for col1 in categorical_columns:
    for col2 in categorical_columns:
        corr_matrix.loc[col1, col2] = cramers_v(Correlacao_g2[col1], Correlacao_g2[col2])

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


#%% Modelo binário grupo 2

##Teste de multicolineraridade indicou a remoção de 2 variáveis a mais: RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada e IDADE
## Além do problema de multicolinearidade, também houve problema com classes desbalanceadas: AUTOR_B_Outros, NATURALIDADE_B_Outros,
## RELAÇÃO_TRABALHO_B_Outros e Estado_Civil_Viuva

lista_colunas2 = list(grupo2_dummies_binario.drop(columns=['Grupo',
                                                           'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
                                                           'NATURALIDADE_B_São_Paulo',
                                                           'RENDA_B_De_1_a_2_salários_mínimos',
                                                           'RENDA_B_Mais_de_2_salários_mínimos',
                                                           'ESCOLARIDADE_B_Baixa',
                                                           'Estado_Civil_Divorciada',
                                                           'Estado_Civil_União_Estável_sem_registro_no_cartório',
                                                         'REGRESSAO_MULTINOMIAL_Retornou',
                                                         'REGRESSAO_MULTINOMIAL_Reativou',
                                                         'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
                                                         'IDADE',
                                                         'RELAÇÃO_TRABALHO_B_Outros',
                                                         'AUTOR_B_Outros',
                                                         'NATURALIDADE_B_Outros',
                                                         'Estado_Civil_Viuva']).columns)
formula_dummies_modelo2 = ' + '.join(lista_colunas2)
formula_dummies_modelo2 = "REGRESSAO_MULTINOMIAL_Reativou ~ " + formula_dummies_modelo2
print("Fórmula utilizada: ",formula_dummies_modelo2)

modelo_binario2 = sm.Logit.from_formula(formula_dummies_modelo2,
                                               grupo2_dummies_binario).fit()

modelo_binario2.summary()


## NENHUM CORRELAÇÃO

################################### GRUPO 3 #######################################

#%% Dummies para grupo 3 para modelo binário

grupo3_dummies_binario = pd.get_dummies(df_grupo3,
                                    columns=['NATURALIDADE_B',
                                    'Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B', 
                                    'REGRESSAO_MULTINOMIAL'],
                                    dtype=int,
                                    drop_first=True)

grupo3_dummies_binario.info()


#%% Renomeando variáveis para não dar programa de sintaxe

grupo3_dummies_binario.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (com registro em cartório': 'Estado_Civil_União_Estável_com_registro_em_cartório',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo'
}, inplace=True)


#%% Verificando Multicolinearidade gRUPO 3

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_g3 = pd.DataFrame(grupo3_dummies_binario)

# Lista de variáveis categóricas binárias (as colunas do DataFrame)
categorical_columns = Correlacao_g3.columns

# Matriz de correlação de Cramér's V
corr_matrix = pd.DataFrame(np.zeros((len(categorical_columns), len(categorical_columns))),
                           index=categorical_columns, columns=categorical_columns)

# Preenchendo a matriz de correlação
for col1 in categorical_columns:
    for col2 in categorical_columns:
        corr_matrix.loc[col1, col2] = cramers_v(Correlacao_g3[col1], Correlacao_g3[col2])

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


#%% Modelo binário grupo 3

# Remoção da varíavel IDADE devido a multicolinearidade
# Remoção das demais categorias por problema de desbalanceamento

lista_colunas3 = list(grupo3_dummies_binario.drop(columns=['Grupo',
                                                         'REGRESSAO_MULTINOMIAL_Retornou',
                                                         'REGRESSAO_MULTINOMIAL_Reativou',
                                                         'IDADE',
                                                         'Estado_Civil_União_Estável_com_registro_em_cartório',
                                                         'AUTOR_B_Familiar',
                                                         'RENDA_B_De_1_a_2_salários_mínimos',
                                                         'NATURALIDADE_B_São_Paulo',
                                                         'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
                                                         'NATURALIDADE_B_Outros',
                                                         'ESCOLARIDADE_B_Média',
                                                         'AUTOR_B_Outros']).columns)
formula_dummies_modelo3 = ' + '.join(lista_colunas3)
formula_dummies_modelo3 = "REGRESSAO_MULTINOMIAL_Reativou ~ " + formula_dummies_modelo3
print("Fórmula utilizada: ",formula_dummies_modelo3)

modelo_binario3 = sm.Logit.from_formula(formula_dummies_modelo3,
                                               grupo3_dummies_binario).fit()

modelo_binario3.summary()

#%% stepwise

from statstests.process import stepwise

step_modelo_binario3 = stepwise(modelo_binario3, pvalue_limit=0.11)


#%% Construção de função para a definição da matriz de confusão

from sklearn.metrics import confusion_matrix, accuracy_score,\
    ConfusionMatrixDisplay, recall_score

def matriz_confusao(predicts, observado, cutoff):
    
    values = predicts.values
    
    predicao_binaria = []
        
    for item in values:
        if item < cutoff:
            predicao_binaria.append(0)
        else:
            predicao_binaria.append(1)
           
    cm = confusion_matrix(predicao_binaria, observado)
    disp = ConfusionMatrixDisplay(confusion_matrix=cm)
    disp.plot()
    plt.xlabel('True')
    plt.ylabel('Classified')
    plt.gca().invert_xaxis()
    plt.gca().invert_yaxis()
    plt.show()
        
    sensitividade = recall_score(observado, predicao_binaria, pos_label=1)
    especificidade = recall_score(observado, predicao_binaria, pos_label=0)
    acuracia = accuracy_score(observado, predicao_binaria)

    #Visualizando os principais indicadores desta matriz de confusão
    indicadores = pd.DataFrame({'Sensitividade':[sensitividade],
                                'Especificidade':[especificidade],
                                'Acurácia':[acuracia]})
    return indicadores

#%% Construção da matriz de confusão 

grupo3_dummies_binario['phat'] = step_modelo_binario3.predict()

# Matriz de confusão para cutoff = 0.2 (utilizou-se de 0,2 para diminuir a sensitividade e aumentar os verdadeiros positivos que é o objetivo)
matriz_confusao(observado=grupo3_dummies_binario['REGRESSAO_MULTINOMIAL_Reativou'],
                predicts=grupo3_dummies_binario['phat'],
                cutoff=0.2)

cutoff = 0.2
grupo3_dummies_binario['Sim'] = (grupo3_dummies_binario['phat'] >= cutoff).astype(int)

#%% Predição

grupo3_dummies_binario['phat'] = modelo_binario3.predict()

#%% 
matriz_confusao(observado=grupo3_dummies_binario['REGRESSAO_MULTINOMIAL_Reativou'],
                predicts=grupo3_dummies_binario['phat'], 
                cutoff=0.2)

#%% Eficiência do modelo com a Curva ROC
from sklearn.metrics import roc_curve, auc


fpr, tpr, thresholds =roc_curve(grupo3_dummies_binario['REGRESSAO_MULTINOMIAL_Reativou'],
                                grupo3_dummies_binario['phat'])
roc_auc = auc(fpr, tpr)

# Cálculo do coeficiente de GINI
gini = (roc_auc - 0.2)/(0.2)

# Plotando a curva ROC
plt.figure(figsize=(15,10))
plt.plot(fpr, tpr, marker='o', color='darkorchid', markersize=10, linewidth=3)
plt.plot(fpr, fpr, color='gray', linestyle='dashed')
plt.title('Área abaixo da curva: %g' % round(roc_auc, 4) +
          ' | Coeficiente de GINI: %g' % round(gini, 4), fontsize=22)
plt.xlabel('1 - Especificidade', fontsize=20)
plt.ylabel('Sensitividade', fontsize=20)
plt.xticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.yticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.show()




####################################### MODELO LOGÍSTICO BINÁRIO PRIMEIRO ATENDIMENTO PA #################################################

######################### GRUPO 1 ##########################################################

#%% Dummies da variável y sem retirar categoria de referência, no caso, primeiro atendimento

grupo1_dummies_binario_PA = pd.get_dummies(df_grupo1,
                                    columns=['REGRESSAO_MULTINOMIAL'],
                                    dtype=int,
                                    drop_first=False)


#%% Dummiezaçaõ das demais variáveis

grupo1_dummies_binario_PA = pd.get_dummies(grupo1_dummies_binario_PA,
                                    columns=['NATURALIDADE_B',
                                    'Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B'],
                                    dtype=int,
                                    drop_first=True)


grupo1_dummies_binario_PA.info()

#%% Renomeando variáveis para não dar programa de sintaxe

grupo1_dummies_binario_PA.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (com registro em cartório': 'Estado_Civil_União_Estável_com_registro_em_cartório',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo'
}, inplace=True)



#%% Modelo binário grupo 1

lista_colunas1_PA = list(grupo1_dummies_binario_PA.drop(columns=['Grupo',
                                                           'REGRESSAO_MULTINOMIAL_Primeiro_Atendimento',
                                                         'REGRESSAO_MULTINOMIAL_Retornou',
                                                         'REGRESSAO_MULTINOMIAL_Reativou', 
                                                         'IDADE', 'NATURALIDADE_B_Outros', 'RELAÇÃO_TRABALHO_B_Outros',
                                                         'Estado_Civil_União_Estável_com_registro_em_cartório',
                                                         'AUTOR_B_Outros']).columns)

formula_dummies_modelo1_PA = ' + '.join(lista_colunas1_PA)
formula_dummies_modelo1_PA = "REGRESSAO_MULTINOMIAL_Primeiro_Atendimento ~ " + formula_dummies_modelo1_PA
print("Fórmula utilizada: ",formula_dummies_modelo1_PA)

modelo_binario1_PA = sm.Logit.from_formula(formula_dummies_modelo1_PA,
                                               grupo1_dummies_binario_PA).fit()

modelo_binario1_PA.summary()

#%% stepwise

from statstests.process import stepwise

step_modelo_binario1_PA = stepwise(modelo_binario1_PA, pvalue_limit=0.07)



#%%

######################### GRUPO 2 ##########################################################

#%% Dummies da variável y sem retirar categoria de referência, no caso, primeiro atendimento

grupo2_dummies_binario_PA = pd.get_dummies(df_grupo2,
                                    columns=['REGRESSAO_MULTINOMIAL'],
                                    dtype=int,
                                    drop_first=False)


#%% Dummiezaçaõ das demais variáveis

grupo2_dummies_binario_PA = pd.get_dummies(grupo2_dummies_binario_PA,
                                    columns=['NATURALIDADE_B',
                                    'Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B'],
                                    dtype=int,
                                    drop_first=True)


grupo2_dummies_binario_PA.info()

#%% Renomeando variáveis não dar a de sintaxe

grupo2_dummies_binario_PA.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'Estado_Civil_Viúva(o)': 'Estado_Civil_Viuva',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo'
}, inplace=True)


grupo2_dummies_binario_PA.info()


#%% Verificando Multicolinearidade gRUPO 2

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_g2_PA = pd.DataFrame(grupo2_dummies_binario_PA)

# Lista de variáveis categóricas binárias (as colunas do DataFrame)
categorical_columns = Correlacao_g2_PA.columns

# Matriz de correlação de Cramér's V
corr_matrix = pd.DataFrame(np.zeros((len(categorical_columns), len(categorical_columns))),
                           index=categorical_columns, columns=categorical_columns)

# Preenchendo a matriz de correlação
for col1 in categorical_columns:
    for col2 in categorical_columns:
        corr_matrix.loc[col1, col2] = cramers_v(Correlacao_g2_PA[col1], Correlacao_g2_PA[col2])

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


#%% Modelo binário grupo 2

lista_colunas2_PA = list(grupo2_dummies_binario_PA.drop(columns=['Grupo',
                                                         'REGRESSAO_MULTINOMIAL_Retornou',
                                                         'REGRESSAO_MULTINOMIAL_Reativou',
                                                         'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
                                                         'IDADE',
                                                         'AUTOR_B_Outros',
                                                         'NATURALIDADE_B_São_Paulo',
                                                         'Estado_Civil_Divorciada',
                                                         'Estado_Civil_União_Estável_sem_registro_no_cartório',
                                                         'ESCOLARIDADE_B_Baixa',
                                                         'RELAÇÃO_TRABALHO_B_Outros',
                                                         'Estado_Civil_Viuva',
                                                         'RENDA_B_Mais_de_2_salários_mínimos',
                                                         'NATURALIDADE_B_Outros',
                                                         'ESCOLARIDADE_B_Baixa']).columns)

formula_dummies_modelo2_PA = ' + '.join(lista_colunas2_PA)
formula_dummies_modelo2_PA = "REGRESSAO_MULTINOMIAL_Primeiro_Atendimento ~ " + formula_dummies_modelo2_PA
print("Fórmula utilizada: ",formula_dummies_modelo2_PA)

modelo_binario2_PA = sm.Logit.from_formula(formula_dummies_modelo2_PA,
                                               grupo2_dummies_binario_PA).fit()

modelo_binario2_PA.summary()




################################### GRUPO 3 #######################################

#%% Dummies da variável y sem retirar categoria de referência, no caso, primeiro atendimento

grupo3_dummies_binario_PA = pd.get_dummies(df_grupo3,
                                    columns=['REGRESSAO_MULTINOMIAL'],
                                    dtype=int,
                                    drop_first=False)


#%% Dummiezaçaõ das demais variáveis

grupo3_dummies_binario_PA = pd.get_dummies(grupo3_dummies_binario_PA,
                                    columns=['NATURALIDADE_B',
                                    'Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B'],
                                    dtype=int,
                                    drop_first=True)


grupo3_dummies_binario_PA.info()

#%% Renomeando variáveis não dar a de sintaxe

grupo3_dummies_binario_PA.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'Estado_Civil_União Estável (com registro em cartório': 'Estado_Civil_União_Estável_com_registro_no_cartório',
    'Estado_Civil_Viúva(o)': 'Estado_Civil_Viuva',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo'
}, inplace=True)

grupo3_dummies_binario_PA.info()

#%% Multicolinearidade

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_g2_PA = pd.DataFrame(grupo2_dummies_binario_PA)

# Lista de variáveis categóricas binárias (as colunas do DataFrame)
categorical_columns = Correlacao_g2_PA.columns

# Matriz de correlação de Cramér's V
corr_matrix = pd.DataFrame(np.zeros((len(categorical_columns), len(categorical_columns))),
                           index=categorical_columns, columns=categorical_columns)

# Preenchendo a matriz de correlação
for col1 in categorical_columns:
    for col2 in categorical_columns:
        corr_matrix.loc[col1, col2] = cramers_v(Correlacao_g2_PA[col1], Correlacao_g2_PA[col2])

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

#%% Modelo binário grupo 2

lista_colunas3_PA = list(grupo3_dummies_binario_PA.drop(columns=['Grupo',
                                                         'REGRESSAO_MULTINOMIAL_Retornou',
                                                         'REGRESSAO_MULTINOMIAL_Reativou',
                                                         'RENDA_B_Sem_renda',
                                                         'IDADE',
                                                         'AUTOR_B_Familiar',
                                                         'NATURALIDADE_B_Outros',
                                                         'RELAÇÃO_TRABALHO_B_Outros',
                                                         'NATURALIDADE_B_São_Paulo',
                                                         'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
                                                         'RENDA_B_De_1_a_2_salários_mínimos',
                                                         'RENDA_B_Mais_de_2_salários_mínimos',
                                                         'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado']).columns)

formula_dummies_modelo3_PA = ' + '.join(lista_colunas3_PA)
formula_dummies_modelo3_PA = "REGRESSAO_MULTINOMIAL_Primeiro_Atendimento ~ " + formula_dummies_modelo3_PA
print("Fórmula utilizada: ",formula_dummies_modelo3_PA)

modelo_binario3_PA = sm.Logit.from_formula(formula_dummies_modelo3_PA,
                                               grupo3_dummies_binario_PA).fit()

modelo_binario3_PA.summary()


#%% 

######################## MODELO BINÁRIO RETORNO ########################

############## GRUPO 1 ########################

#%% Dummies da variável y sem retirar categoria de referência, no caso, primeiro atendimento

grupo1_dummies_binario_RE = pd.get_dummies(df_grupo1,
                                    columns=['REGRESSAO_MULTINOMIAL'],
                                    dtype=int,
                                    drop_first=False)


#%% Dummiezaçaõ das demais variáveis

grupo1_dummies_binario_RE = pd.get_dummies(grupo1_dummies_binario_RE,
                                    columns=['NATURALIDADE_B',
                                    'Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B'],
                                    dtype=int,
                                    drop_first=True)


grupo1_dummies_binario_RE.info()

#%% Renomeando variáveis para não dar programa de sintaxe

grupo1_dummies_binario_RE.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (com registro em cartório': 'Estado_Civil_União_Estável_com_registro_em_cartório',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo'
}, inplace=True)

#%% Verificando Multicolinearidade gRUPO 1

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_geral = pd.DataFrame(grupo1_dummies_binario_RE)

# Lista de variáveis categóricas binárias (as colunas do DataFrame)
categorical_columns = Correlacao_geral.columns

# Matriz de correlação de Cramér's V
corr_matrix = pd.DataFrame(np.zeros((len(categorical_columns), len(categorical_columns))),
                           index=categorical_columns, columns=categorical_columns)

# Preenchendo a matriz de correlação
for col1 in categorical_columns:
    for col2 in categorical_columns:
        corr_matrix.loc[col1, col2] = cramers_v(Correlacao_geral[col1], Correlacao_geral[col2])

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

#%% Modelo binário grupo 1

lista_colunas1_RE = list(grupo1_dummies_binario_RE.drop(columns=['Grupo',
                                                           'REGRESSAO_MULTINOMIAL_Primeiro_Atendimento',
                                                         'REGRESSAO_MULTINOMIAL_Retornou',
                                                         'REGRESSAO_MULTINOMIAL_Reativou', 
                                                         'IDADE', 
                                                         'NATURALIDADE_B_Outros', 
                                                         'NATURALIDADE_B_São_Paulo',
                                                         'ESCOLARIDADE_B_Média',
                                                         'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
                                                         'RELAÇÃO_TRABALHO_B_Regimes_informais',
                                                         'Estado_Civil_Solteira',
                                                         'RELAÇÃO_TRABALHO_B_Outros',
                                                         'Estado_Civil_União_Estável_com_registro_em_cartório',
                                                         'AUTOR_B_Outros',
                                                         'Filhos_Menores_Sim']).columns)

formula_dummies_modelo1_RE = ' + '.join(lista_colunas1_RE)
formula_dummies_modelo1_RE = "REGRESSAO_MULTINOMIAL_Retornou ~ " + formula_dummies_modelo1_RE
print("Fórmula utilizada: ",formula_dummies_modelo1_RE)

modelo_binario1_RE = sm.Logit.from_formula(formula_dummies_modelo1_RE,
                                               grupo1_dummies_binario_RE).fit()

modelo_binario1_RE.summary()

#%% stepwise

from statstests.process import stepwise

step_modelo_binario1_RE = stepwise(modelo_binario1_RE, pvalue_limit=0.11)



#%% Construção de função para a definição da matriz de confusão

from sklearn.metrics import confusion_matrix, accuracy_score,\
    ConfusionMatrixDisplay, recall_score

def matriz_confusao(predicts, observado, cutoff):
    
    values = predicts.values
    
    predicao_binaria = []
        
    for item in values:
        if item < cutoff:
            predicao_binaria.append(0)
        else:
            predicao_binaria.append(1)
           
    cm = confusion_matrix(predicao_binaria, observado)
    disp = ConfusionMatrixDisplay(confusion_matrix=cm)
    disp.plot()
    plt.xlabel('True')
    plt.ylabel('Classified')
    plt.gca().invert_xaxis()
    plt.gca().invert_yaxis()
    plt.show()
        
    sensitividade = recall_score(observado, predicao_binaria, pos_label=1)
    especificidade = recall_score(observado, predicao_binaria, pos_label=0)
    acuracia = accuracy_score(observado, predicao_binaria)

    #Visualizando os principais indicadores desta matriz de confusão
    indicadores = pd.DataFrame({'Sensitividade':[sensitividade],
                                'Especificidade':[especificidade],
                                'Acurácia':[acuracia]})
    return indicadores

#%% Construção da matriz de confusão 

grupo1_dummies_binario_RE['phat'] = step_modelo_binario1_RE.predict()

# Matriz de confusão para cutoff = 0.2 (utilizou-se de 0,2 para diminuir a sensitividade e aumentar os verdadeiros positivos que é o objetivo)
matriz_confusao(observado=grupo1_dummies_binario_RE['REGRESSAO_MULTINOMIAL_Retornou'],
                predicts=grupo1_dummies_binario_RE['phat'],
                cutoff=0.2)

cutoff = 0.2
grupo1_dummies_binario_RE['Sim'] = (grupo1_dummies_binario_RE['phat'] >= cutoff).astype(int)

#%% Predição

grupo1_dummies_binario_RE['phat'] = modelo_binario1_RE.predict()

#%% 
matriz_confusao(observado=grupo1_dummies_binario_RE['REGRESSAO_MULTINOMIAL_Retornou'],
                predicts=grupo1_dummies_binario_RE['phat'], 
                cutoff=0.5)

#%% Eficiência do modelo com a Curva ROC
from sklearn.metrics import roc_curve, auc


fpr, tpr, thresholds =roc_curve(grupo1_dummies_binario_RE['REGRESSAO_MULTINOMIAL_Retornou'],
                                grupo1_dummies_binario_RE['phat'])
roc_auc = auc(fpr, tpr)

# Cálculo do coeficiente de GINI
gini = (roc_auc - 0.5)/(0.5)

# Plotando a curva ROC
plt.figure(figsize=(15,10))
plt.plot(fpr, tpr, marker='o', color='darkorchid', markersize=10, linewidth=3)
plt.plot(fpr, fpr, color='gray', linestyle='dashed')
plt.title('Área abaixo da curva: %g' % round(roc_auc, 4) +
          ' | Coeficiente de GINI: %g' % round(gini, 4), fontsize=22)
plt.xlabel('1 - Especificidade', fontsize=20)
plt.ylabel('Sensitividade', fontsize=20)
plt.xticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.yticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.show()


############## GRUPO 2 ########################

#%% Dummies da variável y sem retirar categoria de referência, no caso, primeiro atendimento

grupo2_dummies_binario_RE = pd.get_dummies(df_grupo2,
                                    columns=['REGRESSAO_MULTINOMIAL'],
                                    dtype=int,
                                    drop_first=False)


#%% Dummiezaçaõ das demais variáveis

grupo2_dummies_binario_RE = pd.get_dummies(grupo2_dummies_binario_RE,
                                    columns=['NATURALIDADE_B',
                                    'Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B'],
                                    dtype=int,
                                    drop_first=True)


grupo2_dummies_binario_RE.info()

#%% Renomeando variáveis para não dar programa de sintaxe

grupo2_dummies_binario_RE.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (com registro em cartório': 'Estado_Civil_União_Estável_com_registro_em_cartório',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'Estado_Civil_Viúva(o)': 'Estado_Civil_Viúva',
}, inplace=True)

grupo2_dummies_binario_RE.info()

#%% Verificando Multicolinearidade gRUPO 2

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_geral = pd.DataFrame(grupo2_dummies_binario_RE)

# Lista de variáveis categóricas binárias (as colunas do DataFrame)
categorical_columns = Correlacao_geral.columns

# Matriz de correlação de Cramér's V
corr_matrix = pd.DataFrame(np.zeros((len(categorical_columns), len(categorical_columns))),
                           index=categorical_columns, columns=categorical_columns)

# Preenchendo a matriz de correlação
for col1 in categorical_columns:
    for col2 in categorical_columns:
        corr_matrix.loc[col1, col2] = cramers_v(Correlacao_geral[col1], Correlacao_geral[col2])

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

#%% Modelo binário grupo 2

lista_colunas2_RE = list(grupo2_dummies_binario_RE.drop(columns=['Grupo',
                                                           'REGRESSAO_MULTINOMIAL_Primeiro_Atendimento',
                                                         'REGRESSAO_MULTINOMIAL_Retornou',
                                                         'REGRESSAO_MULTINOMIAL_Reativou', 
                                                         'IDADE', 
                                                         'NATURALIDADE_B_Outros', 
                                                         'ESCOLARIDADE_B_Baixa',
                                                         'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
                                                         'RELAÇÃO_TRABALHO_B_Regimes_informais',
                                                         'RELAÇÃO_TRABALHO_B_Outros',
                                                         'Estado_Civil_Viúva']).columns)

formula_dummies_modelo2_RE = ' + '.join(lista_colunas2_RE)
formula_dummies_modelo2_RE = "REGRESSAO_MULTINOMIAL_Retornou ~ " + formula_dummies_modelo2_RE
print("Fórmula utilizada: ",formula_dummies_modelo2_RE)

modelo_binario2_RE = sm.Logit.from_formula(formula_dummies_modelo2_RE,
                                               grupo2_dummies_binario_RE).fit()

modelo_binario2_RE.summary()

#%% stepwise

from statstests.process import stepwise

step_modelo_binario2_RE = stepwise(modelo_binario2_RE, pvalue_limit=0.11)

#%% Construção de função para a definição da matriz de confusão

from sklearn.metrics import confusion_matrix, accuracy_score,\
    ConfusionMatrixDisplay, recall_score

def matriz_confusao(predicts, observado, cutoff):
    
    values = predicts.values
    
    predicao_binaria = []
        
    for item in values:
        if item < cutoff:
            predicao_binaria.append(0)
        else:
            predicao_binaria.append(1)
           
    cm = confusion_matrix(predicao_binaria, observado)
    disp = ConfusionMatrixDisplay(confusion_matrix=cm)
    disp.plot()
    plt.xlabel('True')
    plt.ylabel('Classified')
    plt.gca().invert_xaxis()
    plt.gca().invert_yaxis()
    plt.show()
        
    sensitividade = recall_score(observado, predicao_binaria, pos_label=1)
    especificidade = recall_score(observado, predicao_binaria, pos_label=0)
    acuracia = accuracy_score(observado, predicao_binaria)

    #Visualizando os principais indicadores desta matriz de confusão
    indicadores = pd.DataFrame({'Sensitividade':[sensitividade],
                                'Especificidade':[especificidade],
                                'Acurácia':[acuracia]})
    return indicadores

#%% Construção da matriz de confusão 

grupo2_dummies_binario_RE['phat'] = step_modelo_binario2_RE.predict()

# Matriz de confusão para cutoff = 0.2 (utilizou-se de 0,2 para diminuir a sensitividade e aumentar os verdadeiros positivos que é o objetivo)
matriz_confusao(observado=grupo2_dummies_binario_RE['REGRESSAO_MULTINOMIAL_Retornou'],
                predicts=grupo2_dummies_binario_RE['phat'],
                cutoff=0.5)

cutoff = 0.5
grupo2_dummies_binario_RE['Sim'] = (grupo2_dummies_binario_RE['phat'] >= cutoff).astype(int)


#%% Predição

grupo2_dummies_binario_RE['phat'] = modelo_binario2_RE.predict()

#%% 
matriz_confusao(observado=grupo2_dummies_binario_RE['REGRESSAO_MULTINOMIAL_Retornou'],
                predicts=grupo2_dummies_binario_RE['phat'], 
                cutoff=0.5)


#%% Eficiência do modelo com a Curva ROC
from sklearn.metrics import roc_curve, auc


fpr, tpr, thresholds =roc_curve(grupo2_dummies_binario_RE['REGRESSAO_MULTINOMIAL_Retornou'],
                                grupo2_dummies_binario_RE['phat'])
roc_auc = auc(fpr, tpr)

# Cálculo do coeficiente de GINI
gini = (roc_auc - 0.5)/(0.5)

# Plotando a curva ROC
plt.figure(figsize=(15,10))
plt.plot(fpr, tpr, marker='o', color='darkorchid', markersize=10, linewidth=3)
plt.plot(fpr, fpr, color='gray', linestyle='dashed')
plt.title('Área abaixo da curva: %g' % round(roc_auc, 4) +
          ' | Coeficiente de GINI: %g' % round(gini, 4), fontsize=22)
plt.xlabel('1 - Especificidade', fontsize=20)
plt.ylabel('Sensitividade', fontsize=20)
plt.xticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.yticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.show()




############## GRUPO 3 ########################

#%% Dummies da variável y sem retirar categoria de referência, no caso, primeiro atendimento

grupo3_dummies_binario_RE = pd.get_dummies(df_grupo3,
                                    columns=['REGRESSAO_MULTINOMIAL'],
                                    dtype=int,
                                    drop_first=False)


#%% Dummiezaçaõ das demais variáveis

grupo3_dummies_binario_RE = pd.get_dummies(grupo3_dummies_binario_RE,
                                    columns=['NATURALIDADE_B',
                                    'Raça/cor',
                                    'ESCOLARIDADE_B',
                                    'RELAÇÃO_TRABALHO_B',
                                    'Estado_Civil',
                                    'Filhos_Menores',
                                    'RENDA_B',
                                    'AUTOR_B'],
                                    dtype=int,
                                    drop_first=True)


grupo3_dummies_binario_RE.info()

#%% Renomeando variáveis para não dar programa de sintaxe

grupo3_dummies_binario_RE.rename(columns={
    'NATURALIDADE_B_Sudeste, fora de São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'NATURALIDADE_B_São Paulo': 'NATURALIDADE_B_São_Paulo',
    'Raça/cor_Não Branca': 'Raça_cor_Não_Branca',
    'RELAÇÃO_TRABALHO_B_Não exerce atividade remunerada': 'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
    'RELAÇÃO_TRABALHO_B_Regimes informais': 'RELAÇÃO_TRABALHO_B_Regimes_informais',
    'Estado_Civil_Divorciada(o)': 'Estado_Civil_Divorciada',
    'Estado_Civil_Solteira(o)': 'Estado_Civil_Solteira',
    'Estado_Civil_União Estável (com registro em cartório': 'Estado_Civil_União_Estável_com_registro_em_cartório',
    'Estado_Civil_União Estável (sem registro no cartório)': 'Estado_Civil_União_Estável_sem_registro_no_cartório',
    'RENDA_B_De 1 a 2 salários mínimos': 'RENDA_B_De_1_a_2_salários_mínimos',
    'RENDA_B_Mais de 2 salários mínimos': 'RENDA_B_Mais_de_2_salários_mínimos',
    'RENDA_B_Sem renda': 'RENDA_B_Sem_renda',
    'AUTOR_B_Ex-cônjuge, ex-companheiro, ex-namorado': 'AUTOR_B_Ex_cônjuge_ex_companheiro_ex_namorado',
    'NATURALIDADE_B_Sudeste_fora_de_São Paulo': 'NATURALIDADE_B_Sudeste_fora_de_São_Paulo',
    'Estado_Civil_Viúva(o)': 'Estado_Civil_Viúva',
}, inplace=True)

grupo3_dummies_binario_RE.info()

#%% Verificando Multicolinearidade gRUPO 3

def cramers_v(x, y):
    contingency_table = pd.crosstab(x, y)
    chi2, p, dof, ex = chi2_contingency(contingency_table)
    n = contingency_table.sum().sum()
    return np.sqrt(chi2 / (n * (min(contingency_table.shape) - 1)))

# Exemplo de dataframe com variáveis categóricas
Correlacao_geral = pd.DataFrame(grupo3_dummies_binario_RE)

# Lista de variáveis categóricas binárias (as colunas do DataFrame)
categorical_columns = Correlacao_geral.columns

# Matriz de correlação de Cramér's V
corr_matrix = pd.DataFrame(np.zeros((len(categorical_columns), len(categorical_columns))),
                           index=categorical_columns, columns=categorical_columns)

# Preenchendo a matriz de correlação
for col1 in categorical_columns:
    for col2 in categorical_columns:
        corr_matrix.loc[col1, col2] = cramers_v(Correlacao_geral[col1], Correlacao_geral[col2])

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

#%% Modelo binário grupo 3

lista_colunas3_RE = list(grupo3_dummies_binario_RE.drop(columns=['Grupo',
                                                           'REGRESSAO_MULTINOMIAL_Primeiro_Atendimento',
                                                         'REGRESSAO_MULTINOMIAL_Retornou',
                                                         'REGRESSAO_MULTINOMIAL_Reativou', 
                                                         'IDADE', 
                                                         'NATURALIDADE_B_Outros', 
                                                         'NATURALIDADE_B_São_Paulo',
                                                         'RELAÇÃO_TRABALHO_B_Não_exerce_atividade_remunerada',
                                                         'RELAÇÃO_TRABALHO_B_Outros',
                                                         'Estado_Civil_União_Estável_com_registro_em_cartório',
                                                         'RENDA_B_Mais_de_2_salários_mínimos',
                                                         'RENDA_B_De_1_a_2_salários_mínimos']).columns)

formula_dummies_modelo3_RE = ' + '.join(lista_colunas3_RE)
formula_dummies_modelo3_RE = "REGRESSAO_MULTINOMIAL_Retornou ~ " + formula_dummies_modelo3_RE
print("Fórmula utilizada: ",formula_dummies_modelo3_RE)

modelo_binario3_RE = sm.Logit.from_formula(formula_dummies_modelo3_RE,
                                               grupo3_dummies_binario_RE).fit()

modelo_binario3_RE.summary()

#%% stepwise

from statstests.process import stepwise

step_modelo_binario3_RE = stepwise(modelo_binario3_RE, pvalue_limit=0.11)

#%% Construção de função para a definição da matriz de confusão

from sklearn.metrics import confusion_matrix, accuracy_score,\
    ConfusionMatrixDisplay, recall_score

def matriz_confusao(predicts, observado, cutoff):
    
    values = predicts.values
    
    predicao_binaria = []
        
    for item in values:
        if item < cutoff:
            predicao_binaria.append(0)
        else:
            predicao_binaria.append(1)
           
    cm = confusion_matrix(predicao_binaria, observado)
    disp = ConfusionMatrixDisplay(confusion_matrix=cm)
    disp.plot()
    plt.xlabel('True')
    plt.ylabel('Classified')
    plt.gca().invert_xaxis()
    plt.gca().invert_yaxis()
    plt.show()
        
    sensitividade = recall_score(observado, predicao_binaria, pos_label=1)
    especificidade = recall_score(observado, predicao_binaria, pos_label=0)
    acuracia = accuracy_score(observado, predicao_binaria)

    #Visualizando os principais indicadores desta matriz de confusão
    indicadores = pd.DataFrame({'Sensitividade':[sensitividade],
                                'Especificidade':[especificidade],
                                'Acurácia':[acuracia]})
    return indicadores

#%% Construção da matriz de confusão 

grupo3_dummies_binario_RE['phat'] = step_modelo_binario3_RE.predict()

# Matriz de confusão para cutoff = 0.2 (utilizou-se de 0,2 para diminuir a sensitividade e aumentar os verdadeiros positivos que é o objetivo)
matriz_confusao(observado=grupo3_dummies_binario_RE['REGRESSAO_MULTINOMIAL_Retornou'],
                predicts=grupo3_dummies_binario_RE['phat'],
                cutoff=0.3)

cutoff = 0.3
grupo3_dummies_binario_RE['Sim'] = (grupo3_dummies_binario_RE['phat'] >= cutoff).astype(int)

#%% Predição

grupo3_dummies_binario_RE['phat'] = modelo_binario3_RE.predict()

#%% 
matriz_confusao(observado=grupo3_dummies_binario_RE['REGRESSAO_MULTINOMIAL_Retornou'],
                predicts=grupo3_dummies_binario_RE['phat'], 
                cutoff=0.3)


#%% Eficiência do modelo com a Curva ROC
from sklearn.metrics import roc_curve, auc


fpr, tpr, thresholds =roc_curve(grupo3_dummies_binario_RE['REGRESSAO_MULTINOMIAL_Retornou'],
                                grupo3_dummies_binario_RE['phat'])
roc_auc = auc(fpr, tpr)

# Cálculo do coeficiente de GINI
gini = (roc_auc - 0.3)/(0.3)

# Plotando a curva ROC
plt.figure(figsize=(15,10))
plt.plot(fpr, tpr, marker='o', color='darkorchid', markersize=10, linewidth=3)
plt.plot(fpr, fpr, color='gray', linestyle='dashed')
plt.title('Área abaixo da curva: %g' % round(roc_auc, 4) +
          ' | Coeficiente de GINI: %g' % round(gini, 4), fontsize=22)
plt.xlabel('1 - Especificidade', fontsize=20)
plt.ylabel('Sensitividade', fontsize=20)
plt.xticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.yticks(np.arange(0, 1.1, 0.2), fontsize=14)
plt.show()
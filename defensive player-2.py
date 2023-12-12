#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.metrics import mean_squared_error, accuracy_score
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from sklearn.linear_model import Lasso
from sklearn.model_selection import KFold, GridSearchCV


# In[2]:


YEARS = range(2020,2022)


# In[3]:


data = pd.DataFrame()

for i in YEARS:  
    i_data = pd.read_csv('https://github.com/nflverse/nflverse-data/releases/download/pbp/'                    'play_by_play_' + str(i) + '.csv.gz',
                   compression= 'gzip', low_memory= False)

    data = data._append(i_data, sort=True)


# solo_tackle * 1 + assist_tackle * 0.5 + tackled_for_loss * 1 + sack * 2 + qb_hit * 1 + pass_defended * 1 +
# interceptions * 3 + fumble_forced * 3 + fumble recovery * 3 + defensive_two_point_conv * 2 + Defensive Touchdowns * 6

# In[4]:


data1 = data.loc[data['defteam'].notnull() == True]


# In[6]:


df_numerics = data1.select_dtypes(include='number')


# In[8]:


missing_values = df_numerics.isnull().sum()
columns_to_drop = missing_values[missing_values > 90000].index
df_numerics = df_numerics.drop(columns=columns_to_drop)


# In[9]:


df_numerics = df_numerics.fillna(0)


# In[12]:


df_numerics.reset_index(drop=True, inplace=True)


# In[17]:


df_numerics1 = df_numerics.drop(columns = ['qb_hit','sack','solo_tackle', 'assist_tackle','tackled_for_loss','interception','fumble',
                  'touchdown'])


# In[18]:


scaler = StandardScaler()
data_scaled = scaler.fit_transform(df_numerics1)


# In[19]:


pca = PCA(n_components = 70)
pca_features = pca.fit_transform(data_scaled)


# In[101]:


pca_features.shape


# In[102]:


pca_df = pd.DataFrame(
    data=pca_features)


# In[18]:


plt.figure(figsize=(10, 6))
plt.plot(range(1, len(pca.explained_variance_ratio_) + 1), pca.explained_variance_ratio_.cumsum(), marker='o', linestyle='-')
plt.title('Explained Variance by Components')
plt.xlabel('Number of Components')
plt.ylabel('Cumulative Explained Variance')
plt.grid(True)
plt.show()


# # sack

# In[23]:


data1.reset_index(drop=True, inplace=True)


# In[24]:


a  = df_numerics[['qb_hit','sack','solo_tackle', 'assist_tackle','tackled_for_loss','interception','fumble',
                  'touchdown']]
a[['pass_defense_1_player_id','td_team','defteam','game_id']] = data1[['pass_defense_1_player_id','td_team','defteam','game_id']]


# In[25]:


a['touchdown'] = 0

a.loc[a['td_team'] == a['defteam'], 'touchdown'] = 1


# In[26]:


a['pass_defense_1_player_id'] = a['pass_defense_1_player_id'].notna().astype(int)


# In[27]:


a.rename(columns={'pass_defense_1_player_id': 'pass_defense'}, inplace=True)


# In[28]:


a = a.drop(columns = ['td_team','defteam'])


# In[30]:


b = a.apply(pd.to_numeric, errors='ignore', downcast='float')


# In[33]:


b['fp']=b['qb_hit']+b['sack']*2+b['solo_tackle']+b['assist_tackle'] *0.5+b['tackled_for_loss']+b['pass_defense']+b['interception'] *3 +b['fumble']*3+b['touchdown']*6


# In[32]:


solo_tackle * 1 + assist_tackle * 0.5 + tackled_for_loss * 1 + sack * 2 + qb_hit * 1 + pass_defended * 1 +
interceptions * 3 + fumble * 3  + Defensive Touchdowns * 6


# In[129]:


counts = (b.iloc[:, 0:9] == 1).sum()

# Plotting
plt.figure(figsize=(10, 6))
counts.plot(kind='bar')
plt.title('Count of Occurrences for Each Variable')
plt.xlabel('Variable')
plt.ylabel('Count')
plt.xticks(rotation=45)
plt.grid(axis='y')
plt.tight_layout()

# Show plot
plt.show()


# In[131]:


counts/91797


# # all

# In[638]:


list_variables = b.iloc[:, 0:9].columns.tolist()
best_alpha_list = []
for i in list_variables:
    X = pca_df
    y = b[i]

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=42)
    
    params = {"alpha":np.arange(0.00001, 10, 500)}
    kf=KFold(n_splits=5,shuffle=True, random_state=42)
    lasso = Lasso()
    lasso_cv=GridSearchCV(lasso, param_grid=params, cv=kf)
    lasso_cv.fit(X, y)
    
    lasso = Lasso(alpha=float(lasso_cv.best_params_['alpha']))
    lasso.fit(X_train, y_train)
    lasso_coef = np.abs(lasso.coef_)
    best_alpha_list.append(lasso_cv.best_params_['alpha'])


# In[639]:


best_alpha_list


# In[51]:


list_variables = b.iloc[:, 0:9].columns.tolist()
feature_subset_list = []
classification_result = []
best_models = []
for i in list_variables:
    X = pca_df
    y = b[i]

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=42)
        
    lasso = Lasso(alpha=0.00005)
    lasso.fit(X_train, y_train)
    lasso_coef = np.abs(lasso.coef_)
    
    names = X.columns
    
    features_with_coef = list(zip(names, lasso_coef))
    features_with_coef.sort(key=lambda x: x[1], reverse=True)
    feature_subset = [feature for feature, coef in features_with_coef[:20]]
    
    df = pca_df[feature_subset]
    feature_subset_list.append(feature_subset)
    
    df[i] = b[i].reset_index().drop(columns=['index']).shift(-1)
    df['game_id'] = b['game_id'].reset_index().drop(columns=['index'])
    
    df['row_number'] = df.groupby('game_id').cumcount()
    df = df[df['row_number'] != 0].drop(columns=['row_number'])
    
    X1 = df.drop(columns = [i,'game_id'])[:-1]
    y1 = df[i][:-1]
    print(i)
    X_train1, X_test1, y_train1, y_test1 = train_test_split(X1, y1, test_size=0.1, random_state=42)

    classification_models = {
        "Logistic Regression": LogisticRegression(max_iter=1000, random_state=42),
        "Decision Tree Classifier": DecisionTreeClassifier(random_state=42),
        "Random Forest Classifier": RandomForestClassifier(n_estimators=100, random_state=42),
        "Gradient Boosting Classifier": GradientBoostingClassifier(random_state=42)
    }
    
    classification_results = {}
    
    for name, model in classification_models.items():
        model.fit(X_train1, y_train1)
        y_pred = model.predict(X_test1)
        accuracy = accuracy_score(y_test1, y_pred)
        classification_results[name] = accuracy
    classification_result.append(classification_results)
    best_model = max(classification_results, key=classification_results.get)
    best_models.append(best_model)


# # Test

# In[39]:


data_test = pd.read_csv('https://github.com/nflverse/nflverse-data/releases/download/pbp/'                    'play_by_play_' + '2023' + '.csv.gz',
                   compression= 'gzip', low_memory= False)


# In[40]:


test0 = data_test.loc[data_test['defteam'].notnull() == True]


# In[41]:


test = test0.select_dtypes(include='number')


# In[42]:


test = test[df_numerics.columns]


# In[43]:


test = test.fillna(0)


# In[44]:


test[['pass_defense_1_player_id','td_team','defteam','game_id']] = test0[['pass_defense_1_player_id','td_team','defteam','game_id']]


# In[45]:


test = test.reset_index()


# In[46]:


test['touchdown'] = 0

test.loc[test['td_team'] == test['defteam'], 'touchdown'] = 1


# In[47]:


test['pass_defense_1_player_id'] = test['pass_defense_1_player_id'].notna().astype(int)
test.rename(columns={'pass_defense_1_player_id': 'pass_defense'}, inplace=True)


# In[48]:


fp_ture = test['qb_hit']+test['sack']*2+test['solo_tackle']+test['assist_tackle'] *0.5+test['tackled_for_loss']+test['pass_defense']+test['interception'] *3 +test['fumble']*3+test['touchdown']*6


# In[49]:


c = test[['qb_hit','sack','solo_tackle', 'assist_tackle','tackled_for_loss','interception','fumble',
                  'touchdown','pass_defense']]


# In[50]:


test = test.drop(columns = ['index','td_team','defteam','game_id','qb_hit','sack','solo_tackle', 'assist_tackle','tackled_for_loss','interception','fumble',
                  'touchdown'])


# # test pca

# In[52]:


scaler = StandardScaler()
data_scaled_test = scaler.fit_transform(test.iloc[:,0:-1])


# In[53]:


pca_features_test = pca.transform(data_scaled_test)


# In[55]:


pca_df_test = pd.DataFrame(
    data=pca_features_test)


# In[57]:


pred_list = []

for i in range(9):
    #X = test[feature_subset]
    #y = test[list_variables[i]]
    
    df_test = pca_df_test[feature_subset_list[i]]
    re = classification_models[best_models[i]].predict(df_test).tolist()
    pred_list.append(re)


# # fp

# In[113]:


test_pred_df = pd.DataFrame(pred_list)
test_pred_df = test_pred_df.transpose()
test_pred_df.columns = list_variables


# In[ ]:


solo_tackle * 1 + assist_tackle * 0.5 + tackled_for_loss * 1 + sack * 2 + qb_hit * 1 + pass_defended * 1 +
interceptions * 3 + fumble * 3  + Defensive Touchdowns * 6


# In[114]:


test_pred_df['fp'] = test_pred_df['qb_hit']+test_pred_df['sack']*2+test_pred_df['solo_tackle']+test_pred_df['assist_tackle'] *0.5+test_pred_df['tackled_for_loss']+test_pred_df['pass_defense']+test_pred_df['interception'] *3 +test_pred_df['fumble']*3+test_pred_df['touchdown']*6


# In[122]:


test_pred_df['fp'] = test_pred_df['fp'].shift(-1)


# In[148]:


MSE = mean_squared_error(fp_ture[1:], test_pred_df['fp'][:-1])


# In[149]:


MSE


# # no pca

# In[89]:


list_variables = b.iloc[:, 0:9].columns.tolist()
feature_subset_list = []
classification_result = []
best_models = []
for i in list_variables:
    X = df_numerics1
    y = b[i]

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=42)
        
    lasso = Lasso(alpha=0.00005)
    lasso.fit(X_train, y_train)
    lasso_coef = np.abs(lasso.coef_)
    
    names = X.columns
    
    features_with_coef = list(zip(names, lasso_coef))
    features_with_coef.sort(key=lambda x: x[1], reverse=True)
    feature_subset = [feature for feature, coef in features_with_coef[:20]]
    
    df = df_numerics1[feature_subset]
    feature_subset_list.append(feature_subset)
    
    df[i] = b[i].reset_index().drop(columns=['index']).shift(-1)
    df['game_id'] = b['game_id'].reset_index().drop(columns=['index'])
    
    df['row_number'] = df.groupby('game_id').cumcount()
    df = df[df['row_number'] != 0].drop(columns=['row_number'])
    
    X1 = df.drop(columns = [i,'game_id'])[:-1]
    y1 = df[i][:-1]
    print(i)
    X_train1, X_test1, y_train1, y_test1 = train_test_split(X1, y1, test_size=0.1, random_state=42)

    classification_models = {
        "Logistic Regression": LogisticRegression(max_iter=1000, random_state=42),
        "Decision Tree Classifier": DecisionTreeClassifier(random_state=42),
        "Random Forest Classifier": RandomForestClassifier(n_estimators=100, random_state=42),
        "Gradient Boosting Classifier": GradientBoostingClassifier(random_state=42)
    }
    
    classification_results = {}
    
    for name, model in classification_models.items():
        model.fit(X_train1, y_train1)
        y_pred = model.predict(X_test1)
        accuracy = accuracy_score(y_test1, y_pred)
        classification_results[name] = accuracy
    classification_result.append(classification_results)
    best_model = max(classification_results, key=classification_results.get)
    best_models.append(best_model)


# In[58]:


pred_list = []

for i in range(9):
    #X = test[feature_subset]
    #y = test[list_variables[i]]
    
    df_test = test[feature_subset_list[i]]
    re = classification_models[best_models[i]].predict(df_test).tolist()
    pred_list.append(re)


# In[59]:


test_pred_df = pd.DataFrame(pred_list)
test_pred_df = test_pred_df.transpose()
test_pred_df.columns = list_variables


# In[60]:


test_pred_df['fp'] = test_pred_df['qb_hit']+test_pred_df['sack']*2+test_pred_df['solo_tackle']+test_pred_df['assist_tackle'] *0.5+test_pred_df['tackled_for_loss']+test_pred_df['pass_defense']+test_pred_df['interception'] *3 +test_pred_df['fumble']*3+test_pred_df['touchdown']*6


# In[62]:


MSE = mean_squared_error(fp_ture[1:], test_pred_df['fp'][:-1])


# In[63]:


MSE


# # No lasso

# In[117]:


list_variables = b.iloc[:, 0:9].columns.tolist()
feature_subset_list = []
classification_result = []
best_models = []
X = pca_df

for i in list_variables:
    
    df = X.copy()
    df[i] = b[i].reset_index().drop(columns=['index']).shift(-1)
    df['game_id'] = b['game_id'].reset_index().drop(columns=['index'])
    
    df['row_number'] = df.groupby('game_id').cumcount()
    df = df[df['row_number'] != 0].drop(columns=['row_number'])
    
    
    
    X1 = df.drop(columns = [i,'game_id'])[:-1]
    y1 = df[i][:-1]
    print(i)
    X_train1, X_test1, y_train1, y_test1 = train_test_split(X1, y1, test_size=0.1, random_state=42)

    classification_models = {
        "Logistic Regression": LogisticRegression(max_iter=1000, random_state=42),
        "Decision Tree Classifier": DecisionTreeClassifier(random_state=42),
        "Random Forest Classifier": RandomForestClassifier(n_estimators=100, random_state=42),
        "Gradient Boosting Classifier": GradientBoostingClassifier(random_state=42)
    }
    
    classification_results = {}
    
    for name, model in classification_models.items():
        model.fit(X_train1, y_train1)
        y_pred = model.predict(X_test1)
        accuracy = accuracy_score(y_test1, y_pred)
        classification_results[name] = accuracy
        print(name)
    classification_result.append(classification_results)
    best_model = max(classification_results, key=classification_results.get)
    best_models.append(best_model)


# In[121]:


pred_list = []

for i in range(9):
    
    re = classification_models[best_models[i]].predict(pca_df_test).tolist()
    pred_list.append(re)


# In[122]:


test_pred_df = pd.DataFrame(pred_list)
test_pred_df = test_pred_df.transpose()
test_pred_df.columns = list_variables


# In[124]:


test_pred_df['fp'] = test_pred_df['qb_hit']+test_pred_df['sack']*2+test_pred_df['solo_tackle']+test_pred_df['assist_tackle'] *0.5+test_pred_df['tackled_for_loss']+test_pred_df['pass_defense']+test_pred_df['interception'] *3 +test_pred_df['fumble']*3+test_pred_df['touchdown']*6


# In[126]:


MSE = mean_squared_error(fp_ture[1:], test_pred_df['fp'][:-1])


# In[127]:


MSE


# # no lasso no pca

# In[ ]:


list_variables = b.iloc[:, 0:9].columns.tolist()
feature_subset_list = []
classification_result = []
best_models = []
for i in list_variables:
    X = df_numerics1
    y = b[i]
    
    
    df = X
    df[i] = b[i].reset_index().drop(columns=['index']).shift(-1)
    df['game_id'] = b['game_id'].reset_index().drop(columns=['index'])
    
    df['row_number'] = df.groupby('game_id').cumcount()
    df = df[df['row_number'] != 0].drop(columns=['row_number'])
    
    
    
    X1 = df.drop(columns = [i,'game_id'])[:-1]
    y1 = df[i][:-1]
    print(i)
    X_train1, X_test1, y_train1, y_test1 = train_test_split(X1, y1, test_size=0.1, random_state=42)

    classification_models = {
        "Logistic Regression": LogisticRegression(max_iter=1000, random_state=42),
        "Decision Tree Classifier": DecisionTreeClassifier(random_state=42),
        "Random Forest Classifier": RandomForestClassifier(n_estimators=100, random_state=42),
        "Gradient Boosting Classifier": GradientBoostingClassifier(random_state=42)
    }
    
    classification_results = {}
    
    for name, model in classification_models.items():
        model.fit(X_train1, y_train1)
        y_pred = model.predict(X_test1)
        accuracy = accuracy_score(y_test1, y_pred)
        classification_results[name] = accuracy
        print(name)
    classification_result.append(classification_results)
    best_model = max(classification_results, key=classification_results.get)
    best_models.append(best_model)


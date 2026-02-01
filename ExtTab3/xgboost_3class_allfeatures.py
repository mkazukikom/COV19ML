%matplotlib inline
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.manifold import TSNE
from sklearn.preprocessing import MinMaxScaler, StandardScaler, OneHotEncoder
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score, log_loss, mean_squared_error, confusion_matrix
from sklearn.metrics import recall_score, precision_score, f1_score, matthews_corrcoef
import xgboost as xgb
from tqdm import tqdm
#import umap

np.random.seed(51)

dir_name = '/content/drive/MyDrive/COV19ML_share/data_20250201/'
aav_data = pd.read_csv(dir_name+'AAV_data_3.csv')
ad_data = pd.read_csv(dir_name+'AD_data_3.csv')
covid_data = pd.read_csv(dir_name+'COVID_data_3.csv')
hc_data = pd.read_csv(dir_name+'HC_data_3.csv')
sle_data = pd.read_csv(dir_name+'SLE_data_3.csv')
ssc_data = pd.read_csv(dir_name+'SSc_data_3.csv')

data = pd.concat([aav_data, ad_data, covid_data, hc_data, sle_data, ssc_data])
data.CLASS.value_counts()

data['sex'] = data['sex'].replace({'F': 1, 'M': 0})

y_covid = data.COVID
y_covid= y_covid.to_numpy()
y_all = data.CLASS.to_numpy()

X = data.drop(columns=['ID','sex','age','CLASS','COVID'])
#X /= X.max(axis=0)
n_data, n_features = X.shape
feature_names = X.columns.values
for i in range(len(y_covid)):
    if y_all[i]=='COVID_moderate' or y_all[i]=='COVID_severe':
        y_covid[i] = 2
print(X.shape, len(y_covid[y_covid==0]), len(y_covid[y_covid==1]), len(y_covid[y_covid==2]) )
n_class = len(np.unique(y_covid))

from sklearn.metrics import roc_auc_score
import xgboost as xgb
import numpy as np
import pandas as pd
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, matthews_corrcoef, confusion_matrix

num_round = 40
lr = 0.05
acc_list_all_tr = []
acc_list_all_te = []
precision_list_all_tr = []
precision_list_all_te = []
recall_list_all_tr = []
recall_list_all_te = []
f1score_list_all_tr = []
f1score_list_all_te = []
mcc_list_all_tr = []
mcc_list_all_te = []
auc_list_all_tr = []
auc_list_all_te = []
table_list_all_tr = []
table_list_all_te = []
selected_feature_list = []
imp_list = []
n_fold = 10
X_np = X.to_numpy()
df_imp_all = pd.DataFrame({"feature_importance": np.zeros(len(feature_names))}, index=feature_names)
skf = StratifiedKFold(n_splits=n_fold, shuffle=True, random_state=0) 
for (tr, te) in skf.split(X_np, y_all):   # 全クラスのデータを均一に分割
    X_tr, X_te, y_tr, y_te = X_np[tr], X_np[te], y_covid[tr], y_covid[te]
    # scaler = StandardScaler()
    # X_tr = scaler.fit_transform(X_tr)
    # X_te = scaler.transform(X_te)
    dtrain_all = xgb.DMatrix(pd.DataFrame(X_tr, columns=feature_names), label=y_tr) #, feature_names=feature_names.tolist())
    dtest_all = xgb.DMatrix(pd.DataFrame(X_te, columns=feature_names), label=y_te)
    param = {'objective': 'multi:softprob', 'nthread': 1, 'num_class': len(np.unique(y_covid)), 'eta': lr, 'subsample': 0.5}
    param['eval_metric'] = ['merror', 'mlogloss']
    evallist_all = [(dtrain_all, 'train')] # , (dtest, 'eval')]
    bst = xgb.train(param, dtrain_all, num_round, evallist_all) #, early_stopping_rounds=10)

    pred_tr_prob = bst.predict(dtrain_all) # Get probabilities for AUC
    pred_te_prob = bst.predict(dtest_all) # Get probabilities for AUC

    df_imp_each = pd.DataFrame({"feature_importance": np.zeros(len(feature_names))}, index=feature_names)
    for k, v in zip(bst.get_fscore().keys(), bst.get_fscore().values()):
        df_imp_all.at[k,"feature_importance"] += v
        df_imp_each.at[k,"feature_importance"] += v
    ### feature selection ###
    df_imp_each = df_imp_each.sort_values(by="feature_importance", ascending=False)
    imp_list.append(df_imp_each)
    #

    ###
    pred_tr = pred_tr_prob.argmax(axis=1)
    pred_te = pred_te_prob.argmax(axis=1)

    acc_tr = accuracy_score(y_tr, pred_tr) # .argmax(axis=1) )
    acc_te = accuracy_score(y_te, pred_te) # .argmax(axis=1) )
    precision_tr = precision_score(y_tr,pred_tr,average='macro')
    precision_te = precision_score(y_te,pred_te,average='macro')
    recall_tr = recall_score(y_tr,pred_tr,average='macro')
    recall_te = recall_score(y_te,pred_te,average='macro')
    f1score_tr = f1_score(y_tr,pred_tr,average='macro')
    f1score_te = f1_score(y_te,pred_te,average='macro')
    mcc_tr = matthews_corrcoef(y_tr, pred_tr)
    mcc_te = matthews_corrcoef(y_te, pred_te)
    auc_tr = roc_auc_score(y_tr, pred_tr_prob, multi_class='ovr', labels=np.unique(y_covid))
    auc_te = roc_auc_score(y_te, pred_te_prob, multi_class='ovr', labels=np.unique(y_covid))

    table_tr = confusion_matrix(y_tr, pred_tr, labels=[i for i in range(n_class)])
    table_tr = pd.DataFrame(table_tr, index=["label"+str(i) for i in range(n_class)], columns=["pred"+str(i) for i in range(n_class)]).T
    table_te = confusion_matrix(y_te, pred_te, labels=[i for i in range(n_class)])
    table_te = pd.DataFrame(table_te, index=["label"+str(i) for i in range(n_class)], columns=["pred"+str(i) for i in range(n_class)]).T
    acc_list_all_tr.append(acc_tr)
    acc_list_all_te.append(acc_te)
    precision_list_all_tr.append(precision_tr)
    precision_list_all_te.append(precision_te)
    recall_list_all_tr.append(recall_tr)
    recall_list_all_te.append(recall_te)
    f1score_list_all_tr.append(f1score_tr)
    f1score_list_all_te.append(f1score_te)
    mcc_list_all_tr.append(mcc_tr)
    mcc_list_all_te.append(mcc_te)
    auc_list_all_tr.append(auc_tr)
    auc_list_all_te.append(auc_te)
    table_list_all_tr.append(table_tr)
    table_list_all_te.append(table_te)
df_imp_all = df_imp_all.sort_values(by="feature_importance", ascending=False)
df_imp_all['feature_importance'] /= n_fold
print(f"tr mean: {np.mean(acc_list_all_tr)}, tr stddev: {np.std(acc_list_all_tr)}, te mean: {np.mean(acc_list_all_te)}, te stddev: {np.std(acc_list_all_te)}")

for i in range(n_fold):
    table_list_all_tr[i].index = "train_"+table_list_all_tr[i].index+"_cv"+str(i)
    table_list_all_te[i].index = "valid_"+table_list_all_te[i].index+"_cv"+str(i)

for i in range(n_fold):
    print(table_list_all_tr[i].to_markdown())
    print(f'acc:{acc_list_all_tr[i]:.5f}, recall:{recall_list_all_tr[i]:.5f}, precision:{precision_list_all_tr[i]:.5f}, fvalue:{f1score_list_all_tr[i]:.5f}, mcc:{mcc_list_all_te[i]:.5f}')

for i in range(n_fold):
    print(table_list_all_te[i].to_markdown())
    print(f'acc:{acc_list_all_te[i]:.5f}, recall:{recall_list_all_te[i]:.5f}, precision:{precision_list_all_te[i]:.5f}, fvalue:{f1score_list_all_te[i]:.5f}, mcc:{mcc_list_all_te[i]:.5f}')

import pandas as pd
import numpy as np
from sklearn.metrics import roc_auc_score

df_all_metrics = pd.DataFrame({
    "train_accuracy": acc_list_all_tr,
    "test_accuracy": acc_list_all_te,
    "train_auc": auc_list_all_tr,
    "test_auc": auc_list_all_te,
    "train_recall": recall_list_all_tr,
    "test_recall": recall_list_all_te,
    "train_precision": precision_list_all_tr,
    "test_precision": precision_list_all_te,
    "train_f1_score": f1score_list_all_tr,
    "test_f1_score": f1score_list_all_te,
    "train_mcc": mcc_list_all_tr,
    "test_mcc": mcc_list_all_te
})

# Calculate mean and standard deviation
mean_row = df_all_metrics.mean().to_frame().T
mean_row.index = ['mean']
std_row = df_all_metrics.std().to_frame().T
std_row.index = ['stddev']

# Concatenate mean and stddev rows to the DataFrame
df_all_metrics_with_stats = pd.concat([df_all_metrics, mean_row, std_row])

print(df_all_metrics_with_stats.to_markdown())

tmp_imp = pd.concat(imp_list, axis=1)
tmp_imp_stat = pd.concat([tmp_imp.T.mean(), tmp_imp.T.std()],axis=1)
tmp_imp_stat.columns=["mean","std"]
tmp_imp_stat.sort_values(by="mean", ascending=False,inplace=True)
tmp_imp_stat.to_csv("xgboost_2class_feature_importance.csv", index=True)
print(tmp_imp_stat.iloc()[:30,:].to_markdown())

tmp_imp = pd.concat(imp_list, axis=1)
tmp_imp_stat = pd.concat([tmp_imp.T.mean(), tmp_imp.T.std()],axis=1)
tmp_imp_stat.columns=["mean","std"]
tmp_imp_stat.sort_values(by="mean", ascending=False,inplace=True)
tmp_imp_stat.to_csv("xgboost_3class_feature_importance.csv", index=True)
print(tmp_imp_stat.iloc()[:30,:].to_markdown())

imp_dict = {}
for k, v in zip(df_imp_all.index, df_imp_all.feature_importance):
    if v>0:
        imp_dict[k] = v
xgb.plot_importance(imp_dict, xlabel="feature importance", max_num_features=10)

%matplotlib inline
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler, StandardScaler, OneHotEncoder
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score, recall_score, precision_score, f1_score, confusion_matrix, roc_auc_score, matthews_corrcoef
import xgboost as xgb

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

y_covid = data.COVID
y_covid= y_covid.to_numpy()
y_all = data.CLASS.to_numpy()

X = data.drop(columns=['ID','sex','age','CLASS','COVID'])
X=X.reset_index(drop=True)
#X /= X.max(axis=0)
n_data, n_features = X.shape
feature_names = X.columns.values
n_class = len(np.unique(y_covid))
print(X.shape, len(y_covid[y_covid==0]), len(y_covid[y_covid==1]) )

num_round=100
final_dtrain = xgb.DMatrix(X, label=y_covid) # train: 入力データ, label: ラベル
final_param = {
    'objective': 'binary:logistic', #'nthread': 1,
    'eta': 0.05,
    'subsample': 0.5,
    'max_depth': 5,
    'eval_metric': ['error', 'logloss', 'auc'],
    'scale_pos_weight': len(y_covid[y_covid==0])/len(y_covid[y_covid==1]),
    'reg_alpha': 1e-3,
    'reg_lambda': 1e-3
}

cv_res = xgb.cv(
    final_param,
    final_dtrain,
    num_boost_round=num_round,
    nfold=10,
    stratified=True,
    metrics={'error', 'logloss', 'auc'},
    seed=0,
    callbacks=[xgb.callback.EvaluationMonitor(show_stdv=True)]
)

fig, (ax1, ax2, ax3) = plt.subplots(1,3,figsize=(20,6))
ax1.errorbar(x=np.arange(num_round),y=cv_res['test-auc-mean'], yerr=cv_res['test-auc-std'])
ax1.set_ylim(0,1)
ax2.errorbar(x=np.arange(num_round),y=cv_res['test-logloss-mean'], yerr=cv_res['test-logloss-std'])
ax2.set_ylim(bottom=0)
ax3.errorbar(x=np.arange(num_round),y=cv_res['test-error-mean'], yerr=cv_res['test-error-std'])
ax3.set_ylim(bottom=0)
plt.show()

num_round = cv_res['test-auc-mean'].argmax()
print(f'{num_round}, acc: {1-cv_res["test-error-mean"][num_round]}, auc: {cv_res["test-auc-mean"][num_round]}, logloss: {cv_res["test-logloss-mean"][num_round]}')

num_round = 100
lr = 0.05
acc_list_all_tr = []
acc_list_all_te = []
auc_list_all_tr = []
auc_list_all_te = []
precision_list_all_tr = []
precision_list_all_te = []
recall_list_all_tr = []
recall_list_all_te = []
f1score_list_all_tr = []
f1score_list_all_te = []
mcc_list_all_tr = []
mcc_list_all_te = []
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
    dtrain_all = xgb.DMatrix(pd.DataFrame(X_tr, columns=feature_names), label=y_tr) #, feature_names=feature_names.tolist())
    dtest_all = xgb.DMatrix(pd.DataFrame(X_te, columns=feature_names), label=y_te)
    param = {'objective': 'binary:logistic', 'eta': lr, 'subsample': 0.5,
             'max_depth': 5, 'scale_pos_weight': len(y_tr[y_tr==0])/len(y_tr[y_tr==1]),
             'reg_alpha': 1e-3, 'reg_lambda': 1e-3}
    param['eval_metric'] = ['error', 'auc', 'logloss'] # 最後のメトリックで評価される
    evallist_all = [(dtrain_all, 'train')] # , (dtest, 'eval')]
    bst = xgb.train(param, dtrain_all, num_round, evals=evallist_all) #, early_stopping_rounds=10)
    #pred_tr = bst.predict(dtrain_all) #, iteration_range=(0, bst.best_iteration + 1))
    df_imp_each = pd.DataFrame({"feature_importance": np.zeros(len(feature_names))}, index=feature_names)
    for k, v in zip(bst.get_fscore().keys(), bst.get_fscore().values()):
        df_imp_all.at[k,"feature_importance"] += v
        df_imp_each.at[k,"feature_importance"] += v
    ### feature selection ###
    df_imp_each = df_imp_each.sort_values(by="feature_importance", ascending=False)
    imp_list.append(df_imp_each)
    '''
    # print(df_imp_each)
    selected_feature_names = df_imp_each.iloc()[:n_selected_features].index.tolist()
    f_imp = df_imp_each[df_imp_each.index==selected_feature_names[-1]].feature_importance.values[0]
    #
    selected_feature_names = df_imp_each[df_imp_each.feature_importance>=f_imp].index.tolist()
    print(len(selected_feature_names), selected_feature_names)
    #selected_feature_names = df_imp_each[:n_selected_features].index.tolist() # top5
    selected_feature_list.append(selected_feature_names)
    #print(selected_feature_names)
    X_selected_np = X[selected_feature_names].to_numpy()
    dtrain = xgb.DMatrix(pd.DataFrame(X_selected_np[tr], columns=selected_feature_names), label=y_tr) #, feature_names=feature_names.tolist())
    dtest = xgb.DMatrix(pd.DataFrame(X_selected_np[te], columns=selected_feature_names), label=y_te)
    evallist = [(dtrain, 'train')] # , (dtest, 'eval')]
    bst = xgb.train(param, dtrain, num_round, evallist) #, early_stopping_rounds=10)
    '''
    pred_tr = bst.predict(dtrain_all) #, iteration_range=(0, bst.best_iteration + 1))
    pred_te = bst.predict(dtest_all) #, iteration_range=(0, bst.best_iteration + 1))
    auc_tr = roc_auc_score(y_tr, pred_tr) # .argmax(axis=1) )
    auc_te = roc_auc_score(y_te, pred_te) # .argmax(axis=1) )
    pred_tr = [0 if i<0.5 else 1 for i in pred_tr]
    pred_te = [0 if i<0.5 else 1 for i in pred_te]
    acc_tr = accuracy_score(y_tr, pred_tr) # .argmax(axis=1) )
    acc_te = accuracy_score(y_te, pred_te) # .argmax(axis=1) )
    precision_tr = precision_score(y_tr,pred_tr)
    precision_te = precision_score(y_te,pred_te)
    recall_tr = recall_score(y_tr,pred_tr)
    recall_te = recall_score(y_te,pred_te)
    f1score_tr = f1_score(y_tr,pred_tr)
    f1score_te = f1_score(y_te,pred_te)
    mcc_tr = matthews_corrcoef(y_tr, pred_tr)
    mcc_te = matthews_corrcoef(y_te, pred_te)
    table_tr = confusion_matrix(y_tr, pred_tr, labels=[i for i in range(n_class)])
    table_tr = pd.DataFrame(table_tr, index=["label"+str(i) for i in range(n_class)], columns=["pred"+str(i) for i in range(n_class)]).T
    table_te = confusion_matrix(y_te, pred_te, labels=[i for i in range(n_class)])
    table_te = pd.DataFrame(table_te, index=["label"+str(i) for i in range(n_class)], columns=["pred"+str(i) for i in range(n_class)]).T
    acc_list_all_tr.append(acc_tr)
    acc_list_all_te.append(acc_te)
    auc_list_all_tr.append(auc_tr)
    auc_list_all_te.append(auc_te)
    precision_list_all_tr.append(precision_tr)
    precision_list_all_te.append(precision_te)
    recall_list_all_tr.append(recall_tr)
    recall_list_all_te.append(recall_te)
    f1score_list_all_tr.append(f1score_tr)
    f1score_list_all_te.append(f1score_te)
    mcc_list_all_tr.append(mcc_tr)
    mcc_list_all_te.append(mcc_te)
    table_list_all_tr.append(table_tr)
    table_list_all_te.append(table_te)
df_imp_all = df_imp_all.sort_values(by="feature_importance", ascending=False)
df_imp_all['feature_importance'] /= n_fold
print(f"acc tr mean: {np.mean(acc_list_all_tr)}, tr stddev: {np.std(acc_list_all_tr)}, te mean: {np.mean(acc_list_all_te)}, te stddev: {np.std(acc_list_all_te)}")
print(f"auc tr mean: {np.mean(auc_list_all_tr)}, tr stddev: {np.std(auc_list_all_tr)}, te mean: {np.mean(auc_list_all_te)}, te stddev: {np.std(auc_list_all_te)}")

for i in range(n_fold):
    table_list_all_tr[i].index = "train_"+table_list_all_tr[i].index+"_cv"+str(i)
    table_list_all_te[i].index = "valid_"+table_list_all_te[i].index+"_cv"+str(i)
for i in range(n_fold):
    imp_list[i].columns += '_cv' +str(i)

import pandas as pd
import numpy as np

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

imp_dict = {}
for k, v in zip(df_imp_all.index, df_imp_all.feature_importance):
    if v>0:
        imp_dict[k] = v
xgb.plot_importance(imp_dict, max_num_features=10, xlabel="Feature Importance")

tmp_imp = pd.concat(imp_list, axis=1)
tmp_imp_stat = pd.concat([tmp_imp.T.mean(), tmp_imp.T.std()],axis=1)
tmp_imp_stat.columns=["mean","std"]
tmp_imp_stat.sort_values(by="mean", ascending=False,inplace=True)
print(tmp_imp_stat.iloc()[:10,:].to_markdown())

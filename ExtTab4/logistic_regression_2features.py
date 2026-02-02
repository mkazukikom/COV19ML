%matplotlib inline
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler, StandardScaler, OneHotEncoder
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score, recall_score, precision_score, f1_score, confusion_matrix, roc_auc_score, matthews_corrcoef
from sklearn.linear_model import LogisticRegression

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

X=X[["BCORP1","KAT2A"]]

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
n_fold = 10
X_np = X.to_numpy()
skf = StratifiedKFold(n_splits=n_fold, shuffle=True, random_state=0)
for (tr, te) in skf.split(X_np, y_all):   # 全クラスのデータを均一に分割
    X_tr, X_te, y_tr, y_te = X_np[tr], X_np[te], y_covid[tr], y_covid[te]
    clf = LogisticRegression(random_state=0).fit(X_tr, y_tr)
    pred_tr = clf.predict_proba(X_tr)[:,1]
    pred_te = clf.predict_proba(X_te)[:,1]
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

df_all_acc = pd.DataFrame([np.mean(acc_list_all_tr), np.std(acc_list_all_tr), np.mean(acc_list_all_te), np.std(acc_list_all_te)]).T
df_all_auc = pd.DataFrame([np.mean(auc_list_all_tr), np.std(auc_list_all_tr), np.mean(auc_list_all_te), np.std(auc_list_all_te)]).T
df_all_recall = pd.DataFrame([np.mean(recall_list_all_tr), np.std(recall_list_all_tr), np.mean(recall_list_all_te), np.std(recall_list_all_te)]).T
df_all_precision = pd.DataFrame([np.mean(precision_list_all_tr), np.std(precision_list_all_tr), np.mean(precision_list_all_te), np.std(precision_list_all_te)]).T
df_all_fvalue = pd.DataFrame([np.mean(f1score_list_all_tr), np.std(f1score_list_all_tr), np.mean(f1score_list_all_te), np.std(f1score_list_all_te)]).T
df_all_mcc = pd.DataFrame([np.mean(mcc_list_all_tr), np.std(mcc_list_all_tr), np.mean(mcc_list_all_te), np.std(mcc_list_all_te)]).T

df = pd.concat([df_all_acc,df_all_auc,df_all_recall,df_all_precision,df_all_fvalue,df_all_mcc], axis=0)
df.columns = ["train mean", "train stddev", "valid mean", "valid stddev"]
df.index = ["accuracy","auc","recall","precision","f1_score","matthews_corrcoef"]
print(df.to_markdown())

df_detailed_metrics = pd.DataFrame({
    'Accuracy (Training)': acc_list_all_tr,
    'Accuracy (Testing)': acc_list_all_te,
    'AUC (Training)': auc_list_all_tr,
    'AUC (Testing)': auc_list_all_te,
    'Recall (Training)': recall_list_all_tr,
    'Recall (Testing)': recall_list_all_te,
    'Precision (Training)': precision_list_all_tr,
    'Precision (Testing)': precision_list_all_te,
    'F1-score (Training)': f1score_list_all_tr,
    'F1-score (Testing)': f1score_list_all_te,
    'MCC (Training)': mcc_list_all_tr,
    'MCC (Testing)': mcc_list_all_te
})
print(df_detailed_metrics.to_markdown(index=False))

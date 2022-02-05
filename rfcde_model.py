import rfcde
import pickle
import datetime
import pandas as pd
import numpy as np
import sys
import multiprocessing as mp

data = pd.read_csv("data_RFCDE.csv")

residuals = data.profit - data.pred_profit
data['residuals'] = residuals
x_train = data[['pred_profit']]
y_train = data[['residuals']]

node_size = 50
n_basis = 30
n_trees = 100
basis_system = 'cosine'

forest = rfcde.RFCDE(n_trees=n_trees, mtry= 1, node_size=node_size, n_basis=n_basis)
forest.train(np.array(x_train),np.array(y_train))

z_grid = np.linspace(-30591,74185,74185+30591+1)
i = 0
while i < x_train.shape[0]:
	print(i)
	tmp2 = forest.predict(np.array(x_train)[i:min(i+10000,x_train.shape[0])],z_grid, 1)
	i = i + 10000
	with open("rfcde_topshot_"+str(i)+".pkl","wb") as f:
		pickle.dump(tmp2,f)

path = "/Volumes/Seagate Desktop Media/nba-topshot/"
i = 0 
while i < x_train.shape[0]:
	print(i)
	with open(path+"rfcde_topshot_"+str(i+10000)+".pkl","rb") as f:
		tmp2 = pickle.load(f)
	print(path+"rfcde_topshot_"+str(i+10000)+".pkl")
	x_train_tmp = np.array(x_train)[i:min(i+10000,x_train.shape[0])]
	y_train_tmp = np.array(y_train)[i:min(i+10000,x_train.shape[0])]
	for k in range(tmp2.shape[0]):
		density = tmp2[k]
		true_profit = np.array(x_train_tmp)[k,]+np.array(y_train_tmp)[k,]
		if true_profit > 0:
			prob = sum(density[int(np.round(np.array(y_train_tmp)[k,])[0])+30591:-1])
		else:
			prob = 1 - sum(density[int(np.round(np.array(y_train_tmp)[k,])[0])+30591:-1])
		outlier_probs.append(prob)
	i = i + 10000

data['probs_cde'] = outlier_probs
subset_d = data[(data['probs_cde'] < 0.01) & (data['profit_ae'] > 0)].reset_index(drop=True)
subset_d.to_csv("an_net.csv",index=False)

## plot an example of residual CDE

from matplotlib import pyplot as plt

fig = plt.figure()
cde_predicted = density.copy()
plt.plot(z_grid[30591-30:30591+30], cde_predicted[30591-30:30591+30], label=r'$\hat{f}(r| \hat{p})$')
plt.axvline(7.7, color='red', label=r'$r$')
plt.xticks(size=16)
plt.yticks(size=16)
plt.xlabel(r'Residuals ($)', size=20)
plt.ylabel('CDE', size=20)
plt.legend(loc='upper right', prop={'size': 20})


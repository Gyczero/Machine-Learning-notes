# -*- coding: utf-8 -*-
# @Time    : 2019-04-13 17:58
# @Author  : Frenkie
# @File    : keras_predict.py

import pandas as pd
import numpy as np
np.set_printoptions(precision=4)

import os
import gc
import warnings
from tqdm import tqdm

from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split, StratifiedKFold
from sklearn.metrics import precision_score, recall_score, roc_curve, roc_auc_score, auc

import tensorflow as tf
import keras
from keras import layers, regularizers
from keras import backend as K
from keras.constraints import max_norm
from keras.models import Sequential
from keras.callbacks import TensorBoard, LearningRateScheduler, EarlyStopping, ReduceLROnPlateau
from keras.models import load_model, Model
from keras.initializers import glorot_uniform
from keras.layers import Input, Dense, Activation, BatchNormalization, Dropout
from keras import optimizers
from keras.utils import plot_model

import h5py

# keras options
weighted = False
balanced = False

# keras vis config
log_dir = 'log_dir'
if not os.path.exists(log_dir):
    os.mkdir(log_dir)

# test size
test_size = 0.3

def auc(y_true, y_pred):
    '''
    增加auc指标
    :param y_true:
    :param y_pred:
    :return:
    '''
    auc = tf.metrics.auc(y_true, y_pred)[1]
    K.get_session().run(tf.local_variables_initializer())
    return auc

def create_model_nn(input_dim, layer_size=200):
    '''
    定义模型结构
    :param input_dim:
    :param layer_size:
    :return:
    '''
    model = Sequential()

    # 输入层，input_dim=特征数
    model.add(Dense(layer_size, input_dim=input_dim, kernel_initializer='normal'))

    # 批量标准化层 =
    model.add(BatchNormalization())
    model.add(Dropout(0.3))
    for i in tqdm(range(2)):
        model.add(Dense(layer_size))
        model.add(BatchNormalization())
        model.add(Activation('relu'))
        model.add(Dropout(0.3))

    model.add(Dense(1, activation='sigmoid'))
    adam = optimizers.Adam(lr=0.001)
    model.compile(optimizer=adam, loss='binary_crossentropy', metrics=[auc])
    return model

from tensorboard import version

if __name__ == '__main__':

    # 通过nrows和skiprows选择想要的部分
    # 通过使用dtypes设置数据类型，节约内存 / 使用astype("float32") 节约内存

    train = pd.read_csv('train.csv', nrows=2000, skiprows=[1, 10, 100])
    Y = train['target']
    train = train.drop(['target', 'ID_code'], axis=1)
    print train.columns

    # print len(train)
    #
    # # train.to_hdf('t1.h5', key='df', mode='w')
    # # train = pd.read_hdf('t1.h5', key='df')
    #
    # test = pd.read_csv("test.csv", nrows=100, skiprows=[1, 10, 100])
    #
    X_train, X_test, y_train, y_test = train_test_split(train, Y, test_size=test_size, random_state=42)
    model_nn = create_model_nn(X_train.shape[1])

    call_back_stop = EarlyStopping(monitor='val_auc', patience=50, verbose=0, mode='max')
    call_back_vis = TensorBoard(log_dir=log_dir,
                                histogram_freq=1,
                                write_graph=True,
                                write_grads=True,
                                write_images=True)

    history = model_nn.fit(X_train, y_train, validation_data=(X_test, y_test), epochs=50, batch_size=64,
                           verbose=1, callbacks=[call_back_stop, call_back_vis])
    print "\n Validation Max Score is {}".format(np.max(history.history['val_auc']))


    # 模型保存和加载
    # model_nn.save('t1_model.h5')
    # load_model = load_model('t1_model.h5')

    # 模型可视化
    plot_model(model_nn, to_file="model.png")






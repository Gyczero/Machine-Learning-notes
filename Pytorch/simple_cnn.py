# -*- coding: utf-8 -*-
# @Time    : 2019-04-21 18:46
# @Author  : Frenkie
# @File    : simple_cnn.py

import torch
import torch.nn as nn
import torch.nn.functional as F


class Net(nn.Module):
    '''
    神经网络模块
    '''

    def __init__(self):

        # 获取Net类的父类，并且初始化父类
        super(Net, self).__init__()

        # kernel
        self.conv1 = nn.Conv2d(1, 6, 5)
        self.conv2 = nn.Conv2d(6, 16, 5)

        self.fc1 = nn.Linear(16 * 5 * 5, 120)
        self.fc2 = nn.Linear(120, 84)
        self.fc3 = nn.Linear(84, 10)

    def forward(self, x):

        # 通过(2,2)的窗口进行池化
        x = F.max_pool2d(F.relu(self.conv1(x)), (2,2))
        x = F.max_pool2d(F.relu(self.conv2(x)), 2)
        x = x.view(-1, self.num_flat_features(x))
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = self.fc3(x)
        return x

    def num_flat_features(self, x):
        size = x.size()[1:]
        num_features = 1
        for s in size:
            num_features *= s
        return num_features



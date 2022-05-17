'''
Author: SuBonan
Date: 2022-05-14 10:30:11
LastEditTime: 2022-05-14 10:38:20
FilePath: \数值计算\draw2.py
Github: https://github.com/SugarSBN
これなに、これなに、これない、これなに、これなに、これなに、ねこ！ヾ(*´∀｀*)ﾉ
'''
import matplotlib.pyplot as plt
import numpy as np

x = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
x1 = [0, 1, 2]
y = [2.0,11.0,-106.0]
y1 = [2.0,3.0,2.142857142857143,2.8378378378378377,2.2469635627530367,2.7302873986735445,2.3217748374586518,2.6579016512723084,2.374994799783671,2.6087003707152228,2.4125837506412577]
y2 = [2.0,2.5555555555555554,2.5005500550055006,2.5000000550000006,2.5000000000000004,2.5,2.5,2.5,2.5,2.5,2.5]
plt.xlabel('n')
plt.ylabel('xk')
l1, = plt.plot(x1, y, 'g-', label = 'iter 1.')
l2, = plt.plot(x, y1, 'b-', label = 'iter 2.')
l3, = plt.plot(x, y2, 'y-', label = 'iter 3.')
plt.legend(handles=[l1,l2,l3],labels=['iter 1.', 'iter 2.', 'iter 3.'],loc='best')  
plt.show()  
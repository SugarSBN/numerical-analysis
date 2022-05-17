'''
Author: SuBonan
Date: 2022-05-15 10:55:43
LastEditTime: 2022-05-15 11:11:09
FilePath: \数值计算\draw5.py
Github: https://github.com/SugarSBN
これなに、これなに、これない、これなに、これなに、これなに、ねこ！ヾ(*´∀｀*)ﾉ
'''
import matplotlib.pyplot as plt
import numpy as np
x = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]
y = [0.0,0.10033467208545055,0.2027100355086725,0.3093362496096233,0.4227932187381618,0.5463024898437905,0.6841368083416924,0.8422883804630795,1.0296385570503641,1.2601582175503392,1.5574077246549023]
y1 = [0.0,0.1,0.201,0.30504010000000004,0.41434504626080104,0.5315132279968876,0.6597638591504549,0.8032926941345649,0.9678206093795617,1.1614882825735442,1.3963937856291082]
y2 = [0.0,0.10049999999999999,0.2030250625,0.30973849443055695,0.4231194820452056,0.5461963024262994,0.6828772399575627,0.838457263822664,1.0204416295825047,1.2399594370795366,1.5143301875868462]
y3 = [0.0,0.10033458907816413,0.2027098782317192,0.3093360393449778,0.4227929928541209,0.5463023075836336,0.6841367566530798,0.8422885693949728,1.0296390611155872,1.2601587827915834,1.557406442844996]


plt.subplot(2,2,1)
plt.xlabel('x')
plt.ylabel('y')
l1, = plt.plot(x, y, 'g-', label = 'y=tan(x)')
l2, = plt.plot(x, y1, 'r-', label = 'Euler Method')
l3, = plt.plot(x, y2, 'b-', label = "Corrected Euler Method")
l4, = plt.plot(x, y3, 'c-', label = "Runge Kutta Order=4")
plt.legend(handles=[l1, l2, l3, l4],labels=['y=tan(x)', 'Euler Method', 'Corrected Euler Method', 'Runge Kutta Order=4'],loc='best')  

plt.subplot(2,2,2)
plt.xlabel('x')
plt.ylabel('y')
l1, = plt.plot(x, y, 'g-', label = 'y=tan(x)')
l2, = plt.plot(x, y1, 'r-', label = 'Euler Method')
l3, = plt.plot(x, y2, 'b-', label = "Corrected Euler Method")
l4, = plt.plot(x, y3, 'c-', label = "Runge Kutta Order=4")
plt.legend(handles=[l1, l2, l3, l4],labels=['y=tan(x)', 'Euler Method', 'Corrected Euler Method', 'Runge Kutta Order=4'],loc='best')  

plt.subplot(2,2,3)
plt.xlabel('x')
plt.ylabel('y')
l1, = plt.plot(x, y, 'g-', label = 'y=tan(x)')
l2, = plt.plot(x, y1, 'r-', label = 'Euler Method')
l3, = plt.plot(x, y2, 'b-', label = "Corrected Euler Method")
l4, = plt.plot(x, y3, 'c-', label = "Runge Kutta Order=4")
plt.legend(handles=[l1, l2, l3, l4],labels=['y=tan(x)', 'Euler Method', 'Corrected Euler Method', 'Runge Kutta Order=4'],loc='best')  

plt.subplot(2,2,4)
plt.xlabel('x')
plt.ylabel('y')
l1, = plt.plot(x, y, 'g-', label = 'y=tan(x)')
l2, = plt.plot(x, y1, 'r-', label = 'Euler Method')
l3, = plt.plot(x, y2, 'b-', label = "Corrected Euler Method")
l4, = plt.plot(x, y3, 'c-', label = "Runge Kutta Order=4")
plt.legend(handles=[l1, l2, l3, l4],labels=['y=tan(x)', 'Euler Method', 'Corrected Euler Method', 'Runge Kutta Order=4'],loc='best')  


plt.show()  
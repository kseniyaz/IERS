import numpy as np
import math as m
import matplotlib.pyplot as plt

import numpy as np
 
coeff_x = []
with open("tab5_2a.txt") as f:
    for line in f:
        coeff_x.append([float(x) for x in line.split()])
coeff_y = []
with open("tab5_2b.txt") as f:
    for line in f:
        coeff_y.append([float(y) for y in line.split()])

print(len(coeff_x), len(coeff_y), coeff_y)

t = 0
dt = 0.00001
arg_x = 0
arg_y = 0
X = 0
Y = 0
Xm = []
Ym = []
while t <= 20:
	#формула 5.43 аргументы в радианах
	l_Moon = (134.96340251*3600 + 1717915923.2178 * t + 31.8792 * t**2 + 0.051635 * t**3 - 0.00024470 * t**4)*m.pi/(180*3600)
	l_Sun = (357.52910918*3600 + 129596581.0481 * t - 0.5532 * t**2 + 0.000136 * t**3 - 0.00001149 * t**4)*m.pi/(180*3600)
	F = (93.27209062*3600 + 1739527262.8478 * t - 12.7512 * t**2 - 0.001037 * t**3 + 0.00000417 * t**4)*m.pi/(180*3600)
	D = (297.85019547*3600 + 1602961601.2090 * t - 6.3706 * t**2 + 0.006593 * t**3 - 0.00003169 * t**4)*m.pi/(180*3600)
	Om = (125.04455501*3600 - 6962890.5431 * t + 7.4722 * t**2 + 0.007702 * t**3 - 0.00005939 * t**4)*m.pi/(180*3600)
	for i in range (1, len(coeff_x) - 1):
		arg_x += l_Moon*coeff_x[i][3] + l_Sun*coeff_x[i][4] + F*coeff_x[i][5] + D*coeff_x[i][6] + Om*coeff_x[i][7]
	for j in range (0, len(coeff_y)):
		arg_y += l_Moon*coeff_y[j][3] + l_Sun*coeff_y[j][4] + F*coeff_y[j][5] + D*coeff_y[j][6] + Om*coeff_y[j][7]
	#формула 5.16
	for i in range(1, len(coeff_x) - 1):
		X += coeff_x[i][1] * m.sin(arg_x) + coeff_x[i][2] * m.cos(arg_x)
	for j in range(0, len(coeff_y)):
		Y += coeff_y[j][1] * m.sin(arg_y) + coeff_y[j][2] * m.cos(arg_y)
	#массив точек X и Y
	Xm.append(X)
	Ym.append(Y)
	t += dt

plt.figure()
plt.plot(Xm, Ym, 'ro')
plt.xlabel('X')
plt.ylabel('Y')
plt.show()
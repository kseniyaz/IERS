import numpy as np
import math as m
 
coeff_x = []
with open("tab5_2a.new.txt") as f:
    for line in f:
        coeff_x.append([float(x) for x in line.split()])
coeff_y = []
with open("tab5_2b.new.txt") as f:
    for line in f:
        coeff_y.append([float(y) for y in line.split()])

tt = 2451545 #JD
dt = 1
Xm = []
Ym = []
time = []
arg_xm = []
while tt <= 2458845:
	X = 0
	Y = 0
	#формула 5.43 аргументы в радианах
	t = (tt - 2451545)/36525
	lambda_moon = (218.31664563*3600 + 1732564372.3047 * t - 5.279 * t**2 + 0.006665 * t**3 - 0.00005522 * t**4)*m.pi/(180*3600)
	lambda_sun = (-79.53354984*3600 + 129602771.0957 * t + 1.0916 * t**2 + 0.000072 * t**3 - 0.00002353 * t**4)*m.pi/(180*3600)
	Om = (125.04455501*3600 - 6962890.5431 * t + 7.4722 * t**2 + 0.007702 * t**3 - 0.00005939 * t**4)*m.pi/(180*3600)
	w_moon = (-41.69131189*3600 + 21611339.6300001 * t - 44.6304 * t**2 - 0.052672 * t**3 + 0.00024887 * t**4)*m.pi/(180*3600)
	w_sun = (-437.06265902*3600 + 6190.04759991 * t + 1.6448 * t**2 -0.000064 * t**3 -0.00001204 * t**4)*m.pi/(180*3600)
	#формула 5.16
	for i in range(0, len(coeff_x)):
		arg_x = lambda_moon*coeff_x[i][3] + lambda_sun*coeff_x[i][4] + Om*coeff_x[i][5] + w_moon*coeff_x[i][6] + w_sun*coeff_x[i][7]
		X += coeff_x[i][1] * m.sin(arg_x) + coeff_x[i][2] * m.cos(arg_x)
		if t == 0:
			arg_xm.append(arg_x)
	for j in range(0, len(coeff_y)):
		arg_y = lambda_moon*coeff_y[j][3] + lambda_sun*coeff_y[j][4] + Om*coeff_y[j][5] + w_moon*coeff_y[j][6] + w_sun*coeff_y[j][7]
		Y += coeff_y[j][1] * m.sin(arg_y) + coeff_y[j][2] * m.cos(arg_y)
	#массив точек X и Y
	Xm.append(X/1000000)
	Ym.append(Y/1000000)
	time.append(t)
	tt += dt


f = open("nutation.new.txt", 'w')
for i in range(len(Xm)):
	f.write(str(time[i]) + ' ' + str(Xm[i])+ ' ' +str(Ym[i]) + '\n')
f.close()

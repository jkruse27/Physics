import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# ---- Constants ---- #

dx = 0.1 
dt = 0.001

L = float(input("Size: "))   

print("""Pyrolytic graphite, parallel to layers 	1.22e-3
Silver, pure (99.9%) 	1.6563e-4
Gold 	1.27e-4
Copper at 25 °C 	1.11e-4
Aluminium 	9.7e-5 
Al-10Si-Mn-Mg (Silafont 36) at 20 °C 	74.2e-6 
Aluminium 6061-T6 Alloy 	6.4e-5 
Al-5Mg-2Si-Mn (Magsimal-59) at 20 °C 	4.4e-5 
Steel, AISI 1010 (0.1% carbon) 	1.88 x 10**-5
Steel, 1% carbon 	1.172e-5
Steel, stainless 304A at 27 °C 	4.2e-6 
Steel, stainless 310 at 25 °C 	3.352e-6 
Inconel 600 at 25 °C 	3.428e-6 
Molybdenum (99.95%) at 25 °C 	54.3e-6 
Iron 	2.3e-5 
Silicon 	8.8e-5 
Quartz 	1.4e-6 
Carbon/carbon composite at 25 °C 	2.165e-4 
Aluminium oxide (polycrystalline) 	1.20e-5 
Silicon Dioxide (Polycrystalline) 	8.3e-7 
Si3 N4 with CNTs 26 °C 	9.142e-6 
Si3 N4 without CNTs 26 °C 	8.605e-6 
PC (Polycarbonate) at 25 °C 	1.44e-7 
PP (Polypropylene) at 25 °C 	9.6e-8 
Paraffin at 25 °C 	8.1e-8 
PVC (Polyvinyl Chloride) 	8e-8
PTFE (Polytetrafluorethylene) at 25 °C 	0.124e-6 
Water at 25 °C 	1.43e-7 
Alcohol 	7e-8 
Water vapour (1 atm, 400 K) 	2.338e-5
Air (300 K) 	1.9e-5 
Argon (300 K, 1 atm) 	2.2*10**-5
Helium (300 K, 1 atm) 	1.9*10**-4
Hydrogen (300 K, 1 atm) 	1.6*10**-4
Nitrogen (300 K, 1 atm) 	2.2*10**-5
Pyrolytic graphite, normal to layers 	3.6e-6
Sandstone 	1.12–1.19e-6
Tin 	4.0e-5
Brick, common 	5.2e-7
Brick, adobe 	2.7e-7
Glass, window 	3.4e-7
Rubber  1.3e-7
Nylon 	9e-8
Wood (Yellow Pine) 	8.2e-8
Oil, engine (saturated liquid, 100 °C) 	7.38*10**-8\n\n""")

a = float(input("Thermal diffusivity: ")) 


# ---- Initial Conditions ---- #

center  = float(input("Center: "))
i_t     = float(input("Temperature at center: "))


def u0(x, center, i_t):
    return np.exp(-((x/10)-center)**2)*i_t

x = [u0(i, center, i_t) for i in range(int(L/dx))]

# Iterations #

x_max = len(x)-1
u      = [i for i in x]     # Temperatures
next_u = [i for i in x]     # Next Temperatures

k = (a*a*dt)/(dx*dx)

fig, ax = plt.subplots()
interval = np.arange(0, L/dx)
zero = [0 for i in interval]
line, = ax.plot(interval, x)
ax.set_xlabel("Position (cm)")
ax.set_ylabel("Temperature (K)")

def animate(i):
    global next_u
    global u
    for j in range(1, x_max):
        next_u[j] = k*(u[j-1]+u[j+1]-2*u[j]) + u[j]
    next_u[0] = next_u[1]
    next_u[x_max] = next_u[x_max-1]
    line.set_ydata(next_u)
    u = [i for i in next_u]
    return line,

ani = animation.FuncAnimation(fig, animate, interval=1)

plt.show()

from matplotlib import pyplot as plt
import numpy as np
import mpl_toolkits.mplot3d.axes3d as p3
from matplotlib import animation

points = {}
names = []
Sun = ['Sun']
Inner_Planets = ['Earth', 'Venus', 'Mars', 'Mercury']
Outer_Planets = ['Jupiter', 'Saturn', 'Uranus', 'Neptune']
planets = Sun+Inner_Planets

with open("simulation.txt", "r") as f:    
    for i in f:
        line = i.split()
        if(len(line) == 4):
            if(line[0] not in names):
                if(line[0] in planets):
                    names.append(line[0])
            if(line[0] not in points):
                points[line[0]] = [[float(line[1])],[float(line[2])],[float(line[3])]]
            else:
                points[line[0]][0].append(float(line[1]))
                points[line[0]][1].append(float(line[2]))
                points[line[0]][2].append(float(line[3]))
                
points_in_time = []
                
for i, j in enumerate(points["Sun"][0]):
    u,v,w = [],[],[]
    for k in points:
        if(k in planets):
            u.append(points[k][0][i])
            v.append(points[k][1][i])
            w.append(points[k][2][i])
    points_in_time.append([np.array(u),np.array(v),np.array(w)])

x_max = max([max(i[0]) for i in points_in_time])
y_max = max([max(i[1]) for i in points_in_time])
z_max = max([max(i[2]) for i in points_in_time])

x_min = min([min(i[0]) for i in points_in_time])
y_min = min([min(i[1]) for i in points_in_time])
z_min = min([min(i[2]) for i in points_in_time])

fig = plt.figure()
ax = p3.Axes3D(fig)

for i in names:
   orbits = ax.plot(np.array(points[i][0]), np.array(points[i][1]), np.array(points[i][2])) 

for i, j in enumerate(names):
    ax.text(points_in_time[0][0][i],points_in_time[0][1][i],points_in_time[0][2][i], j, None)

ax.set_xlim3d(min([x_min, y_min,z_min]), max([x_max, y_max, z_max]))
ax.set_ylim3d(min([x_min, y_min,z_min]), max([x_max, y_max, z_max]))
ax.set_zlim3d(min([x_min, y_min,z_min]), max([x_max, y_max, z_max]))

Writer = animation.writers['ffmpeg']
writer = Writer(fps=24, metadata=dict(artist='Me'), bitrate=1800)

point, = ax.plot(points_in_time[0][0], points_in_time[0][1], points_in_time[0][2], '*')
txt = fig.suptitle('')

def update_points(num, p, points, names, ax):

    print(num)
    date = "Day: "+str(num)


    new_x = p[num][0]
    new_y = p[num][1]
    new_z = p[num][2]

    points.set_data(new_x,new_y)
    points.set_3d_properties(new_z, 'z')
    
    for i in ax.texts:
        i.remove()

    ax.text2D(0.05, 0.95, date, transform=ax.transAxes)
    for i, j in enumerate(names):
        ax.text(new_x[i],new_y[i],new_z[i], j, None)

    return points, txt

ani=animation.FuncAnimation(fig, update_points, frames=len(points_in_time),blit=True, fargs=(points_in_time, point, names, ax))

ani.save('simulation.mp4', writer=writer)

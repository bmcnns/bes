import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

df = pd.read_csv("../experiments/results.csv", header=None, names=['genotype','support','confidence'])

fig, ax = plt.subplots()

ax.set_xlim(0, 1000)
ax.set_ylim(0, 6000)
scatter = ax.scatter([], [])

def init():
    scatter.set_offsets(np.c_[[], []])
    return scatter,

def animate(frame):
    x = df['

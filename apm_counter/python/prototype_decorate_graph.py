import pandas as pd
import matplotlib
import matplotlib.lines as mpl_lines
import matplotlib.text as mpl_text
import matplotlib.pyplot as plt

print(matplotlib.get_backend())
df = pd.DataFrame(data=[[1,2,3],[4,5,6],[7,8,9]],
                  index=[10,20,30],
                  columns=['a','b','c'])
axes = df.plot()
vert_line = mpl_lines.Line2D([20,20], [0,10], lw=2, color='black', axes=axes)
label = mpl_text.Annotation('vert_line', xy=(20.2, 2))
axes.add_artist(vert_line)
axes.add_artist(label)
# Does not stop script
#axes.figure.show()
plt.show()


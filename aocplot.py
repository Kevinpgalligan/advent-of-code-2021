import matplotlib.pyplot as plt
import sys

def best_fit(X, Y):
    """
    Stolen from: https://stackoverflow.com/questions/22239691/code-for-best-fit-straight-line-of-a-scatter-plot-in-python
    """
    xbar = sum(X)/len(X)
    ybar = sum(Y)/len(Y)
    n = len(X) # or len(Y)

    numer = sum([xi*yi for xi,yi in zip(X, Y)]) - n * xbar * ybar
    denum = sum([xi**2 for xi in X]) - n * xbar**2

    b = numer / denum
    a = ybar - b * xbar

    return a, b

xs = []
ys = []
for line in sys.stdin:
    day, linecount = map(int, line.split(" "))
    xs.append(day)
    ys.append(linecount)

plt.plot(xs, ys, marker="o", linestyle="none")
plt.xlabel("day")
plt.ylabel("lines of code")
plt.ylim(bottom=0)
plt.grid(linestyle="--")

a, b = best_fit(xs, ys)
yfit = [a + b*x for x in xs]
plt.plot(xs, yfit, color="red")

plt.show()

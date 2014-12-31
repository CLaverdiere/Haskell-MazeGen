from random import shuffle
import Image, ImageDraw, math
import sys

# Draws maze given coordinates of 4-tuple format:
# [(x1, y1, x2, y2), ...]

w, h = eval(sys.argv[1])
coords = eval(sys.argv[2])

dim = 512
scale = dim / ((w+h) / 2.)

img = Image.new("RGB", (dim, dim))
draw = ImageDraw.Draw(img)

# Use a somewhat random color scheme.
a = lambda x, y: 100
b = lambda x, y: 200
c = lambda x, y: int(x + y) / ((dim / 256) * 2)
cs = [a,b,c]
shuffle(cs)

for coord in coords:
    x1, y1, x2, y2 = [x * scale for x in coord]
    draw.line((x1, y1, x2, y2), width=3,
              fill="rgb({}, {}, {})".format(
                  cs[0](x1,y1), cs[1](x1,y1), cs[2](x1,y1))
              )

del draw
img.save("maze.png");

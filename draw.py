import Image, ImageDraw, math
import sys

# Draws maze given dimensions and coordinates of 4-tuple format:
# (width, height) [(x1, y1, x2, y2), ...]

w, h = eval(sys.argv[1])
coords = eval(sys.argv[2])

dim = 512
scale = dim / ((w+h) / 2.)

img = Image.new("RGB", (dim, dim))
draw = ImageDraw.Draw(img)

r = lambda x, y: int(x + y) / ((dim / 256) * 2)

for coord in coords:
    x1, y1, x2, y2 = [x * scale for x in coord]
    draw.line((x1, y1, x2, y2), width=3,
              fill="rgb({}, 100, 200)".format(r(x1,y1)))

del draw
img.save("maze.png");

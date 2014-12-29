import Image, ImageDraw, math
import sys

# Draws maze given coordinates of 4-tuple format:
# [(x1, y1, x2, y2), ...]

data = eval(sys.argv[1])
dim = 512
scale = dim / 10

img = Image.new("RGB", (dim, dim))
draw = ImageDraw.Draw(img)

for coord in data:
    x1, y1, x2, y2 = [x * scale for x in coord]
    draw.line((x1, y1, x2, y2), width=3, fill="rgb(0, 100, 200)")

del draw
img.save("maze.png");

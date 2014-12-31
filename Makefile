maze.png: data.txt draw.py
	python draw.py `cat data.txt`

data.txt: MazeGen.hs
	runhaskell MazeGen.hs > data.txt

all: maze.png

clean:
	rm data.txt
	rm maze.png

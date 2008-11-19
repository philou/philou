import string
import copy
import random

def allEqual(items):
	if 0 == len(items):
		return True

	first = items[0]
	for item in items:
		if not first == item:
			return False
	return True

class Coord:

	@staticmethod
	def colToLetter(col):
		if col == 0:
			return 'A'
		elif col == 1:
			return 'B'
		elif col == 2:
			return 'C'

		assert False

	@staticmethod
	def letterToCol(letter):
		if letter == 'A':
			return 0
		elif letter == 'B':
			return 1
		elif letter == 'C':
			return 2

		assert False

	def __init__(self, col, row):
		self.row = row
		self.col = col

	@staticmethod
	def initFromString(string):
		return Coord(Coord.letterToCol(string[0]), int(string[1])-1)
		
	def __str__(self):
		return Coord.colToLetter(self.col) + str(self.row+1)

class Game:

	Size = 3
	Empty = ' '
	X = 'x'
	O = 'o'
	Draw = 'nobody'

	@staticmethod
	def otherPlayer(player):
		if player == Game.X:
			return Game.O
		else:
			return Game.X
	
	def __init__(self):
		self.grid = [[Game.Empty for col in range(0,Game.Size)] for row in range(0,Game.Size)]

	def __str__(self):
		return " ABC\n" + string.join([str(row+1) + string.join(self.grid[row],'') for row in range(0,Game.Size)], "\n")
	
	@staticmethod
	def isValidCoord(coord):
		return 0 <= coord.col and coord.col < Game.Size and 0 <= coord.row and coord.row < Game.Size

	def getAt(self,coord):
		assert Game.isValidCoord(coord)
		return self.grid[coord.row][coord.col]
	
	def check(self, coord, value):
		assert Game.isValidCoord(coord)
		assert Game.Empty == self.getAt(coord)
		
		result = copy.deepcopy(self)
		result.grid[coord.row][coord.col] = value
		return result
		
	def isWonBy(self, value):
		for row in range(0,Game.Size):
			if allEqual([value, self.getAt(Coord(0,row)), self.getAt(Coord(1,row)), self.getAt(Coord(2,row))]):
				return True
		for col in range(0,Game.Size):
			if allEqual([value, self.getAt(Coord(col,0)), self.getAt(Coord(col,1)), self.getAt(Coord(col,2))]):
				return True
		if allEqual([value, self.getAt(Coord(0,0)), self.getAt(Coord(1,1)), self.getAt(Coord(2,2))]):
			return True
		if allEqual([value, self.getAt(Coord(2,0)), self.getAt(Coord(1,1)), self.getAt(Coord(0,2))]):
			return True
		return False
		
	def winner(self):
		if self.isWonBy(Game.X):
			return Game.X
		elif self.isWonBy(Game.O):
			return Game.O
		elif not self.hasEmptyCells():
			return Game.Draw
		else:
			return False

	def hasEmptyCells(self):
		for cell in self.emptyCells():
			return True
		return False
			
	def emptyCells(self):
		for row in range(0,Game.Size):
			for col in range(0,Game.Size):
				if Game.Empty == self.getAt(Coord(col, row)):
					yield Coord(col, row)
				
				
		

def killerMove(game, player):
	assert not game.winner()

	for move in game.emptyCells():
		nextGame = game.check(move, player)
		if looses(nextGame, Game.otherPlayer(player)):
			return move

	return False

def looses(game, player):
	winner = game.winner()
	if not winner:
                for move in game.emptyCells():
                        nextGame = game.check(move, player)
                        if killerMove(nextGame, Game.otherPlayer(player)):
                                return True
                return False
	else:
		return Game.otherPlayer(player) == game.winner()

def bestMove(game, player):
	assert not game.winner()

	kMove = killerMove(game, player)
	if kMove:
		return kMove

	legalMoves = [move for move in game.emptyCells()]
	print map(str, legalMoves)
		
	def isNotDumbMove(move):
		return not killerMove(game.check(move, player), Game.otherPlayer(player))
	moves = filter(isNotDumbMove, legalMoves)
	print moves
	if 0 < len(moves):
		return random.choice(moves)
		
	return random.choice(legalMoves)

def human(game):
	print game
	try:
		humanMove = Coord.initFromString(raw_input("Please check a cell (Xn):"))
		return game.check(humanMove, Game.X)
	except AssertionError:
		print "Incorrect input ..."
		return human(game)

def ai(game):
	aiMove = bestMove(game, Game.O)
	print "Os played " + str(aiMove) + " :"
	return game.check(aiMove, Game.O)

def play():
	game = Game()
	print "The game starts :"
	while( not game.winner()):
		game = human(game)
		if not game.winner():
			game = ai(game)
	print game
	print "The game is finished, the winner is " + game.winner()

def test():
	assert allEqual(['a','a','a'])
	assert not allEqual(['a','x','x'])

	assert Game.otherPlayer(Game.X) == Game.O
	assert Game.otherPlayer(Game.O) == Game.X
	
	assert Coord(1,2)
	assert not False
	
	game = Game()
	assert not game.isWonBy(Game.X)
	game1 = game.check(Coord(0,0),Game.X).check(Coord(0,1),Game.X).check(Coord(0,2),Game.X)
	assert game1.isWonBy(Game.X)

test()

play()

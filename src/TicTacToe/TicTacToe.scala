object FieldState extends Enumeration {
	type FieldState = Value
	val Empty = Value("Empty")
	val Player = Value("Player")
	val Computer = Value("Computer")
}



// Model for a single field on a board.
class FieldModel(val row: Int, val column: Int) {
	private var fieldState = FieldState.Empty

	def getFieldState(): FieldState.FieldState = {
		return fieldState
	}

	def setFieldState(newFieldState: FieldState.FieldState) {
		fieldState = newFieldState
	}
}



// Model for a complete Tic Tac Toe board.
class BoardModel(val width: Int, val height: Int) {
	// All fields on the board as a matrix of rows containing columns.
	private var fields = Array.ofDim[FieldModel](height, width)

	// Initialize all fields.
	for (row <- 0 to height - 1) {
		for (col <- 0 to width - 1) {
			fields(row)(col) = new FieldModel(row, col)
		}
	}

	// Iterates all fields in order from top left to bottom right.
	def foreachField(iterator: (FieldModel) => Unit): Unit = {
		for (row <- fields) {
			for (field <- row) {
				iterator(field)
			}
		}
	}

	// Returns all fields on the board as an array.
	// The array is not guaranteed to be sorted, but it could be.
	def getFieldsAsArray(): Array[FieldModel] = {
		var array = new Array[FieldModel](height * width)
		for (row <- 0 to height - 1) {
			for (col <- 0 to width - 1) {
				array((row * width) + col) = fields(row)(col)
			}
		}
		return array
	}

	// Returns the field at a certain row and column.
	def getFieldAt(row: Int, column: Int): FieldModel = fields(row)(column)

	// Returns all fields that haven't been seized yet.
	def getEmptyFields(): List[FieldModel] = {
		return getFieldsAsArray()
			.toList
			.filter(field => field.getFieldState() == FieldState.Empty)
	}
}



// Renders a BoardModel as a string.
class StringBoardView {
	def render(model: BoardModel): Unit = {
		var lines = Array.ofDim[String](model.height, model.width)
		model.foreachField(field => {
			lines(field.row)(field.column) = fieldStateToString(field.getFieldState())
		})
		var string = "  "
		for (col <- 1 to model.width) {
			string += s"$col "
		}
		string += "\n"
		for (row <- 0 to model.height - 1) {
			val line = lines(row)
			string += s"${row + 1} ${line.mkString(" ")}\n"
		}
		print(string)
	}


	private def fieldStateToString(fieldState: FieldState.FieldState): String = fieldState match {
		case FieldState.Empty => "-"
		case FieldState.Player => "o"
		case FieldState.Computer => "x"
	}
}



abstract class Player(game: Game) {
	def getPlayerName(): String
	def getPlayerFieldState(): FieldState.FieldState

	// Play a single turn.
	def playTurn(): Unit = {
		val (row, column) = playTurnConcrete()
		val field = game.getBoardModel().getFieldAt(row, column)
		if (wasFieldAlreadySeized(row, column))
			throw new Error("illegal attempt to override field that has already been set")
		field.setFieldState(getPlayerFieldState)
	}

	// Play a single turn and return the coordinates of the field to be seized by this player as a
	// tuple of zero-based integers (row, column).
	def playTurnConcrete(): (Int, Int)

	// Checks if a field was already seized by any player.
	protected def wasFieldAlreadySeized(row: Int, column: Int): Boolean = {
		return game.getBoardModel().getFieldAt(row, column).getFieldState() != FieldState.Empty
	}
}



abstract class Game(
	val boardWidth: Int,
	val boardHeight: Int,
	val winningSeries: Int,
	var boardView: StringBoardView
) {
	// Abstract Methods

	def createPlayer1(): Player
	def createPlayer2(): Player

	// Initialization

	private val player1 = createPlayer1()
	private val player2 = createPlayer2()

	// The player that is about to or has just made his turn.
	// We initialize this with a random player.
	private var currentPlayer = if (scala.util.Random.nextInt(2) == 1) player1 else player2

	private val board = new BoardModel(boardWidth, boardHeight)


	// Getters / Setters

	def getBoardModel(): BoardModel = board


	// Methods

	// Starts the game. When this method returns, the game has either determined a winner or it has determined that
	// there are no more moves that can be played.
	def start(): Unit = {
		do {
			alternatePlayer()
			// print("\033[H\033[2J")
			println(s"\n--- It's ${currentPlayer.getPlayerName()}'s turn ---".toUpperCase)
			currentPlayer.playTurn()
			boardView.render(getBoardModel())
		} while(!gameHasEnded())
	}


	// Check whether the game has ended (for any reason).
	def gameHasEnded(): Boolean = {
		return (
			doesAnyPlayerHaveWinningSeries() ||
			!doesBoardHaveEmptyFields()
		)
	}


	// Checks if there's one or more empty fields on the stage.
	def doesBoardHaveEmptyFields(): Boolean = {
		return getBoardModel()
			.getEmptyFields()
			.map(field => field.getFieldState() == FieldState.Empty)
			.contains(true)
	}


	// Check if any of the players has a winning series of fields.
	def doesAnyPlayerHaveWinningSeries(): Boolean = {
		return (
			doesPlayerHaveWinningSeries(player1) ||
			doesPlayerHaveWinningSeries(player2)
		)
	}


	// Check if a player has a winning series.
	def doesPlayerHaveWinningSeries(player: Player): Boolean = {
		return false
	}


	private def alternatePlayer(): Unit = {
		if (player1 == currentPlayer) {
			currentPlayer = player2
		} else {
			currentPlayer = player1
		}
	}
}



class RandomComputerPlayer(game: Game) extends Player(game) {
	def getPlayerName(): String = "the computer"
	def getPlayerFieldState(): FieldState.FieldState = FieldState.Computer

	// Play a single turn and return the coordinates of the field to be seized by this player as a
	// tuple of zero-based integers (row, column).
	def playTurnConcrete(): (Int, Int) = {
		val emptyFields = game.getBoardModel().getEmptyFields() 
		val index = scala.util.Random.nextInt(emptyFields.length - 1)
		val field = emptyFields(index);
		return (field.row, field.column)
	}
}



class TerminalInputPlayer(game: Game) extends Player(game) {
	def getPlayerName(): String = "Player 1"
	def getPlayerFieldState(): FieldState.FieldState = FieldState.Player

	// Play a single turn and return the coordinates of the field to be seized by this player as a
	// tuple of zero-based integers (row, column).
	def playTurnConcrete(): (Int, Int) = {
		val row = requestNumberFromUser(
			"Enter a row: ",
			num => num >= 1 && num <= game.boardHeight
		)
		val column = requestNumberFromUser(
			"Enter a column: ",
			num => num >= 1 && num <= game.boardWidth && !wasFieldAlreadySeized(row, num)
		)
		return (row - 1, column - 1)
	}

	def requestNumberFromUser(promptText: String, isValid: (Int) => Boolean): Int = {
		print(promptText)
		var input: Int = 0
		try {
			input = readInt()
			if (!isValid(input))
				throw new Exception()
		} catch {
			case e: Exception => {
				println("Please enter a valid number.")
				return requestNumberFromUser(promptText, isValid)
			}
		}
		return input
	}
}



object TicTacToe extends Game(
	3, 3, 3,
	new StringBoardView()
) {
	def createPlayer1(): Player = {
		return new TerminalInputPlayer(this)
	}

	def createPlayer2(): Player = {
		return new RandomComputerPlayer(this)
	}

	def main(args: Array[String]): Unit = {
		start()
	}
}

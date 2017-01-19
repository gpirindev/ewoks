// Editor.scala
// Copyright (c) 2015 J. M. Spivey

import Undoable.Change

/** The editor state extended with methods for editor commands. */
class Editor extends Undoable[Editor.Action] {
    /** The buffer being edited. */
    private val ed = new EdBuffer

    /** The display. */
    private var display: Display = null
    
    /** Whether the command loop should continue */
    private var alive = true
    
    /** Show the buffer on a specified display */
    def activate(display: Display) {
        this.display = display
        display.show(ed)
        ed.register(display)
        ed.initDisplay()
    }

    /** Ask for confirmation if the buffer is not clean */
    def checkClean(action: String) = {
        if (!ed.isModified && ed.atSave())
            true
        else {
            val question = 
                "Buffer modified -- really %s?".format(action)
            MiniBuffer.ask(display, question)
        }
    }

    /** Load a file into the buffer */
    def loadFile(fname: String) { ed.loadFile(fname) }

    /** Command: Place the mark */
    def placeMark(): Unit = {
        ed.mark = ed.point
    }

    /** Command: Swap the mark and the point*/
    def swapMarkPoint(): Unit = {
        val temp = ed.mark
        ed.mark = ed.point
        ed.point = temp
    }

    /** Command: Move the cursor in the specified direction */
    def moveCommand(dir: Int) {
        var p = ed.point
        val row = ed.getRow(p)

        dir match {
            case Editor.LEFT => 
                if (p > 0) p -= 1
            case Editor.RIGHT =>
                if (p < ed.length) p += 1
            case Editor.UP =>
                p = ed.getPos(row-1, goalColumn())
            case Editor.DOWN =>
                p = ed.getPos(row+1, goalColumn())
            case Editor.HOME =>
                p = ed.getPos(row, 0)
            case Editor.END =>
                p = ed.getPos(row, ed.getLineLength(row)-1)
            case Editor.PAGEDOWN =>
                p = ed.getPos(row + Editor.SCROLL, 0)
                display.scroll(+Editor.SCROLL)
            case Editor.PAGEUP =>
                p = ed.getPos(row - Editor.SCROLL, 0)
                display.scroll(-Editor.SCROLL)
            case Editor.CTRLHOME =>
                p = ed.getPos(0, 0)
            case Editor.CTRLEND =>
                p = ed.getPos(ed.numLines-1, ed.getLineLength(ed.numLines-1)-1)
            case _ =>
                throw new Error("Bad direction")
        }

        ed.point = p
    }

    /** Command: Insert a character */
    def insertCommand(ch: Char): Change = {
        val p = ed.point
        val time = ed.timestamp
        ed.insert(p, ch)
        ed.point = p+1
        if(ed.mark >= p) ed.mark = ed.mark + 1
        new ed.AmalgInsertion(p, ch, time)
    }
    
    /** Command: Transpose the characters to the left and right of the cursor */
    def transposeCommand(): Change = {
        val p = ed.point
        if (ed.getColumn(p) == 0 || p == ed.length) { beep(); return null }
        ed.transpose(p)
        ed.point = p+1
        if (ed.charAt(p) == '\n') { ed.point += 1}
        new ed.Transposition(p)
    }
    
    /** Command: Delete in a specified direction */
    def deleteCommand(dir: Int): Change = {
        var p = ed.point
        var mk = ed.mark
        var ch: Char = 0

        dir match {
            case Editor.LEFT =>
                if (p == 0) { beep(); return null }
                p -= 1
                ch = ed.charAt(p)
                ed.deleteChar(p)
                ed.point = p
                if(mk >= p) ed.mark = mk - 1
            case Editor.RIGHT =>
                if (p == ed.length) { beep(); return null }
                ch = ed.charAt(p)
                ed.deleteChar(p)
                if(mk > p) ed.mark = mk - 1
            case Editor.END =>
                if (p == ed.length) { beep(); return null }
                if (ed.charAt(p) == '\n') {
                  ch = ed.charAt(p)
                  ed.deleteChar(p)
                }
                else {
                  var range = ed.getRange(p, ed.getLineLength(ed.getRow(p)) - ed.getColumn(p) - 1)
                  ed.deleteRange(p, ed.getLineLength(ed.getRow(p)) - ed.getColumn(p) - 1)
                  return new ed.Deletion(p, range)
                }
            case _ =>
                throw new Error("Bad direction")
        }

        new ed.Deletion(p, ch)
    }

    /** Uppercase the letter in the word at the current position*/
    def uppercaseCommand(): Change = {
        var p = ed.point
        if(p == ed.length || !ed.letterOrNum(p)) { beep(); return null }
        var start = ed.startOfWord(p)
        var len = ed.lengthOfWord(p)
        var range = ed.getRange(start, len).toString()
        ed.replace(start, range.toUpperCase)
        new ed.Replacement(start, range)
    }

    /** The last searched word */
    var lastSearched = ""
    
    /** Search for a word */
    def searchCommand() {
        var p = ed.point
        val elem = MiniBuffer.readString(display, "Search", lastSearched)
        if (elem != null && elem.length > 0) {
          lastSearched = elem
          var index = ed.search(elem, p)
          if(index > 0) ed.point = index
          else beep();
        }        
    }
    
    /** Command: Save the file */
    def saveFileCommand(): Change = {
        val name = 
            MiniBuffer.readString(display, "Write file", ed.filename)
        if (name != null && name.length > 0)
            ed.saveFile(name)
        if (ed.timestamp > 0) return new ed.Save()
        return null
    }

    /** Prompt for a file to read into the buffer.  */
    def replaceFileCommand() {
        if (! checkClean("overwrite")) return
        val name = 
            MiniBuffer.readString(display, "Read file", ed.filename)
        if (name != null && name.length > 0) {
            ed.point = 0
            ed.loadFile(name)
            ed.initDisplay()
            reset()
        }
    }

    /** Command: recenter and rewrite the display */
    def chooseOrigin() { 
        display.chooseOrigin() 
        ed.forceRewrite()
    }
    
    /** Quit, after asking about modified buffer */
    def quit() {
        if (checkClean("quit")) alive = false
    }


    // Command execution protocol
    
    /** Goal column for vertical motion. */
    private var goal = -1
    private var prevgoal = 0
    
    /** Execute a command, wrapping it in actions common to all commands */
    def obey(cmd: Editor.Action): Change = {
        prevgoal = goal; goal = -1
        display.setMessage(null)
        val before = ed.getState()
        val change = cmd(this)
        val after = ed.getState()
        ed.update()
        ed.wrapChange(before, change, after)
    }
    
    /** The desired column for the cursor after an UP or DOWN motion */
    private def goalColumn() = {        
        /* Successive UP and DOWN commands share the same goal column,
         * but other commands cause it to be reset to the current column */
        if (goal < 0) {
            val p = ed.point
            goal = if (prevgoal >= 0) prevgoal else ed.getColumn(p)
        }
        
        goal
    }

    /** Beep */
    def beep() { display.beep() }

    /** Read keystrokes and execute commands */
    def commandLoop() {
        activate(display)

        while (alive) {
            val key = display.getKey()
            Editor.keymap.find(key) match {
                case Some(cmd) => { perform(cmd)}
                case None => beep()
            }
        }
    }
}

object Editor {
    /** Direction for use as argument to moveCommand or deleteCommand. */
    val LEFT = 1
    val RIGHT = 2
    val UP = 3
    val DOWN = 4
    val HOME = 5
    val END = 6
    val PAGEUP = 7
    val PAGEDOWN = 8
    val CTRLHOME = 9
    val CTRLEND = 10
    
    /** Amount to scroll the screen for PAGEUP and PAGEDOWN */
    val SCROLL = Display.HEIGHT - 3

    /** Possible value for damage. */
    val CLEAN = 0
    val REWRITE_LINE = 1
    val REWRITE = 2

    /** Main program for the entire Ewoks application. */
    def main(args: Array[String]) {
        if (args.length > 1) {
            Console.err.println("Usage: ewoks [file]")
            scala.sys.exit(2)
        }

        val terminal = new Terminal("EWOKS")
        terminal.activate()
        val app = new Editor()
        val display = new Display(terminal)
        app.activate(display)
        if (args.length > 0) app.loadFile(args(0))
        app.commandLoop()
        scala.sys.exit(0)
    }

    /** Keymap for editor commands */
    type Action = (Editor => Change)

    // This implicit conversion allows methods that return Unit to
    // be used as commands, not contributing to the undo history
    import scala.language.implicitConversions
    implicit def fixup(v: Unit): Change = null

    val keymap = Keymap[Action](
        Display.RETURN -> (_.insertCommand('\n')),
        Display.RIGHT -> (_.moveCommand(RIGHT)),
        Display.LEFT -> (_.moveCommand(LEFT)),
        Display.UP -> (_.moveCommand(UP)),
        Display.DOWN -> (_.moveCommand(DOWN)),
        Display.HOME -> (_.moveCommand(HOME)),
        Display.END -> (_.moveCommand(END)),
        Display.PAGEUP -> (_.moveCommand(PAGEUP)),
        Display.PAGEDOWN -> (_.moveCommand(PAGEDOWN)),
        Display.CTRLHOME -> (_.moveCommand(CTRLHOME)),
        Display.CTRLEND -> (_.moveCommand(CTRLEND)),
        Display.ctrl('?') -> (_.deleteCommand(LEFT)),
        Display.DEL -> (_.deleteCommand(RIGHT)),
        Display.ctrl('A') -> (_.moveCommand(HOME)),
        Display.ctrl('B') -> (_.moveCommand(LEFT)),
        Display.ctrl('D') -> (_.deleteCommand(RIGHT)),
        Display.ctrl('E') -> (_.moveCommand(END)),
        Display.ctrl('F') -> (_.moveCommand(RIGHT)),
        Display.ctrl('G') -> (_.beep),
        Display.ctrl('K') -> (_.deleteCommand(END)),
        Display.ctrl('L') -> (_.chooseOrigin),
        Display.ctrl('M') -> (_.placeMark),
        Display.ctrl('N') -> (_.moveCommand(DOWN)),
        Display.ctrl('O') -> (_.swapMarkPoint),
        Display.ctrl('P') -> (_.moveCommand(UP)),
        Display.ctrl('Q') -> (_.quit),
        Display.ctrl('R') -> (_.replaceFileCommand),
        Display.ctrl('S') -> (_.searchCommand()),
        Display.ctrl('T') -> (_.transposeCommand),
        Display.ctrl('U') -> (_.uppercaseCommand),
        Display.ctrl('W') -> (_.saveFileCommand),
        Display.ctrl('Y') -> (_.redo),
        Display.ctrl('Z') -> (_.undo))

    for (ch <- Display.printable)
        keymap += ch -> (_.insertCommand(ch.toChar))
}

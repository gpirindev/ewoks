// EdBuffer.scala
// Copyright (c) 2015 J. M. Spivey

import java.io.{Reader, Writer, FileReader, FileWriter, IOException}
import Undoable.Change

/** The state of an editing session */
class EdBuffer {
    /** The text being edited. */
    private val text = new PlaneText()

    /** The display. */
    private var display: Display = null

    /** The immutable blocks */
    var blocks = new Immutables

    // State components that are preserver by undo and redo

    /** Current editing position. */
    private var _point = 0

    /** Current mark position. */
    private var _mark = 0

    /** Timestamp */
    private var _timestamp = 0

    // State components that are not restored on undo

    /** File name for saving the text. */
    private var _filename = ""

    /** Dirty flag */
    private var modified = false

    /** Time of last save */
    private var lastSave = 0

    /** At save time */
    def atSave() = {lastSave == timestamp}


    /** Register a display */
    def register(display: Display) { this.display = display }

    /** Mark the buffer as modified */
    private def setModified() { modified = true }

    /** Mark the buffer as modified or not*/
    private def setModified(isMod: Boolean) { modified = isMod }

    /** Test whether the text is modified */
    def isModified = modified
    

    // Display update
    
    /** Extent that the display is out of date. */
    private var damage = EdBuffer.CLEAN
    
    /** If damage = REWRITE_LINE, the line that should be rewritten */
    private var damage_line = 0

    /** Note damage to the display. */
    private def noteDamage(rewrite: Boolean) {
        val newdamage = 
            if (rewrite) EdBuffer.REWRITE else EdBuffer.REWRITE_LINE
        damage = Math.max(damage, newdamage)
        damage_line = text.getRow(point)
    }
    
    /** Force a display rewrite */
    def forceRewrite() { noteDamage(true) }

    /** Update display with cursor at point */
    def update() { update(point) }

    /** Update display with cursor at arbitrary position */
    def update(pos: Int) {
        display.refresh(damage, text.getRow(pos), text.getColumn(pos))
        damage = EdBuffer.CLEAN
    }
    
    /** Initialise display */
    def initDisplay() {
        noteDamage(true)
        update()
    }


    // Accessors

    def point = _point

    def point_=(point: Int) {
        if (damage == EdBuffer.REWRITE_LINE && getRow(point) != damage_line)
            damage = EdBuffer.REWRITE
        _point = point
    }

    def mark = _mark

    def mark_=(mark: Int) {
        _mark = mark
    }
    
    def timestamp = _timestamp

    def timestamp_=(timestamp: Int) { _timestamp = timestamp}

    def filename = _filename

    private def filename_=(filename: String) { _filename = filename }


    // Delegate methods for text
    
    def charAt(pos: Int) = text.charAt(pos)

    def getRow(pos: Int) = text.getRow(pos)

    def getColumn(pos: Int) = text.getColumn(pos)
    
    def getPos(row: Int, col: Int) = text.getPos(row, col)

    def length = text.length

    def getLineLength(row: Int) = text.getLineLength(row)

    def getRange(pos: Int, len: Int) = text.getRange(pos, len)

    def numLines = text.numLines

    def fetchLine(n: Int, buf: Text) { text.fetchLine(n, buf) }

    def writeFile(out: Writer) { text.writeFile(out) }

    /**A letter or a number at this position*/
    def letterOrNum(pos: Int) :Boolean = {
        val ch = charAt(pos).toInt
        if((48<=ch && ch<58)||(65<=ch && ch<91)||(97<=ch && ch<123)) return true
        return false
    }

    /** Starting position of the word the cursor is at */
    def startOfWord(pos: Int): Int = {
        if(pos == 0) return 0
        var st = pos - 1
        while(st != 0 && letterOrNum(st)) st-=1
        if(st == 0 && letterOrNum(st)) return 0
        return st+1
    }

    /** Length of the word the cursor is at */
    def lengthOfWord(pos: Int): Int = {
        return endOfWord(pos) - startOfWord(pos) + 1
    }

    /** Ending position of the word the cursor is at */
    def endOfWord(pos: Int): Int = {
        if(pos == length - 1) return length - 1
        var st = pos + 1
        while(st != length && letterOrNum(st)) st+=1
        return st-1
    }

    /** Convert a character using ROT-13*/
    def rotify(ch: Char): Char = {
        if('a' <= ch && ch <='z') return (97 + (ch.toInt -97 + 13)%26).toChar
        if('A' <= ch && ch <='Z') return (65 + (ch.toInt -65 + 13)%26).toChar
        return ch
    }

    // Mutator methods
    /** Replace a character in the text using ROT-13*/
    def rotifyChar(pos: Int): Unit = {
        val ch = text.charAt(pos)
        val rot = rotify(ch)
        if(ch != rot) {
            text.deleteChar(pos)
            text.insert(pos, rot)
        }
    }

    /** Encrypt a range */
    def encrypt(from: Int, to: Int): Unit = {
        noteDamage(true)
        for(i<- from to to) rotifyChar(i)
        setModified()
    }

    /** Delete a character */
    def deleteChar(pos: Int) {
        val ch = text.charAt(pos)
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.deleteChar(pos)
	      timestamp = timestamp + 1
        setModified()
    }

    /** Delete a range of characters. */
    def deleteRange(pos: Int, len: Int) {
        noteDamage(true)
        text.deleteRange(pos, len)
	      timestamp = timestamp + 1
        setModified()
    }
    
    /** Insert a character */
    def insert(pos: Int, ch: Char) {
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.insert(pos, ch)
	      timestamp = timestamp + 1
        setModified()
    }
    
    /** Insert a string */
    def insert(pos: Int, s: String) {
        noteDamage(true)
        text.insert(pos, s)
        setModified()
    }
    
    /** Insert an immutable text. */
    def insert(pos: Int, s: Text.Immutable) {
        noteDamage(true)
        text.insert(pos, s)
        setModified()
    }
    
    /** Insert a Text. */
    def insert(pos: Int, t: Text) {
        noteDamage(true)
        text.insert(pos, t)
        setModified()
    }

    /** Replace a Text. */
    def replace(pos: Int, s: String) {
        noteDamage(true)
        val len = s.length
        text.deleteRange(pos, len)
        text.insert(pos, s)
        timestamp = timestamp + 1
        setModified()
    }
    
    /** Transpose. */
    def transpose(pos: Int) {
        noteDamage(true)
        //not at the beginning of the line, not at the end of the text
        if(getColumn(pos) != 0 && pos < length){
          //at the middle of the line
          if(charAt(pos) != '\n') {
            var t = charAt(pos);
            insert(pos-1, t);
            deleteChar(pos+1);
          }
          //at the end of the line
          else if(charAt(pos) == '\n') {
            //next line is not empty
            if(pos + 2 < length && charAt(pos+1) != '\n'){
              var t = charAt(pos-1);
              insert(pos-1, charAt(pos+1));
              deleteChar(pos);
              deleteChar(pos+1);
              insert(pos+1, t);
            }
            //next line is empty
            else {
              var t = charAt(pos-1);
              deleteChar(pos-1);
              insert(pos, t);
            }
          }
        }
	      timestamp = timestamp + 1
        setModified()
    }
    
    /** Search for a string */
    def search(elem : String, pos: Int): Int = {
        var from = pos
        var piece = elem
        var index = text.indexOf(piece, from)
        noteDamage(index != -1)
        return index
    }
    
    /** Load a file into the buffer. */
    def loadFile(name: String) {
        filename = name
        text.clear()
        
        try {
            val in = new FileReader(name)
            text.insertFile(0, in)
            in.close()
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't read file '%s'", name)
        }
        
        modified = false
        noteDamage(true)
    }
    
    /** Save contents on a file */
    def saveFile(name: String) {
        filename = name
    
        try {
            val out = new FileWriter(name)
            text.writeFile(out)
            out.close()
            modified = false
	          lastSave = timestamp
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't write '%s'", name)
        }
    }

    /** Make a Memento that records the current editing state */
    def getState() = new Memento()
    
    /** An immutable record of the editor state at some time.  The state that
     * is recorded consists of just the current point. */
    class Memento {
        private val pt = point
        private val mk = mark
	      private val time = timestamp
        private val isMod = modified
        /** Restore the state when the memento was created */
        def restore() { point = pt; mark = mk; timestamp = time; setModified(isMod);}
    }

    /** Change that records an insertion */
    class Insertion(pos: Int, text: Text.Immutable) extends Change {
        def undo() { deleteRange(pos, text.length) }
        def redo() { insert(pos, text) }
        override def amalgamate(change: Change) = {
            change match {
                case other: Save => true
                case _ => false
            }
        }
    }

    /** Insertion that can be amalgamated with adjacent, similar changes */
    class AmalgInsertion(val pos: Int, ch: Char, val time: Int) extends Change {
        /** The text inserted by all commands that have merged with this one */
        private val text = new Text(ch)

        def undo() { deleteRange(pos, text.length) }

        def redo() { insert(pos, text) }

        override def amalgamate(change: Change) = {
            change match {
                case other: Save => true
                case other: AmalgInsertion =>
                    if (text.charAt(text.length-1) == '\n'
                            || other.pos != this.pos + this.text.length || (this.time < lastSave && this.time + this.text.length >= lastSave)) 
                        false
                    else {
                        text.insert(text.length, other.text)
                        true
                    }

                case _ => false
            }
        }
    }

    /** Change that records a deletion */
    class Deletion(pos: Int, deleted: Any) extends Change {
        def undo() { deleted match {
          case _: Char => insert(pos, deleted.asInstanceOf[Char])
          case _: Text.Immutable => insert(pos, deleted.asInstanceOf[Text.Immutable])
          }
        }
        def redo() { deleted match {
          case _: Char => deleteChar(pos)
          case _: Text.Immutable => deleteRange(pos, deleted.asInstanceOf[Text.Immutable].length)
          } 
        }
        override def amalgamate(change: Change) = {
            change match {
                case other: Save => true
                case _ => false
            }
        }
    }

    /** Change that records word replacement */
    class Replacement(val start: Int, s: String) extends Change {
        def undo() { replace(start, s) }
        def redo() { replace(start, s.toUpperCase)}
        override def amalgamate(change: Change) = {
            change match {
                case other: Replacement =>
                    if(this.start == other.start) true
                    else false
                case other: Save => true
                case _ => false
            }
        }
    }
    
    /** Change that records transposition */
    class Transposition(pos: Int) extends Change {
        def undo() { transpose(pos) }
        def redo() { transpose(pos) }
        override def amalgamate(change: Change) = {
            change match {
                case other: Save => true
                case _ => false
            }
        }
    }

    /** Change that records saving */
    class Save() extends Change {
        def undo() {}
        def redo() {}
        override def amalgamate(change: Change) = {
            change match {
                case other: Save => true
                case _ => false
            }
        }
    }

    def wrapChange(before: Memento, change: Change, after: Memento) = {
        if (change == null)
            null
        else
            new EditorChange(before, change, after)
    }

    /** Wrapper for text changes that preserves other state */
    class EditorChange(before: Memento, 
            private val change: Change,
            private var after: Memento) extends Change {

        def undo() {
            change.undo(); before.restore()
        }
            
        def redo() {
            change.redo(); after.restore()
        }
        
        def amalgamate(other: EditorChange) = {
            if (! change.amalgamate(other.change))
                false
            else {
                after = other.after
                true
            }
        }

        override def amalgamate(other: Change) =
            amalgamate(other.asInstanceOf[EditorChange])
    }
}

object EdBuffer {
    /** Possible value for damage. */
    val CLEAN = 0
    val REWRITE_LINE = 1
    val REWRITE = 2
}

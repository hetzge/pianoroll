package pianoroll

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.FeatureSpec
import org.scalatest.FunSuite
import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.layout.StackPane
import javafx.scene.control.Label
import javafx.scene.Scene
import javafx.scene.canvas.Canvas
import javafx.animation.AnimationTimer
import javafx.scene.paint.Color
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent
import javafx.scene.input.MouseDragEvent
import javafx.scene.Node
import javafx.scene.input.KeyEvent
import java.util.BitSet
import javafx.scene.Group
import javafx.scene.input.KeyCode
import javafx.scene.control.Button
import javafx.scene.input.MouseButton
import javafx.scene.Parent

object Logger {

  def debug(messages: Any*) = {
    println("debug => " + messages.mkString(", "))
  }

  def info(messages: Any*) = {
    println("info => " + messages.mkString(", "))
  }

  def error(messages: Any*) = {
    println("error => " + messages.mkString(", "))
  }

}

case class Position(val x: Int, val y: Int) {

  def eachX(length: Int)(consumer: (Position) => Unit): Unit = (x until (x + length)).map(Position(_, y)).foreach(consumer)

  def eachX(until: Position)(consumer: (Position) => Unit): Unit = eachX(until.x - x)(consumer)

}

case class Id(val value: Int)

class Pattern(val parentOption: Option[Pattern] = None) {
  var root = Position(0, 0)

  type ByPositionToneMap = scala.collection.mutable.HashMap[Position, Tone]
  type ByIdToneMap = scala.collection.mutable.HashMap[Id, Tone]
  type ToneSet = scala.collection.mutable.HashSet[Tone]

  /**
   * All tones rendered on this pattern. Can contain non real tones which are temporary or has helper functionality.
   */
  private val tonesByPosition: ByPositionToneMap = scala.collection.mutable.HashMap()
  private val tonesById: ByIdToneMap = scala.collection.mutable.HashMap()

  /**
   * A collection of all real tones on this collection
   */
  private val tones: ToneSet = scala.collection.mutable.HashSet()

  
  /**
   * TODO move this logic to gui code
   */
  def action(fromParameter: Position, toParameter: Position, leftMouseButton: Boolean, rightMouseButton: Boolean, strgKey: Boolean, altKey: Boolean) = {

    val from = if (fromParameter.x > toParameter.x) toParameter else fromParameter
    val to = if (toParameter.x < fromParameter.x) fromParameter else toParameter

    Logger.debug("action", from, to, leftMouseButton, rightMouseButton, strgKey, altKey)

    if (leftMouseButton) {
      if (getTone(fromParameter).isDefined) {
        if (strgKey) {
          stretch(fromParameter, toParameter)
        } else {
          move(fromParameter, toParameter)
        }
      } else {
        draw(from, to)
      }
    } else if (rightMouseButton) {
      remove(from, to)
    }

  }

  def redraw() = {
    tonesByPosition.clear()
    tonesById.clear()
    tones.foreach(addTone(_))
  }

  // TODO prüfen ob bereits vorhanden -> Exception -> Behandlung ?
  def addTone(tone: Tone) = {
    tone.eachPosition { position => tonesByPosition.put(position, tone) }
    tonesById.put(tone.id, tone)
    tones.add(tone)
  }

  def removeTone(tone: Tone) = {
    tone.eachPosition { position => tonesByPosition.remove(position) }
    tonesById.remove(tone.id)
    tones.remove(tone)
  }

  def getTone(id: Id): Option[Tone] = {
    tonesById.get(id) match {
      case None => {
        parentOption match {
          case Some(parent) => parent.getTone(id)
          case None => None
        }
      }
      case some => some
    }
  }

  def getTone(position: Position, source: Pattern = Pattern.this): Option[Tone] = {
    tonesByPosition.get(position) match {
      case None => {
        parentOption match {
          case Some(parent) => parent.getTone(position, source)
          case None => None
        }
      }
      case Some(tone) => {
        source.getTone(tone.id) match {
          case Some(idTone) => {
            if (idTone.containPoint(position)) {
              Some(idTone)
            } else {
              None
            }
          }
          case None => None
        }
      }
    }
  }

  def draw(from: Position, to: Position) = {
    Logger.debug("draw", from, to)

    val length = to.x - from.x

    var startPositionOption: Option[Position] = None
    var endPositionOption: Option[Position] = None

    from.eachX(length) { positionI =>
      if (getTone(positionI).isEmpty) {
        if (startPositionOption.isEmpty) {
          startPositionOption = Some(positionI)
        }
      }
      if (getTone(positionI).isDefined) {
        if (startPositionOption.isDefined && endPositionOption.isEmpty) {
          endPositionOption = Some(positionI)
        }
      }
      // end of draw
      if (positionI.x == from.x + length - 1) {
        endPositionOption = Some(Position(positionI.x + 1, positionI.y))
      }

      if (startPositionOption.isDefined && endPositionOption.isDefined) {
        addTone(new Tone(Pattern.this, startPositionOption.get, endPositionOption.get.x - startPositionOption.get.x))
        startPositionOption = None
        endPositionOption = None
      }
    }
  }

  def remove(from: Position, to: Position) = {
    Logger.debug("remove", from, to)

    val length = to.x - from.x;

    from.eachX(length) { positionI =>
      getTone(positionI).foreach { tone =>
        removeTone(tone)
        if (tone.calculatedPosition.x < from.x && tone.calculatedPosition.x + tone.calculatedLength > from.x + length) {
          // split
          Logger.debug("remove split")

          val oldLength = tone.calculatedLength
          tone.setLength(tone.length - (tone.calculatedPosition.x + tone.calculatedLength - from.x))
          addTone(new Tone(Pattern.this, Position(tone.position.x + tone.length + length, tone.position.y), oldLength - length - tone.length, tone.parentOption))

          addTone(tone)
        } else if (tone.calculatedPosition.x < from.x && tone.calculatedPosition.x + tone.calculatedLength <= from.x + length) {
          // shorter
          Logger.debug("remove shorter")

          tone.setLength(from.x - tone.calculatedPosition.x)

          addTone(tone)
        } else if (tone.calculatedPosition.x >= from.x && tone.calculatedPosition.x + tone.calculatedLength <= from.x + length) {
          // remove
          Logger.debug("remove remove")

          removeTone(tone)
        } else if (tone.calculatedPosition.x < from.x + length && tone.calculatedPosition.x + tone.calculatedLength > from.x + length) {
          // move and shorter
          Logger.debug("remove move and shorter")

          val offset = from.x + length - positionI.x
          tone.setLength(tone.length - offset)
          tone.setX(tone.position.x + offset)

          addTone(tone)
        } else {
          throw new IllegalStateException("Unknown how to handle remove")
        }
      }
    }
  }

  def move(from: Position, to: Position) = {
    Logger.debug("move", from, to)

    getTone(from).foreach { tone =>
      removeTone(tone)
      val offset = to.x - from.x
      tone.addX(offset)
      tone.setY(to.y)
      addTone(tone)
    }
  }

  def stretch(from: Position, to: Position) = {
    Logger.debug("stretch", from, to)

    getTone(from).foreach { tone =>
      removeTone(tone)
      val offset = to.x - from.x
      tone.setLength(tone.length + offset)
      addTone(tone)
    }
  }

  def connect(from: Position, to: Position) = {
    // TODO (on strg)
  }

}
class Instance(val pattern: Pattern)

object EmptyTone extends Tone(null, Position(0, 0), 0, None)
object Tone {
  private var _nextId = 0

  def nextId = {
    val result = _nextId
    _nextId += 1
    Id(result)
  }
}

// TODO owner: Pattern raus ?!
class Tone(val owner: Pattern, private var _position: Position, private var _length: Int, val parentOption: Option[Tone] = None) {

  private val _id = Tone.nextId

  def id: Id = parentOption match {
    case Some(parent) => parent.id
    case None => _id
  }

  def position = _position

  def length: Int = _length

  def setLength(lengthValue: Int) = _length = lengthValue

  def setX(xValue: Int) = _position = _position.copy(x = xValue)

  def addX(offset: Int) = _position = _position.copy(x = _position.x + offset)

  def setY(yValue: Int) = _position = _position.copy(y = yValue)

  def calculatedPosition = Position(_position.x + parentOption.getOrElse(EmptyTone)._position.x, _position.y + parentOption.getOrElse(EmptyTone)._position.y)

  def calculatedLength = _length + parentOption.getOrElse(EmptyTone)._length

  def containPoint(containPoint: Position) = calculatedPosition.y == containPoint.y && calculatedPosition.x <= containPoint.x && calculatedPosition.x + _length > containPoint.x

  def eachPosition(consumer: (Position) => Unit) = calculatedPosition.eachX(_length)(consumer)

  override def equals(that: Any): Boolean = {
    that match {
      case that: Tone => this.hashCode == that.hashCode
      case _ => false
    }
  }

  override def hashCode: Int = _id.value

}

class Spec extends FunSuite with Matchers {

  test("pattern variant changes tone") {
    val patternA = new Pattern()
    val toneA = new Tone(patternA, Position(1, 1), 1)
    patternA.addTone(toneA)

    val patternB = new Pattern(Some(patternA))
    val toneB = new Tone(patternB, Position(2, 2), 1, Some(toneA))
    patternB.addTone(toneB)

    patternA.getTone(Position(1, 1)) should be(Some(toneA))
    patternB.getTone(Position(1, 1)) should be(None)
    patternB.getTone(Position(2, 2)) should be(None)
    patternB.getTone(Position(3, 3)) should be(Some(toneB))
  }

  test("draw tone on blank") {
    val patternA = new Pattern()
    
    patternA.draw(Position(3, 3), Position(6, 3))
    
    Position(3, 3).eachX(3){ position =>
      patternA.getTone(position) should not be (None)
    }
    
    patternA.getTone(Position(2, 3)) should be(None)
    patternA.getTone(Position(6, 3)) should be(None)
  }
  
  test("draw tone with existing in front, middle and at the end") {
    val patternA = new Pattern()
    val toneA = new Tone(patternA, Position(0, 2), 2)
    patternA.addTone(toneA)

    val toneB = new Tone(patternA, Position(4, 2), 2)
    patternA.addTone(toneB)

    val toneC = new Tone(patternA, Position(8, 2), 2)
    patternA.addTone(toneC)

    patternA.draw(Position(0, 2), Position(8, 2))

    patternA.getTone(Position(1, 2)) should be(Some(toneA))
    patternA.getTone(Position(1, 2)) should not be (patternA.getTone(Position(2, 2)))
    patternA.getTone(Position(2, 2)) should be(patternA.getTone(Position(3, 2)))
    patternA.getTone(Position(4, 2)) should be(patternA.getTone(Position(5, 2)))
    patternA.getTone(Position(5, 2)) should be(Some(toneB))
    patternA.getTone(Position(6, 2)) should not be (patternA.getTone(Position(5, 2)))
    patternA.getTone(Position(7, 2)) should be(patternA.getTone(Position(6, 2)))
    patternA.getTone(Position(8, 2)) should not be (patternA.getTone(Position(6, 2)))
    patternA.getTone(Position(8, 2)) should be(Some(toneC))
  }

  test("remove tone with existing in front, middle and at the end") {
    val patternA = new Pattern()

    val toneA = new Tone(patternA, Position(0, 2), 2)
    patternA.addTone(toneA)

    val toneB = new Tone(patternA, Position(4, 2), 2)
    patternA.addTone(toneB)

    val toneC = new Tone(patternA, Position(8, 2), 2)
    patternA.addTone(toneC)

    patternA.remove(Position(1, 2), Position(9, 2))

    patternA.getTone(Position(0, 2)) should be(Some(toneA))
    patternA.getTone(Position(4, 2)) should not be (Some(toneB))
    patternA.getTone(Position(8, 2)) should not be (Some(toneC))
    patternA.getTone(Position(9, 2)) should be(Some(toneC))

    Position(1, 2).eachX(7) { position =>
      patternA.getTone(position) should be(None)
    }
  }

  test("remove tone with existing around") {
    val patternA = new Pattern()
    val toneAPosition = Position(0, 2)
    val toneA = new Tone(patternA, toneAPosition, 8)
    patternA.addTone(toneA)

    patternA.remove(Position(2, 2), Position(6, 2))

    patternA.getTone(toneAPosition) should be(Some(toneA))
    patternA.getTone(Position(1, 2)) should be(Some(toneA))
    patternA.getTone(Position(6, 2)) should not be (Some(toneA))
    patternA.getTone(Position(6, 2)) should not be (None)

    Position(2, 2).eachX(4) { position =>
      patternA.getTone(position) should be(None)
    }
  }

  test("remove till end") {
    val patternA = new Pattern()
    val toneAPosition = Position(0, 2)
    val toneA = new Tone(patternA, toneAPosition, 8)
    patternA.addTone(toneA)

    patternA.remove(Position(5, 2), Position(8, 2))

    patternA.getTone(toneAPosition) should be(Some(toneA))
    patternA.getTone(Position(5, 2)) should be(None)
    patternA.getTone(Position(4, 2)) should be(Some(toneA))
  }

  test("move tone") {
    val patternA = new Pattern()
    val toneAPositon = Position(0, 2)
    val toneA = new Tone(patternA, toneAPositon, 8)
    patternA.addTone(toneA)

    patternA.move(Position(0, 1), Position(0, 3))

    patternA.getTone(toneAPositon) should be(Some(toneA))
    patternA.getTone(Position(0, 3)) should be(None)

    patternA.move(Position(1, 2), Position(3, 3))

    patternA.getTone(toneAPositon) should be(None)
    patternA.getTone(Position(1, 2)) should be(None)
    patternA.getTone(Position(2, 2)) should be(None)
    patternA.getTone(Position(9, 2)) should be(None)
    patternA.getTone(Position(10, 2)) should be(None)

    patternA.getTone(Position(1, 3)) should be(None)
    patternA.getTone(Position(2, 3)) should be(Some(toneA))
    patternA.getTone(Position(9, 3)) should be(Some(toneA))
    patternA.getTone(Position(10, 3)) should be(None)
  }

  test("move tone back") {
    val patternA = new Pattern()
    val toneAPositon = Position(10, 2)
    val toneA = new Tone(patternA, toneAPositon, 8)
    patternA.addTone(toneA)

    patternA.move(Position(12, 2), Position(8, 2))

    patternA.getTone(toneAPositon) should be(Some(toneA))
    patternA.getTone(Position(6, 2)) should be(Some(toneA))
    patternA.getTone(Position(5, 2)) should be(None)
    patternA.getTone(Position(18, 2)) should be(None)
    patternA.getTone(Position(13, 2)) should be(Some(toneA))
    patternA.getTone(Position(14, 2)) should be(None)
  }

  test("stretch tone") {
    val patternA = new Pattern()
    val toneAPositon = Position(0, 2)
    val toneA = new Tone(patternA, toneAPositon, 8)
    patternA.addTone(toneA)

    patternA.stretch(Position(5, 2), Position(7, 3))

    patternA.getTone(Position(9, 2)) should be(Some(toneA))
    patternA.getTone(Position(10, 2)) should be(None)
    patternA.getTone(toneAPositon) should be(Some(toneA))
  }

  test("single tone") {
    val patternA = new Pattern()

    patternA.draw(Position(2, 2), Position(3, 2))

    patternA.getTone(Position(2, 2)) should not be (None)
    patternA.getTone(Position(3, 2)) should be(None)

    patternA.move(Position(2, 2), Position(4, 4))

    patternA.getTone(Position(2, 2)) should be(None)
    patternA.getTone(Position(4, 4)) should not be (None)
    patternA.getTone(Position(5, 4)) should be(None)
    patternA.getTone(Position(3, 4)) should be(None)

    patternA.stretch(Position(4, 4), Position(6, 4))

    patternA.getTone(Position(4, 4)) should not be (None)
    patternA.getTone(Position(5, 4)) should not be (None)
    patternA.getTone(Position(6, 4)) should not be (None)
    patternA.getTone(Position(7, 4)) should be(None)
    patternA.getTone(Position(3, 4)) should be(None)
    patternA.getTone(Position(4, 4)) should be(patternA.getTone(Position(6, 4)))

    patternA.remove(Position(5, 4), Position(6, 4))

    patternA.getTone(Position(4, 4)) should not be (None)
    patternA.getTone(Position(5, 4)) should be(None)
    patternA.getTone(Position(6, 4)) should not be (None)

    patternA.remove(Position(4, 4), Position(5, 4))
    patternA.remove(Position(6, 4), Position(7, 4))

    patternA.getTone(Position(4, 4)) should be(None)
    patternA.getTone(Position(5, 4)) should be(None)
    patternA.getTone(Position(6, 4)) should be(None)
  }

}

class JavaFxApp extends Application {

  override def start(primaryStage: Stage) {
    primaryStage.setTitle("Sup!")

    val root = new StackPane()

    val canvas = new PianoRollCanvas()

    root.getChildren.add(canvas)
    root.getChildren.add(KeyHelper)

    primaryStage.setScene(new JavaFxAppScene(root))
    primaryStage.show()
  }

  class JavaFxAppScene(root: Parent) extends Scene(root, 800, 800) {
    setOnKeyPressed(KeyHelper.pressedHandler)
    setOnKeyReleased(KeyHelper.releasedHandler)
  }

  class PianoRollCanvas extends Canvas {

    val graphicsContext = getGraphicsContext2D()
    var cameraPosition = Position(0, 0)
    var horizontalScale: Int = 10
    var verticalScale: Int = 20

    val pattern = new Pattern()
    val toneAPositon = Position(0, 0)
    val toneA = new Tone(pattern, toneAPositon, 8)
    pattern.addTone(toneA)
    
    setWidth(800)
    setHeight(800)

    def render() = {
      Logger.debug("render")

      renderBackground()
      renderGrid()
      renderPattern()
    }

    def renderBackground() = {
      graphicsContext.clearRect(0, 0, getWidth(), getHeight())
    }

    def renderGrid() = {
      graphicsContext.setLineWidth(1.0d)
      graphicsContext.setStroke(Color.WHITE.darker())

      for (x <- cameraPosition.x / horizontalScale to cameraPosition.x + getWidth().toInt) {
        graphicsContext.strokeLine(x * horizontalScale, 0, x * horizontalScale, getHeight())
      }

      graphicsContext.setLineWidth(1.0d)
      graphicsContext.setStroke(Color.WHITE.darker().darker())

      for (y <- cameraPosition.y / verticalScale to cameraPosition.y + getHeight().toInt) {
        graphicsContext.strokeLine(0, y * verticalScale, getWidth(), y * verticalScale)
      }
    }

    def renderPattern() = {
      val verticalOffset = 2

      for (x <- cameraPosition.x / horizontalScale to cameraPosition.x + getWidth().toInt) {
        for (y <- cameraPosition.y / verticalScale to cameraPosition.y + getHeight().toInt) {
          val position = Position(x, y)

          pattern.getTone(position).foreach { tone =>
            graphicsContext.setFill(Color.RED)
            graphicsContext.fillRect(x * horizontalScale, y * verticalScale + verticalOffset, horizontalScale, verticalScale - verticalOffset * 2)

            if (tone.calculatedPosition.equals(position)) {
              graphicsContext.setFill(Color.BLUE.brighter())
              graphicsContext.fillRect(x * horizontalScale, y * verticalScale, 3, verticalScale)
            }
          }
        }
      }
    }

    var dragFromOption: Option[Position] = None
    var click = false

    setOnDragDetected(new EventHandler[MouseEvent]() {
      override def handle(mouseEvent: MouseEvent) = {
        println("drag detected")
        startFullDrag();
        click = false

        val dragFromX = toXGridPosition(mouseEvent.getX())
        val dragFromY = toYGridPosition(mouseEvent.getY())

        dragFromOption = Some(Position(dragFromX, dragFromY))

        mouseEvent.consume()
      }
    })

    setOnMouseDragReleased(new EventHandler[MouseDragEvent]() {
      override def handle(mouseEvent: MouseDragEvent) = {
        println("drag released")
        if (dragFromOption.isDefined) {

          val dragFrom = dragFromOption.get

          val dragToX = toXGridPosition(mouseEvent.getX())
          val dragToY = toYGridPosition(mouseEvent.getY())

          val dragTo = Position(dragToX + 1, dragToY)

          action(dragFrom, dragTo, mouseEvent.getButton().equals(MouseButton.PRIMARY), mouseEvent.getButton().equals(MouseButton.SECONDARY), KeyHelper.isKeyPressed(KeyCode.CONTROL.ordinal()), KeyHelper.isKeyPressed(KeyCode.ALT.ordinal()))

          println("drag")

          mouseEvent.consume()
          dragFromOption = None
        }
      }
    })

    setOnMousePressed(new EventHandler[MouseEvent]() {
      override def handle(mouseEvent: MouseEvent) = {
        println("pressed")
        click = true
      }
    })

    setOnMouseReleased(new EventHandler[MouseEvent]() {
      override def handle(mouseEvent: MouseEvent) = {
        println("released")
        if (click) {

          val x = toXGridPosition(mouseEvent.getX())
          val y = toYGridPosition(mouseEvent.getY())

          val position = Position(x, y)

          action(position, position.copy(x = position.x + 1), mouseEvent.getButton().equals(MouseButton.PRIMARY), mouseEvent.getButton().equals(MouseButton.SECONDARY), KeyHelper.isKeyPressed(KeyCode.CONTROL.ordinal()), KeyHelper.isKeyPressed(KeyCode.ALT.ordinal()))

          mouseEvent.consume()
        }
      }
    })

    def action(from: Position, to: Position, leftMouseButton: Boolean, rightMouseButton: Boolean, strgKey: Boolean, altKey: Boolean) {
      pattern.action(from, to, leftMouseButton, rightMouseButton, strgKey, altKey)
      render()
    }

    def toXGridPosition(value: Double) = (value / horizontalScale).toInt
    def toYGridPosition(value: Double) = (value / verticalScale).toInt

    def toXRenderPosition(value: Int) = value * horizontalScale
    def toYRenderPosition(value: Int) = value * verticalScale

    render()
  }

}

object KeyHelper extends Canvas {

  val pressedKeys = new BitSet()

  val pressedHandler = new EventHandler[KeyEvent]() {
    override def handle(event: KeyEvent) = {
      println(event.getCode())
      pressedKeys.set(event.getCode().ordinal(), true)
    }
  }

  val releasedHandler = new EventHandler[KeyEvent]() {
    override def handle(event: KeyEvent) = {
      println("released")
      pressedKeys.set(event.getCode().ordinal(), false)
    }
  }

  def isKeyPressed(keyCode: Int): Boolean = pressedKeys.get(keyCode)

}

object JavaFxApp {
  def main(args: Array[String]) {
    Application.launch(classOf[JavaFxApp], args: _*)
  }
}

/*
 * Wie wird eine remove Manipulation realisiert ? length = 0 ?
 * Was passiert mit Konflikten die durch eine Veränderung eines Parents entsteht ? (ähnliche Auflösung wie bei remove/draw ?!) 
 * ... Überlappung / verdecken von 2er Reihe
 * Rerender only on changes
 * Drag Backward
 */



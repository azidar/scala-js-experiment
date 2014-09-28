package example

import org.scalajs.dom
import scala.util.Random
import scala.scalajs.js
import js.annotation.JSExport
case class Point(x: Double, y: Double){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)
  def *(d: Double) = Point(x * d, y * d)
  def /(d: Double) = Point(x / d, y / d)
  def length = Math.sqrt(x * x + y * y)
}

case class Ball(var pos: Point, var vel: Point, val w:Int=20, val h:Int=20)
case class Paddle(var pos: Point, var vel:Point=Point(0,0), val w:Int=20, val h:Int=200)

@JSExport
object ScalaJSExample {

  var startTime = js.Date.now()

  val canvas = dom.document
                  .getElementById("canvas")
                  .asInstanceOf[dom.HTMLCanvasElement]
  val ctx = canvas.getContext("2d")
                  .asInstanceOf[dom.CanvasRenderingContext2D]
  var player1 = Paddle(Point(dom.innerWidth.toInt*3/4, dom.innerHeight.toInt/2))
  var player2 = Paddle(Point(dom.innerWidth.toInt/4, dom.innerHeight.toInt/2))

  var ball = Ball(Point(dom.innerWidth/2, dom.innerHeight.toInt/2),Point(4,0))

  var death: Option[(String, Int)] = None
  def collision(a:Paddle, b:Ball):Boolean = {
    val nb = Ball(ball.pos + ball.vel,ball.vel)
    if((Math.abs(a.pos.x - nb.pos.x) <= ( a.w/2 + nb.w/2)) && (nb.pos.y > (a.pos.y - a.h/2) ) && (nb.pos.y < (a.pos.y + a.h/2))) {
      true 
    } else false
  }
  def run() = {

    canvas.height = dom.innerHeight
    canvas.width = dom.innerWidth

    // doing
    if(ball.pos.x <= 0) death = Some(("Player 1 Wins!",100))
    if(ball.pos.x >= dom.innerWidth.toInt) death = Some(("Player 2 Wins!",100))
    if(ball.pos.y - ball.w/2 <= 0) ball.vel = Point(ball.vel.x,-1*ball.vel.y)
    if(ball.pos.y + ball.w/2 >= dom.innerHeight) ball.vel = Point(ball.vel.x,-1*ball.vel.y)

    if(collision(player1,ball)) {
      ball.vel = Point(-1*ball.vel.x, ball.vel.y + 0.2 * player1.vel.y)
      ball.pos = ball.pos + player1.vel
    } else if(collision(player2,ball)) {
      ball.vel = Point(-1*ball.vel.x, ball.vel.y + 0.2 * player2.vel.y)
      ball.pos = ball.pos + player2.vel
    } else {
      ball.pos = ball.pos + ball.vel
    }

    //update player based on keyboard input
    def updatePlayer(p:Paddle,keys:List[Int]):Unit = {
      val deltas = List(Point(0,-6),Point(-6,0),Point(6,0),Point(0,6))
      p.vel = Point(0, 0)
      keys.zipWithIndex.map( {case (x,i) => {
        if (keysDown(x)) {
          p.pos += deltas(i)
          p.vel = deltas(i)
        }}})
    }

    //player1 uses arrow keys
    val p1keys = List(38,37,39,40)
    updatePlayer(player1,p1keys)
    //player2 uses WASD keys
    val p2keys = List(87,65,68,83)
    updatePlayer(player2,p2keys)
  }

  def deltaT = ((js.Date.now() - startTime) / 1000).toInt

  def draw() = {
    // drawing
    ctx.fillStyle = "black"

    ctx.fillRect(0, 0, canvas.width, canvas.height)
    death match{
      case None =>

        ctx.fillStyle = "white"
        ctx.fillRect(player1.pos.x - 10, player1.pos.y - 100, 20, 200)
        ctx.fillText("player1", player1.pos.x - 15, player1.pos.y - 30)

        ctx.fillStyle = "green"
        ctx.fillRect(player2.pos.x - 10, player2.pos.y - 100, 20, 200)
        ctx.fillText("player2", player2.pos.x - 15, player2.pos.y - 30)

        ctx.fillStyle = "red"
        ctx.fillRect(ball.pos.x - 10, ball.pos.y - 10, 20, 20)

        ctx.fillStyle = "white"

        ctx.fillText(s"$deltaT seconds", canvas.width / 2 - 100, canvas.height / 5)
      case Some((msg, time)) =>
        ctx.fillStyle = "white"
        ctx.fillText(msg, canvas.width / 2 - 100, canvas.height / 2)
        if (time - 1 == 0){
          death = None
          startTime = js.Date.now()
        }else{
          death = Option((msg, time - 1))
        }
    }
  }

  val keysDown = collection.mutable.Set.empty[Int]

  @JSExport
  def main(): Unit = {
    dom.console.log("main")
    /*
    dom.document.onmousemove = { (e: dom.MouseEvent) =>
      player = Paddle(Point(e.clientX.toInt, e.clientY.toInt),Point(0,0),20,200)
      (): js.Any
    }
    */
    dom.onkeydown = {(e: dom.KeyboardEvent) =>
      keysDown.add(e.keyCode.toInt)
    }
    dom.onkeyup = {(e: dom.KeyboardEvent) =>
      keysDown.remove(e.keyCode.toInt)
    }
 
    dom.setInterval(() => {run(); draw()}, 20)
  }
}

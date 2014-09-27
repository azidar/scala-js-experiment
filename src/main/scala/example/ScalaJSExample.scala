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

case class Ball(var pos: Point, var vel: Point, val w:Int, val h:Int)
case class Paddle(var pos: Point, var vel:Point, val w:Int, val h:Int)

@JSExport
object ScalaJSExample {

  var startTime = js.Date.now()

  val canvas = dom.document
                  .getElementById("canvas")
                  .asInstanceOf[dom.HTMLCanvasElement]
  val ctx = canvas.getContext("2d")
                  .asInstanceOf[dom.CanvasRenderingContext2D]
  var player  = Paddle(Point(dom.innerWidth.toInt*3/4, dom.innerHeight.toInt/2),Point(0,0),20,200)
  var player2 = Paddle(Point(dom.innerWidth.toInt/4, dom.innerHeight.toInt/2),Point(0,0),20,200)

  var ball = Ball(Point(dom.innerWidth/2, dom.innerHeight.toInt/2),Point(4,0),20,20)

  var death: Option[(String, Int)] = None
  def collision(a:Paddle, b:Ball):Boolean = {
    if((Math.abs(a.pos.x - b.pos.x) <= ( a.w/2 + b.w/2)) && (b.pos.y > (a.pos.y - a.h/2) ) && (b.pos.y < (a.pos.y + a.h/2))) {
      true 
    } else false
  }
  def run() = {

    canvas.height = dom.innerHeight
    canvas.width = dom.innerWidth

    // doing
    if(ball.pos.x <= 0) death = Some(("Player 2 Wins!",100))
    if(ball.pos.x >= dom.innerWidth.toInt) death = Some(("Player 1 Wins!",100))
    if(ball.pos.y - ball.w/2 <= 0) ball.vel = Point(ball.vel.x,-1*ball.vel.y)
    if(ball.pos.y + ball.w/2 >= dom.innerHeight) ball.vel = Point(ball.vel.x,-1*ball.vel.y)

    val nextBall = Ball(ball.pos + ball.vel,ball.vel,ball.w,ball.h)
    if(collision(player,nextBall)) {
      ball.vel = Point(-1*ball.vel.x, ball.vel.y + 0.2 * player.vel.y)
      ball.pos = ball.pos + player.vel
    } else if(collision(player2,nextBall)) {
      ball.vel = Point(-1*ball.vel.x, ball.vel.y + 0.2 * player2.vel.y)
      ball.pos = ball.pos + player2.vel
    } else {
      ball.pos = ball.pos + ball.vel
    }
    //update player based on keyboard input
    
    //player1 uses arrow keys
    player.vel = Point(0, 0)
    if (keysDown(38)) {
      player.pos += Point(0, -6)//up
      player.vel = Point(0, -6)
    }
    if (keysDown(37)) {
      player.pos += Point(-6, 0)//let
      player.vel = Point(-6,0)
    }
    if (keysDown(39)) {
      player.pos += Point(6, 0)//right
      player.vel = Point(6, 0)
    }
    if (keysDown(40)) {
      player.pos += Point(0, 6)//down
      player.vel = Point(0, 6)
    }

    //player2 uses WASD keys
    /*
    if (keysDown(87)) player2.pos += Point(0, -2)//up(W)
    if (keysDown(65)) player2.pos += Point(-2, 0)//left(A)
    if (keysDown(68)) player2.pos += Point(2, 0)//right(D)
    if (keysDown(83)) player2.pos += Point(0, 2)//down(S)
    */
    player2.vel = Point(0, 0)
    if (keysDown(87)) {
      player2.pos += Point(0, -6)//up
      player2.vel = Point(0, -6)
    }
    if (keysDown(65)) {
      player2.pos += Point(-6, 0)//let
      player2.vel = Point(-6,0)
    }
    if (keysDown(68)) {
      player2.pos += Point(6, 0)//right
      player2.vel = Point(6, 0)
    }
    if (keysDown(83)) {
      player2.pos += Point(0, 6)//down
      player2.vel = Point(0, 7)
    }
  }

/*
    balls = balls.filter(e =>
      e.pos.x >= 0 && e.pos.x <= canvas.width &&
      e.pos.y >= 0 && e.pos.y <= canvas.height
    )

    //def randSpeed = Random.nextInt(5) - 3
    balls = balls ++ Seq.fill(20 - balls.length)(
      new Ball(
        Point(Random.nextInt(canvas.width.toInt), 0),
        Point(randSpeed, randSpeed)
      )
    )

    for(ball <- balls){
      ball.pos = ball.pos + ball.vel
      val delta = player - ball.pos
      ball.vel = ball.vel + delta / delta.length / 100
    }

    if(balls.exists(e => (e.pos - player).length < 20)){
      death = Some((s"You lasted $deltaT seconds", 100))
      balls = balls.filter(e => (e.pos - player).length > 20)
    }
    
  }
  */
  def deltaT = ((js.Date.now() - startTime) / 1000).toInt

  def draw() = {
    // drawing
    ctx.fillStyle = "black"

    ctx.fillRect(0, 0, canvas.width, canvas.height)
    death match{
      case None =>

        ctx.fillStyle = "white"
        ctx.fillRect(player.pos.x - 10, player.pos.y - 100, 20, 200)
        ctx.fillText("player1", player.pos.x - 15, player.pos.y - 30)

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

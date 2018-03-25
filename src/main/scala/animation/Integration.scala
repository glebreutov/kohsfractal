package animation

import java.lang.Math.toRadians

import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js
import org.scalajs.dom.html.{Button, Canvas}
import org.scalajs.dom.raw.HTMLImageElement
import Animation._


object Integration extends js.JSApp {

  def main(): Unit = {
    initGame
  }


  def initGame(): Unit = {

    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    val size = 0.95 * Math.min(dom.window.innerHeight, dom.window.innerWidth)
    canvas.height = size.toInt
    canvas.width = size.toInt
    dom.document.body.appendChild(canvas)

    val origin = Vec(canvas.height / 2, canvas.height / 2)

    val sideLen = (canvas.height * 0.6).intValue
    val outerTriangleLen = sideLen + sideLen / 3
    val startOuter = startingPointCenter(origin, outerTriangleLen)
    val startStart = startOuter + Vec((outerTriangleLen - sideLen) / 2, 0)

    println(startStart)
    def reset(): Unit ={
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, canvas.width, canvas.height)
      ctx.strokeStyle = "white"
      ctx.lineWidth = 1

    }



    def drawMe2(compl: SceneState): Unit = {
      reset()
      ctx.beginPath()
      val points = buildScene(canvas.height, canvas.width, compl)
      ctx.moveTo(points.head.x, points.head.y)
      for(p <- points.tail){
        ctx.lineTo(p.x, p.y)
      }

      ctx.stroke()
    }

    //dom.window.requestAnimationFrame(drawMe2)






    def drawMeStep(compl: SceneState): Unit = {
      drawMe2(compl)
      //println(state)
      dom.window.setTimeout(() => drawMeStep(compl.next), 20)
      //dom.window.requestAnimationFrame()
      //button.onclick = (_) => () => drawMeStep(state.next)
      //dom.window.requestAnimationFrame((_) => drawMeStep(state.next))
    }
    drawMeStep(SceneState(0))


  }
}

/**
 * Copyright 2012 David Stark
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package src.runner

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point
import java.awt.image.BufferedImage
import scala.util.Random
import src.util.MediaProvider
import src.util.Draw
import src.Planet

class GameDisplay(w : GameWorld, width : Int, height : Int) {

  private case class Star(x : Int, y : Int, scale : Int, size : Int)
  private val img : BufferedImage = MediaProvider.getImage("misc/star")
  private val STAR_IMGS : List[BufferedImage] = List(img, MediaProvider.scale(img, 1), img, MediaProvider.scale(img, 3))
  private val r : Random = new Random
  private val stars : List[Star] = Nil ++ (for (_ <- 0 until 1200) yield Star(r.nextInt(2300) - 300, r.nextInt(2300) - 300, r.nextInt(10) + 1, r.nextInt(2) + r.nextInt(2) + 1))
  private val helpText : String = 
    "\nPress space to advance by one event." +
    "\nPress R to toggle auto-advance." +
    "\nUse arrow keys to move view." +
    "\nPoint at things for info." +
    "\nPress S to save galaxy details to text file."
  
  private def closestPlanet(p : Point) : (Planet, Int) = {
    val viewPX : Int = (w.stage.camX + p.x - width / 2)
    val viewPY : Int = (w.stage.camY + p.y - height / 2)
    w.sg.planets.map(p => (p, (p.sprite.x + 120 - viewPX) * (p.sprite.x + 120 - viewPX) + (p.sprite.y + 120 - viewPY) * (p.sprite.y + 120 - viewPY))).minBy(_ _2)
  }

  def draw(g : Graphics2D, ptr : Option[Point]) : Unit = {
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, width, height)
    stars.foreach(p => g.drawImage(STAR_IMGS(p.size), p.x + (-w.stage.camX + width / 2) / p.scale, p.y + (-w.stage.camY + height / 2) / p.scale, null))
    g.translate(-w.stage.camX + width / 2, -w.stage.camY + height / 2)
    w.stage.draw(g)

    for {
      p <- ptr
      if ! w.sg.planets.isEmpty
      (clp, dist) = closestPlanet(p)
      if dist < 240 * 240
    } {
      Draw.text(g, "[bg=333333cc]" + clp.fullDesc(w.sg), clp.sprite.x + 170, clp.sprite.y, 320, 1000)
    }

    g.translate(w.stage.camX - width / 2, w.stage.camY - height / 2)

    Draw.text(g, "[bg=333333cc]" + w.sg.turnLog.mkString("\n"), 10, height - 100, width - 20, 100)
    Draw.text(g, "[bg=333333cc]" + w.sg.year + helpText, 10, 10)

  }
}

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

package src.util

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color

class Stage {
  var doTrack : Boolean = true
  private var sprites : List[Sprite] = Nil
  private var animations : List[Stage.Animation] = Nil

  var camX : Int = 0 //not real
  var camY : Int = 0 //not real

  def animate(a : Stage.Animation) : Unit = animations = animations :+ a
  def animate(a : Stage.Animation, a2 : Stage.Animation) : Unit = animations = animations ++ List(a, a2)

  def tick : Boolean = {
    animations = animations.flatMap(a => a.tick(this))
    animations.isEmpty
  }

  def draw(g : Graphics2D) : Unit = sprites.map(s => draw(g, s, 0, 0))

  private def draw(g : Graphics2D, s : Sprite, dx : Int, dy : Int) : Unit = {
    if (s.flash)
      g.drawImage(MediaProvider.mask(s.img, Stage.FLASH), dx + s.x, dy + s.y, null)
    else if (s.highlight)
      g.drawImage(MediaProvider.border(s.img, Stage.FLASH), dx + s.x - 1, dy + s.y - 1, null)
    else
      g.drawImage(s.img, dx + s.x, dy + s.y, null)

    s.children.foreach(ss => draw(g, ss, dx + s.x, dy + s.y))
  }

}

object Stage {

  private val FLASH : Color = new Color(255, 255, 180)

  sealed abstract class Animation {
    def tick(stage : Stage) : Option[Animation]
  }

  def delay(wait : Int) : Animation = Delay(wait)
  private case class Delay(waitTime : Int) extends Animation {

    def tick(stage : Stage) : Option[Animation] = if (waitTime > 0) Some(Delay(waitTime - 1)) else None

  }

  def tracking(s : Sprite, a : Animation) : Animation = Tracking(s, Some(a), 0, 0, 0, 0, false)
  def track(s : Sprite) : Animation = Tracking(s, None, 0, 0, 0, 0, false)
  private case class Tracking(s : Sprite, a : Option[Animation], tick : Int, var time : Int, var sx : Int, var sy : Int, lock : Boolean) extends Animation {

    def tick(stage : Stage) : Option[Animation] = {
      if (stage.doTrack) {
        val tx : Int = s.globalX + s.img.getWidth / 2
        val ty : Int = s.globalY + s.img.getHeight / 2
        if (tick == 0) {
          sx = stage.camX
          sy = stage.camY
          time = (Math.sqrt((sx - tx) * (sx - tx) + (sy - ty) * (sy - ty)) / 120).toInt + 3
        }
        stage.camX = if (lock) tx else sx + (tx - sx) * tick / time
        stage.camY = if (lock) ty else sy + (ty - sy) * tick / time
        if (lock)
          tickInnerAnimation(stage)
        else
          Some(Tracking(s, a, tick + 1, time, sx, sy, tick + 1 > time))
      } else
        tickInnerAnimation(stage)
    }

    private def tickInnerAnimation(stage : Stage) : Option[Animation] =
      for {
        ap <- a
        newA <- ap.tick(stage)
      } yield Tracking(s, Some(newA), tick, time, sx, sy, lock)

  }

  def move(s : Sprite, tx : Int, ty : Int) : Animation = Move(s, tx, ty, (Math.sqrt((s.x - tx) * (s.x - tx) + (s.y - ty) * (s.y - ty)) / 60).toInt + 2, s.x, s.y, 0)
  private case class Move(s : Sprite, tx : Int, ty : Int, var time : Int, var sx : Int, var sy : Int, tick : Int) extends Animation {

    def tick(stage : Stage) : Option[Animation] = {
      s.highlight = tick < time
      s.x = sx + (tx - sx) * tick / time
      s.y = sy + (ty - sy) * tick / time
      if (tick < time)
        Some(Move(s, tx, ty, time, sx, sy, tick + 1))
      else
        None
    }
  }

  def remove(s : Sprite) : Animation = Remove(s, 0)
  private case class Remove(s : Sprite, tick : Int) extends Animation {

    def tick(stage : Stage) : Option[Animation] = {
      s.flash = true
      if (tick > 5) {
        s.parent match {
          case None    => stage.sprites = stage.sprites.filterNot(_ == s)
          case Some(p) => p.children = p.children - s
        }
        None
      } else
        Some(Remove(s, tick + 1))
    }
  }

  def add(s : Sprite, parent : Option[Sprite]) : Animation = Add(s, parent, 0)
  def add(s : Sprite) : Animation = Add(s, None, 0)
  private case class Add(s : Sprite, parent : Option[Sprite], tick : Int) extends Animation {

    def tick(stage : Stage) : Option[Animation] = {
      s.flash = true
      if (tick == 0) {
        parent match {
          case None    => stage.sprites = stage.sprites :+ s
          case Some(p) => p.children = p.children + s
        }
        s.parent = parent
      }
      if (tick > 4) {
        s.flash = false
        None
      } else
        Some(Add(s, parent, tick + 1))
    }
  }

  def change(s : Sprite, newImg : BufferedImage) : Animation = Change(s, newImg, 0)
  private case class Change(s : Sprite, newImg : BufferedImage, tick : Int) extends Animation {

    def tick(stage : Stage) : Option[Animation] = {
      s.flash = true
      if (tick > 2) {
        s.img = newImg
      }
      if (tick > 4) {
        s.flash = false
        None
      } else
        Some(Change(s, newImg, tick + 1))
    }
  }

  def emancipate(s : Sprite) : Animation = new Emancipate(s)
  private class Emancipate(s : Sprite) extends Animation {

    def tick(stage : Stage) : Option[Animation] =
      s.parent match {
        case Some(p) => {
          s.x = s.globalX
          s.y = s.globalY
          p.children = p.children - s
          s.parent = None
          stage.sprites = stage.sprites :+ s
          None
        }
        case None => None
      }
  }

  def subordinate(s : Sprite, parent : Sprite) : Animation = new Subordinate(s, parent)
  private class Subordinate(s : Sprite, parent : Sprite) extends Animation {

    def tick(stage : Stage) : Option[Animation] = {
      s.parent match {
        case Some(p) => p.children = p.children - s
        case None    => ()
      }
      stage.sprites = stage.sprites.filterNot(_ == s)
      s.parent = Some(parent)
      parent.children = parent.children + s
      s.x = s.x - parent.globalX
      s.y = s.y - parent.globalY
      None
    }
  }

}

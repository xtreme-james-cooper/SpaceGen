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
  var sprites : List[Sprite] = Nil
  private var animations : List[Animation] = Nil

  var camX : Int = 0 //not real
  var camY : Int = 0 //not real

  def animate(a : Animation) : Unit = animations = animations :+ a
  def animate(a : Animation, a2 : Animation) : Unit = animations = animations ++ List(a, a2)

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

    s.tree.children.foreach(ss => draw(g, ss, dx + s.x, dy + s.y))
  }

}

object Stage {

  private val FLASH : Color = new Color(255, 255, 180)

  def delay(wait : Int) : Animation = Delay(wait)

  def tracking(s : Sprite, a : Animation) : Animation = Tracking(s, Some(a), 0, 0, 0, 0, false)
  def track(s : Sprite) : Animation = Tracking(s, None, 0, 0, 0, 0, false)

  def move(s : Sprite, tx : Int, ty : Int) : Animation = Move(s, tx, ty, (Math.sqrt((s.x - tx) * (s.x - tx) + (s.y - ty) * (s.y - ty)) / 60).toInt + 2, s.x, s.y, 0)

  def remove(s : Sprite) : Animation = Remove(s, 0)

  def add(s : Sprite, parent : Option[Sprite]) : Animation = Add(s, parent, 0)
  def add(s : Sprite) : Animation = Add(s, None, 0)

  def change(s : Sprite, newImg : BufferedImage) : Animation = Change(s, newImg, 0)

  def emancipate(s : Sprite) : Animation = new Emancipate(s)

  def subordinate(s : Sprite, parent : Sprite) : Animation = new Subordinate(s, parent)

}
